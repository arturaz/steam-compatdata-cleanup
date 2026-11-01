#!/usr/bin/env -S sh -c 'command -v scala-cli >/dev/null||{ echo "cleanup.scala needs scala-cli: https://scala-cli.virtuslab.org/install">&2;exit 127;};exec scala-cli shebang "$0" "$@"'
//> using scala 3.9.0
//> using options -Wall -Werror -language:strictEquality
//> using dep io.github.arturaz::yantl:0.4.3
//> using dep com.formdev:flatlaf:3.7.2

// Removes Proton prefixes (steamapps/compatdata/<appid>) and shader caches (steamapps/shadercache/<appid>)
// for games that are no longer installed.
//
// Requires scala-cli (https://scala-cli.virtuslab.org/install); running without it prints that link.
//
// Usage:
//   ./cleanup.scala                  # GUI: pick games to delete (dry run in the terminal if there is no display)
//   ./cleanup.scala --theme dark     # GUI look: auto (default, follows the desktop), light, dark, darcula, intellij,
//                                    #   gtk, metal, nimbus
//   ./cleanup.scala --cli            # dry run in the terminal: list what would be deleted
//   ./cleanup.scala --interactive    # terminal: ask per game, delete the ones you confirm
//   ./cleanup.scala --delete         # terminal: delete everything listed, no questions
//   ./cleanup.scala --steam-dir /path/to/Steam
//
// An app is considered installed if any Steam library has steamapps/appmanifest_<appid>.acf.
// Non-Steam shortcuts (from userdata/*/config/shortcuts.vdf) and appid 0 are always kept.
// Game names and Steam Cloud rules come from Steam's local caches; no network access.

import java.awt.{BorderLayout, Color, Component, FlowLayout, Font, GraphicsEnvironment}
import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import java.lang.management.ManagementFactory
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import javax.swing.{AbstractAction, BorderFactory, BoxLayout, JButton, JComboBox, JComponent, JFrame, JLabel, JOptionPane, JPanel,
  JProgressBar, JScrollPane, JSplitPane, JTable, KeyStroke, SwingConstants, SwingUtilities, UIManager, WindowConstants}
import javax.swing.plaf.FontUIResource
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing.text.StyleContext
import com.formdev.flatlaf.{FlatDarculaLaf, FlatDarkLaf, FlatIntelliJLaf, FlatLightLaf}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Failure, Success, Try, Using}
import yantl.*

// ---------------------------------------------------------------------------------------------------------------------
// Semantic types

/** Unvalidated newtype with equality and ordering of the underlying type. */
trait SimpleNewtype[A](using ord: Ordering[A]) extends Newtype.WithoutValidationOf[A]:
  given CanEqual[Type, Type] = CanEqual.derived
  given Ordering[Type] = Ordering.by[Type, A](_.unwrap)(using ord)

/** Steam application id. Non-Steam shortcuts use ids above 2^31. */
object AppId extends SimpleNewtype[Long]:
  /** Proton sometimes creates compatdata/0; it is never an orphan. */
  val Zero: AppId = AppId(0L)
  def parse(s: String): Option[AppId] = s.toLongOption.map(apply)
  def fromUnsignedInt(i: Int): AppId = AppId(Integer.toUnsignedLong(i))
type AppId = AppId.Type

/** An amount of disk space. */
object Bytes extends Newtype.WithoutValidationOf[Long] with StdLibGivens with math.Integral.ExtraImplicits:
  val Zero: Bytes = Bytes(0L)
  extension (b: Bytes)
    def isZero: Boolean = b.unwrap == 0L
    def pretty: String =
      val units = List("B", "KiB", "MiB", "GiB", "TiB")
      var v = b.unwrap.toDouble
      var u = 0
      while v >= 1024 && u < units.length - 1 do
        v /= 1024
        u += 1
      f"$v%.1f ${units(u)}"
type Bytes = Bytes.Type

/** A game's display name, as known by Steam. */
object GameName extends SimpleNewtype[String]
type GameName = GameName.Type

/** Steam's last cloud sync result for a game, e.g. "synchronized". */
object SyncState extends SimpleNewtype[String]:
  val Synchronized: SyncState = SyncState("synchronized")
type SyncState = SyncState.Type

/** A Windows user folder inside a Proton prefix, relative to drive_c/users/steamuser, e.g. "AppData/LocalLow". */
object UserFolder extends SimpleNewtype[String]
type UserFolder = UserFolder.Type

/** A Steam Auto-Cloud root name, e.g. "WinAppDataLocalLow". */
object CloudRoot extends SimpleNewtype[String]
type CloudRoot = CloudRoot.Type

/** Name of a single file or folder directly inside a [[UserFolder]], usually the game's or studio's name. */
object EntryName extends SimpleNewtype[String]
type EntryName = EntryName.Type

/** The Steam installation directory (the one with userdata/, appcache/, ...). */
object SteamDir extends Newtype.WithoutValidationOf[Path]:
  extension (d: SteamDir)
    def userdataDirs: List[Path] = listDir(d.unwrap.resolve("userdata"))
    def appInfoVdf: Path = d.unwrap.resolve("appcache/appinfo.vdf")
    def libraryFoldersVdf: Path = d.unwrap.resolve("steamapps/libraryfolders.vdf")
    def asLibrary: LibraryDir = LibraryDir(d.unwrap)
type SteamDir = SteamDir.Type

/** A Steam library directory (the one containing steamapps/). */
object LibraryDir extends Newtype.WithoutValidationOf[Path]:
  given CanEqual[LibraryDir, LibraryDir] = CanEqual.derived
  extension (d: LibraryDir)
    def steamapps: Path = d.unwrap.resolve("steamapps")
    def leftovers(kind: LeftoverKind): Path = steamapps.resolve(kind.dirName)
type LibraryDir = LibraryDir.Type

/** Kinds of per-app data Steam leaves behind after uninstalling. */
enum LeftoverKind(val dirName: String, val label: String) derives CanEqual:
  /** Proton/Wine prefix: steamapps/compatdata/<appid>. */
  case Prefix extends LeftoverKind("compatdata", "Proton prefix")
  /** Compiled shader cache: steamapps/shadercache/<appid>. */
  case ShaderCache extends LeftoverKind("shadercache", "Shader cache")

case class Leftover(kind: LeftoverKind, path: Path, size: Bytes)

// ---------------------------------------------------------------------------------------------------------------------
// Arguments

/** What to do in the terminal. */
enum CliAction derives CanEqual:
  case DryRun, Delete, Interactive

enum Mode derives CanEqual:
  case Gui
  case Cli(action: CliAction)

/** Swing look and feel. */
enum Theme derives CanEqual:
  /** FlatLaf light or dark, following the desktop's dark mode setting. */
  case Auto
  case Light, Dark, Darcula, IntelliJ
  // The JDK's built-in looks; Gtk is the JDK's "system" look on Linux.
  case Gtk, Metal, Nimbus

object Theme:
  def parse(s: String): Option[Theme] = values.find(_.toString.equalsIgnoreCase(s))
  def names: String = values.map(_.toString.toLowerCase).mkString("|")

case class Args(steamDir: SteamDir, mode: Mode, theme: Theme)

val usage =
  s"Usage: cleanup.scala [--cli | --delete | --interactive|-i] [--theme ${Theme.names}] [--steam-dir <path>]"

def parseArgs(args: List[String], acc: Args): Args = args match
  case Nil                              => acc
  case "--gui" :: rest                  => parseArgs(rest, acc.copy(mode = Mode.Gui))
  case "--cli" :: rest                  => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.DryRun)))
  case "--delete" :: rest               => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.Delete)))
  case ("--interactive" | "-i") :: rest => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.Interactive)))
  case "--steam-dir" :: dir :: rest     => parseArgs(rest, acc.copy(steamDir = SteamDir(Paths.get(dir))))
  case "--theme" :: name :: rest =>
    Theme.parse(name) match
      case Some(theme) => parseArgs(rest, acc.copy(theme = theme))
      case None =>
        System.err.println(s"Unknown theme: $name\n$usage")
        sys.exit(2)
  case ("--help" | "-h") :: _ => println(usage); sys.exit(0)
  case other :: _ =>
    System.err.println(s"Unknown argument: $other\n$usage")
    sys.exit(2)

// ---------------------------------------------------------------------------------------------------------------------
// Filesystem helpers

def listDir(dir: Path): List[Path] =
  if !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS) then Nil
  else Using.resource(Files.list(dir))(_.iterator.asScala.toList.sortBy(_.getFileName.toString))

/** Total size of regular files in a tree, not following symlinks. */
def treeSize(path: Path): Bytes =
  var total = 0L
  Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path]:
      override def visitFile(f: Path, attrs: BasicFileAttributes) =
        if attrs.isRegularFile then total += attrs.size
        FileVisitResult.CONTINUE
      override def visitFileFailed(f: Path, e: java.io.IOException) = FileVisitResult.CONTINUE
  )
  Bytes(total)

/** Recursively deletes a tree, deleting symlinks themselves rather than their targets. */
def deleteTree(dir: Path): Unit =
  val _ = Files.walkFileTree(
    dir,
    new SimpleFileVisitor[Path]:
      override def visitFile(f: Path, attrs: BasicFileAttributes) =
        Files.delete(f)
        FileVisitResult.CONTINUE
      override def postVisitDirectory(d: Path, e: java.io.IOException) =
        if e != null then throw e
        Files.delete(d)
        FileVisitResult.CONTINUE
  )

/** Runs a command and returns its trimmed output, or None if it is missing, fails or takes too long. */
def commandOutput(command: String*): Option[String] =
  Try {
    val process = ProcessBuilder(command*).redirectErrorStream(true).start()
    val output = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
    if process.waitFor(3, TimeUnit.SECONDS) && process.exitValue == 0 then Some(output.trim) else None
  }.toOption.flatten

// ---------------------------------------------------------------------------------------------------------------------
// VDF (Valve KeyValues) parsing.

/** A key in a VDF block. Steam is inconsistent about key case, so lookups ignore it. */
object VdfKey extends SimpleNewtype[String]
type VdfKey = VdfKey.Type

object Vdf:
  final case class Str(value: String)
  final case class Int32(value: Int)
  final case class Int64(value: Long)
  final case class Float32(value: Float)

  /** A value in a block: either a nested block or a leaf of type `L`. Tagged, so matching recovers the exact type. */
  enum Value[+L]:
    case Nested(block: Block[L])
    case Leaf(value: L)

    def asBlock: Option[Block[L]] = this match
      case Nested(block) => Some(block)
      case Leaf(_)       => None

    def asLeaf: Option[L] = this match
      case Nested(_)   => None
      case Leaf(value) => Some(value)

  /** A block of key/value pairs. Covariant, so a block with fewer kinds of leaves can be used where one with more
    * is expected.
    */
  final case class Block[+L](entries: Map[VdfKey, Value[L]])

  /** Text VDF (libraryfolders.vdf, localconfig.vdf, ...) only has strings. */
  type TextLeaf = Str
  /** Binary VDF (appinfo.vdf) has everything text VDF has, plus typed numbers. */
  type BinaryLeaf = TextLeaf | Int32 | Int64 | Float32

  type TextBlock = Block[TextLeaf]
  type BinaryBlock = Block[BinaryLeaf]

  // Every text block is a valid binary block.
  summon[TextBlock <:< BinaryBlock]

extension [L](b: Vdf.Block[L])
  /** Looks up a direct child, matching keys case-insensitively (Steam is inconsistent about case). */
  def get(key: String): Option[Vdf.Value[L]] =
    b.entries.get(VdfKey(key))
      .orElse(b.entries.collectFirst { case (k, v) if k.unwrap.equalsIgnoreCase(key) => v })
  /** Follows a path of keys through nested blocks. */
  def at(path: String*): Option[Vdf.Value[L]] =
    path.foldLeft(Option[Vdf.Value[L]](Vdf.Value.Nested(b)))((value, key) => value.flatMap(_.asBlock).flatMap(_.get(key)))
  def block(path: String*): Option[Vdf.Block[L]] = at(path*).flatMap(_.asBlock)
  def leaf(path: String*): Option[L] = at(path*).flatMap(_.asLeaf)
  /** Child blocks with their keys, ignoring leaf values. */
  def childBlocks: List[(VdfKey, Vdf.Block[L])] =
    b.entries.toList.flatMap((key, value) => value.asBlock.map(key -> _))
  /** Leaf values directly in this block. */
  def leaves: List[L] = b.entries.values.toList.flatMap(_.asLeaf)

// Leaf accessors are defined on BinaryBlock, so they cover every leaf kind exhaustively; TextBlocks get them through
// covariance.
extension (b: Vdf.BinaryBlock)
  def str(path: String*): Option[String] = b.leaf(path*).flatMap(asString)
  /** String values directly in this block. */
  def strings: List[String] = b.leaves.flatMap(asString)
  /** A whole number, whether stored as a binary integer or as text. */
  def long(path: String*): Option[Long] = b.leaf(path*).flatMap {
    case Vdf.Int32(i)   => Some(i.toLong)
    case Vdf.Int64(l)   => Some(l)
    case Vdf.Str(s)     => s.toLongOption
    case Vdf.Float32(_) => None
  }

def asString(leaf: Vdf.BinaryLeaf): Option[String] = leaf match
  case Vdf.Str(s)                                  => Some(s)
  case Vdf.Int32(_) | Vdf.Int64(_) | Vdf.Float32(_) => None

/** Minimal parser for Steam's text VDF format. */
def parseTextVdf(s: String): Vdf.TextBlock =
  var i = 0
  def skipWs(): Unit =
    while i < s.length && (s(i).isWhitespace || s.startsWith("//", i)) do
      if s(i) == '/' then while i < s.length && s(i) != '\n' do i += 1
      else i += 1
  def token(): String =
    if s(i) == '"' then
      val sb = StringBuilder()
      i += 1
      while i < s.length && s(i) != '"' do
        if s(i) == '\\' && i + 1 < s.length then
          sb += (s(i + 1) match { case 'n' => '\n'; case 't' => '\t'; case c => c })
          i += 2
        else
          sb += s(i)
          i += 1
      i += 1
      sb.result()
    else
      val start = i
      while i < s.length && !s(i).isWhitespace && s(i) != '{' && s(i) != '}' do i += 1
      s.substring(start, i)
  def block(): Vdf.TextBlock =
    val b = Map.newBuilder[VdfKey, Vdf.Value[Vdf.TextLeaf]]
    skipWs()
    while i < s.length && s(i) != '}' do
      val k = VdfKey(token())
      skipWs()
      if i < s.length && s(i) == '{' then
        i += 1
        b += k -> Vdf.Value.Nested(block())
        i += 1 // closing brace
      else b += k -> Vdf.Value.Leaf(Vdf.Str(token()))
      skipWs()
    Vdf.Block(b.result())
  block()

def readTextVdf(file: Path): Option[Vdf.TextBlock] =
  if !Files.isRegularFile(file) then None else Try(parseTextVdf(Files.readString(file))).toOption

// ---------------------------------------------------------------------------------------------------------------------
// Steam metadata

/** Library dirs from steamapps/libraryfolders.vdf (always includes the main Steam dir). */
def libraryDirs(steamDir: SteamDir): List[LibraryDir] =
  val fromVdf = readTextVdf(steamDir.libraryFoldersVdf).toList
    .flatMap(_.block("libraryfolders"))
    .flatMap(_.childBlocks)
    .flatMap((_, library) => library.str("path"))
    .map(p => LibraryDir(Paths.get(p)))
  (steamDir.asLibrary :: fromVdf)
    .map(d => LibraryDir(d.unwrap.toAbsolutePath.normalize))
    .distinct
    .filter(d => Files.isDirectory(d.steamapps))

/** App ids that have an appmanifest in any library. */
def installedAppIds(libraries: List[LibraryDir]): Set[AppId] =
  val Manifest = """appmanifest_(\d+)\.acf""".r
  libraries
    .flatMap(lib => listDir(lib.steamapps))
    .flatMap(p => p.getFileName.toString match
      case Manifest(id) => AppId.parse(id)
      case _            => None
    )
    .toSet

/** App ids of non-Steam shortcuts. shortcuts.vdf is binary VDF: 0x02 "appid" 0x00 <int32 LE>. */
def shortcutAppIds(steamDir: SteamDir): Set[AppId] =
  val marker = "\u0002appid\u0000".getBytes(StandardCharsets.ISO_8859_1)
  steamDir.userdataDirs
    .map(_.resolve("config/shortcuts.vdf"))
    .filter(Files.isRegularFile(_))
    .flatMap { file =>
      val bytes = Files.readAllBytes(file)
      // Case-insensitive match: older Steam versions wrote "AppID".
      val lower = bytes.map(b => if b >= 'A' && b <= 'Z' then (b + 32).toByte else b)
      (0 to bytes.length - marker.length - 4)
        .filter(i => java.util.Arrays.equals(lower, i, i + marker.length, marker, 0, marker.length))
        .map(i => AppId.fromUnsignedInt(ByteBuffer.wrap(bytes, i + marker.length, 4).order(ByteOrder.LITTLE_ENDIAN).getInt))
    }
    .toSet

/** Reads the "appinfo" section for the given ids from appcache/appinfo.vdf.
  *
  * Format (versions 0x27, 0x28, 0x29 of magic 0x075644xx):
  *   header: u32 magic, u32 universe, [v29: i64 string table offset]
  *   per app: u32 appid (0 = end), u32 size, then `size` bytes:
  *     u32 infoState, u32 lastUpdated, u64 picsToken, 20b sha1, u32 changeNumber, [v28+: 20b binary sha1], binary KV
  *   binary KV entry: u8 type (0x08 = end of map), key (v29: i32 string table index, else C string), value.
  * Returns an empty map if the file is missing or in an unknown format.
  */
def appInfos(steamDir: SteamDir, wanted: Set[AppId]): Map[AppId, Vdf.BinaryBlock] =
  val file = steamDir.appInfoVdf
  if wanted.isEmpty || !Files.isRegularFile(file) then return Map.empty
  Try {
    val buf = ByteBuffer.wrap(Files.readAllBytes(file)).order(ByteOrder.LITTLE_ENDIAN)
    val magic = buf.getInt
    val version = magic & 0xff
    require((magic >>> 8) == 0x075644 && version >= 0x27 && version <= 0x29, f"unknown appinfo.vdf magic 0x$magic%08x")
    buf.getInt // universe

    def cString(): String =
      val start = buf.position
      while buf.get != 0 do ()
      new String(buf.array, start, buf.position - start - 1, StandardCharsets.UTF_8)

    val stringTable: IndexedSeq[String] =
      if version < 0x29 then IndexedSeq.empty
      else
        val offset = buf.getLong.toInt
        val saved = buf.position
        buf.position(offset)
        val table = IndexedSeq.fill(buf.getInt)(cString())
        buf.position(saved)
        table

    def key(): VdfKey = VdfKey(if version >= 0x29 then stringTable(buf.getInt) else cString())

    /** Parses a block body until 0x08. Pointers, colors and wide strings are skipped (Steam doesn't use them here). */
    def readBlock(): Vdf.BinaryBlock =
      val b = Map.newBuilder[VdfKey, Vdf.Value[Vdf.BinaryLeaf]]
      var tpe = buf.get & 0xff
      while tpe != 0x08 do
        val k = key()
        tpe match
          case 0x00        => b += k -> Vdf.Value.Nested(readBlock())
          case 0x01        => b += k -> Vdf.Value.Leaf(Vdf.Str(cString()))
          case 0x02        => b += k -> Vdf.Value.Leaf(Vdf.Int32(buf.getInt))
          case 0x03        => b += k -> Vdf.Value.Leaf(Vdf.Float32(buf.getFloat))
          case 0x07 | 0x0a => b += k -> Vdf.Value.Leaf(Vdf.Int64(buf.getLong))
          case 0x04 | 0x06 => buf.getInt
          case 0x05        => while buf.getShort != 0 do ()
          case other       => throw IllegalStateException(s"unknown KV type 0x${other.toHexString}")
        tpe = buf.get & 0xff
      Vdf.Block(b.result())

    val infos = Map.newBuilder[AppId, Vdf.BinaryBlock]
    var appId = AppId.fromUnsignedInt(buf.getInt)
    while appId != AppId.Zero do
      val size = buf.getInt
      val next = buf.position + size
      if wanted.contains(appId) then
        buf.position(buf.position + 4 + 4 + 8 + 20 + 4 + (if version >= 0x28 then 20 else 0))
        readBlock().block("appinfo").foreach(info => infos += appId -> info)
      buf.position(next)
      appId = AppId.fromUnsignedInt(buf.getInt)
    infos.result()
  }.recover { case e =>
    System.err.println(s"Warning: could not read Steam app cache ${file.toString}: ${e.getMessage}")
    Map.empty
  }.get

def gameName(info: Vdf.BinaryBlock): Option[GameName] = info.str("common", "name").map(GameName(_))

// ---------------------------------------------------------------------------------------------------------------------
// Save data detection

/** Folders inside a prefix where games commonly keep saves and settings. */
val saveFolders: List[UserFolder] =
  List("AppData/Local", "AppData/LocalLow", "AppData/Roaming", "Documents", "Saved Games").map(UserFolder(_))

/** Entries that Wine/Proton/runtimes create in every prefix and that never hold game saves. */
val notSaves: Set[EntryName] = Set("Microsoft", "Temp", "openvr", "dxvk", "vkd3d", "vkd3d-proton",
  "mesa_shader_cache", "UnrealEngine", "Downloads", "Music", "Pictures", "Templates", "Videos").map(EntryName(_))

enum Cloud derives CanEqual:
  /** An Auto-Cloud rule covers this folder and Steam's last sync for the game succeeded. */
  case Synced
  /** An Auto-Cloud rule covers this folder, but the last sync state is not "synchronized" (or unknown). */
  case Unconfirmed(state: Option[SyncState])
  /** The game uses Steam Cloud, but none of its rules cover this folder. */
  case NotCovered
  /** The game does not use Steam Cloud at all. */
  case NoCloud

  def label: String = this match
    case Synced             => "☁ in Steam Cloud"
    case Unconfirmed(state) => s"☁ Steam Cloud, last sync: ${state.fold("unknown")(_.unwrap)}"
    case NotCovered         => "✗ not synced by Steam Cloud"
    case NoCloud            => "✗ game has no Steam Cloud"

  def risk: Risk = this match
    case Synced         => Risk.Safe
    case Unconfirmed(_) => Risk.Unsure
    case NotCovered     => Risk.Lost
    case NoCloud        => Risk.Lost

/** How likely deleting something loses saves, from least to most. */
enum Risk derives CanEqual:
  case NoSaves, Safe, Unsure, Lost

/** A non-empty, game-created file or folder in one of a prefix's save folders. */
case class Save(folder: UserFolder, entry: EntryName, fullPath: Path, size: Bytes, cloud: Cloud):
  def path: String = s"$folder/$entry"

/** Non-empty, game-created entries in the usual save locations of a prefix. */
def likelySaves(prefix: Path, cloud: (UserFolder, EntryName) => Cloud): List[Save] =
  val user = prefix.resolve("pfx/drive_c/users/steamuser")
  for
    folder <- saveFolders
    path   <- listDir(user.resolve(folder.unwrap))
    entry = EntryName(path.getFileName.toString)
    if !Files.isSymbolicLink(path) && !notSaves.contains(entry)
    size = treeSize(path)
    if !size.isZero
  yield Save(folder, entry, path, size, cloud(folder, entry))

// ---------------------------------------------------------------------------------------------------------------------
// Steam Cloud

/** Steam Auto-Cloud roots → the matching folder inside a Proton prefix. */
val cloudRoots: Map[CloudRoot, UserFolder] = Map(
  "WinAppDataLocal"    -> "AppData/Local",
  "WinAppDataLocalLow" -> "AppData/LocalLow",
  "WinAppDataRoaming"  -> "AppData/Roaming",
  "WinMyDocuments"     -> "Documents",
  "WinSavedGames"      -> "Saved Games"
).map((root, folder) => CloudRoot(root) -> UserFolder(folder))

/** Last Steam Cloud sync state per app, across all local Steam users (userdata/<user>/config/localconfig.vdf). */
def cloudSyncStates(steamDir: SteamDir): Map[AppId, List[SyncState]] =
  steamDir.userdataDirs
    .flatMap(user => readTextVdf(user.resolve("config/localconfig.vdf")))
    .flatMap(_.block("UserLocalConfigStore", "Software", "Valve", "Steam", "apps").toList)
    .flatMap(_.childBlocks)
    .flatMap { (id, app) =>
      for
        appId <- AppId.parse(id.unwrap)
        state <- app.str("cloud", "last_sync_state")
      yield appId -> SyncState(state)
    }
    .groupMap(_._1)(_._2)

/** Decides whether a prefix entry (`folder`/`entry`) of an app is backed up by Steam Cloud. */
def cloudStatus(info: Option[Vdf.BinaryBlock], syncStates: Map[AppId, List[SyncState]], appId: AppId)(
    folder: UserFolder,
    entry: EntryName
): Cloud =
  info.flatMap(_.block("ufs")) match
    case None => Cloud.NoCloud
    case Some(ufs) =>
      val rules = ufs.block("savefiles").toList.flatMap(_.childBlocks).map(_._2)
      val covered = rules.exists { rule =>
        val forWindows = rule.block("platforms").forall(_.strings.exists(p => p == "Windows" || p == "all"))
        val ruleFolder = rule.str("root").map(CloudRoot(_)).flatMap(cloudRoots.get)
        val ruleEntry = rule.str("path").getOrElse("").split("[/\\\\]").headOption.filter(_.nonEmpty)
        forWindows && ruleFolder.contains(folder) && ruleEntry.forall(_.equalsIgnoreCase(entry.unwrap))
      }
      if !covered then Cloud.NotCovered
      else
        // Demos often sync into the full game's cloud storage.
        val cloudId = ufs.long("appidredirect").map(AppId(_)).getOrElse(appId)
        val states = syncStates.getOrElse(cloudId, Nil) ++ syncStates.getOrElse(appId, Nil)
        if states.contains(SyncState.Synchronized) then Cloud.Synced else Cloud.Unconfirmed(states.headOption)

// ---------------------------------------------------------------------------------------------------------------------
// Scanning

/** The three groups games are shown in, in display order. */
enum SaveGroup derives CanEqual:
  /** Has saves that Steam Cloud does not verifiably hold: deleting may lose them for good. */
  case NotBackedUp
  /** All saves are in Steam Cloud. */
  case BackedUp
  case NoSaves

case class Orphan(appId: AppId, name: Option[GameName], leftovers: List[Leftover], saves: List[Save]):
  def size: Bytes = leftovers.map(_.size).sum
  def backedUp: Boolean = saves.nonEmpty && saves.forall(_.cloud == Cloud.Synced)
  def group: SaveGroup =
    if saves.isEmpty then SaveGroup.NoSaves else if backedUp then SaveGroup.BackedUp else SaveGroup.NotBackedUp
  /** The riskiest of the game's saves. */
  def risk: Risk = saves.map(_.cloud.risk).maxByOption(_.ordinal).getOrElse(Risk.NoSaves)
  def displayName: String = name.fold(s"Unknown app $appId")(_.unwrap)
  private def has(kind: LeftoverKind) = leftovers.exists(_.kind == kind)
  /** Extra note when leftovers aren't just a Proton prefix. */
  def kindNote: Option[String] = (has(LeftoverKind.Prefix), has(LeftoverKind.ShaderCache)) match
    case (true, true)  => Some("prefix + shader cache")
    case (false, true) => Some("shader cache only")
    case _             => None

def totalSize(os: Iterable[Orphan]): Bytes = os.map(_.size).sum

case class Scan(libraries: List[LibraryDir], installedCount: Int, shortcutCount: Int, orphans: List[Orphan])

/** Finds leftovers of uninstalled games, ordered by group, then largest first. Left is a reason to stop. */
def scan(steamDir: SteamDir): Either[String, Scan] =
  val libraries = libraryDirs(steamDir)
  if libraries.isEmpty then return Left(s"No Steam libraries found under ${steamDir.unwrap.toString}")
  val installed = installedAppIds(libraries)
  if installed.isEmpty then return Left("No appmanifests found — refusing to continue (would delete everything).")
  val shortcuts = shortcutAppIds(steamDir)
  val keep = installed ++ shortcuts + AppId.Zero

  val orphanDirs: Map[AppId, List[(LeftoverKind, Path)]] =
    (for
      lib   <- libraries
      kind  <- LeftoverKind.values.toList
      dir   <- listDir(lib.leftovers(kind))
      if Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)
      appId <- AppId.parse(dir.getFileName.toString)
      if !keep.contains(appId)
    yield appId -> (kind, dir)).groupMap(_._1)(_._2)

  val infos = appInfos(steamDir, orphanDirs.keySet)
  val syncStates = if orphanDirs.isEmpty then Map.empty else cloudSyncStates(steamDir)
  val orphans = orphanDirs.toList
    .map { (appId, dirs) =>
      val info = infos.get(appId)
      val saves = dirs.collect { case (LeftoverKind.Prefix, dir) => dir }
        .flatMap(likelySaves(_, cloudStatus(info, syncStates, appId)))
      val leftovers = dirs.map((kind, dir) => Leftover(kind, dir, treeSize(dir)))
      Orphan(appId, info.flatMap(gameName), leftovers, saves)
    }
    .sortBy(o => (o.group.ordinal, -o.size.unwrap, o.appId))
  Right(Scan(libraries, installed.size, shortcuts.size, orphans))

/** Deletes a game's leftovers, returning the error for each one that could not be deleted. */
def deleteLeftovers(o: Orphan): List[(Leftover, Option[Exception])] =
  o.leftovers.map { l =>
    l -> (try
      deleteTree(l.path)
      None
    catch case e: Exception => Some(e))
  }

def plural(n: Int, word: String): String = s"$n $word${if n == 1 then "" else "s"}"
def gameCount(n: Int): String = plural(n, "game")

// ---------------------------------------------------------------------------------------------------------------------
// Terminal output

/** ANSI styling, disabled when stdout is not a terminal or NO_COLOR is set. */
object Style:
  val enabled: Boolean =
    System.console() != null && sys.env.get("NO_COLOR").forall(_.isEmpty) && !sys.env.get("TERM").contains("dumb")
  private def wrap(code: String)(s: String) = if enabled then s"\u001b[${code}m$s\u001b[0m" else s
  val bold   = wrap("1")
  val dim    = wrap("2")
  val red    = wrap("31")
  val green  = wrap("32")
  val yellow = wrap("33")
  val cyan   = wrap("36")

import Style.*

/** Terminal column width of a string: ignores ANSI escapes, counts CJK / full-width characters as 2. */
def displayWidth(s: String): Int =
  s.replaceAll("\u001b\\[[0-9;]*m", "").codePoints.toArray.map { c =>
    val wide =
      (c >= 0x1100 && c <= 0x115f) || (c >= 0x2e80 && c <= 0xa4cf) || (c >= 0xac00 && c <= 0xd7a3) ||
        (c >= 0xf900 && c <= 0xfaff) || (c >= 0xfe30 && c <= 0xfe4f) || (c >= 0xff00 && c <= 0xff60) ||
        (c >= 0xffe0 && c <= 0xffe6) || (c >= 0x1f300 && c <= 0x1faff) || c >= 0x20000
    if wide then 2 else 1
  }.sum
def padLeft(s: String, w: Int): String = " " * (w - displayWidth(s)).max(0) + s
def padRight(s: String, w: Int): String = s + " " * (w - displayWidth(s)).max(0)

val sizeW = 10

/** Prints one section of games with a heading; `nameW` keeps size columns aligned across sections. */
def printSection(heading: String, games: List[Orphan], nameW: Int): Unit =
  if games.isEmpty then return
  println(heading)
  games.foreach { o =>
    val size = if o.size.isZero then dim(o.size.pretty) else o.size.pretty
    val name = if o.name.isEmpty then dim(o.displayName) else bold(o.displayName)
    val note = o.kindNote.map(n => s"  ${dim(n)}").getOrElse("")
    println(s"    ${padRight(name, nameW)}  ${padLeft(size, sizeW)}  ${dim(s"#${o.appId}")}$note")
    o.saves.zipWithIndex.foreach { (save, i) =>
      val branch = if i == o.saves.length - 1 then "└─" else "├─"
      val color = save.cloud.risk match
        case Risk.Safe                 => green
        case Risk.Unsure               => yellow
        case Risk.Lost | Risk.NoSaves  => red
      println(s"      ${padRight(s"$branch ${save.path}", nameW - 2)}  ${padLeft(save.size.pretty, sizeW)}  ${color(save.cloud.label)}")
    }
  }
  println()

def delete(o: Orphan): Bytes =
  deleteLeftovers(o).map {
    case (l, None) =>
      println(s"    ${green("✓")} deleted ${dim(l.path.toString)}")
      l.size
    case (l, Some(e)) =>
      System.err.println(s"    ${red("✗")} failed to delete ${l.path.toString}: ${e.getMessage}")
      Bytes.Zero
  }.sum

enum Answer derives CanEqual:
  case Yes, No, All, Quit

def ask(o: Orphan): Answer =
  val warning =
    if o.saves.isEmpty then ""
    else if o.backedUp then green(" — saves are in Steam Cloud")
    else red(" — saves NOT backed up!")
  print(s"Delete ${bold(o.displayName)} (${o.size.pretty})$warning ${dim("[y/N/a(ll)/q(uit)]")} ")
  Console.flush()
  Option(scala.io.StdIn.readLine()).map(_.trim.toLowerCase) match
    case None               => Answer.Quit // EOF
    case Some("y" | "yes")  => Answer.Yes
    case Some("a" | "all")  => Answer.All
    case Some("q" | "quit") => Answer.Quit
    case Some(_)            => Answer.No

def runCli(steamDir: SteamDir, action: CliAction): Unit =
  val result = scan(steamDir) match
    case Left(error) =>
      System.err.println(error)
      sys.exit(1)
    case Right(result) => result
  println(dim(s"Steam libraries: ${result.libraries.mkString(", ")}"))
  println(dim(s"Keeping ${plural(result.installedCount, "installed app")} and " +
    s"${plural(result.shortcutCount, "non-Steam shortcut")}."))

  val orphans = result.orphans // display order = interactive order
  if orphans.isEmpty then
    println(green("\nNothing to clean up."))
    return

  def inGroup(g: SaveGroup) = orphans.filter(_.group == g)
  val notBackedUp = inGroup(SaveGroup.NotBackedUp)
  val backedUp = inGroup(SaveGroup.BackedUp)
  val noSaves = inGroup(SaveGroup.NoSaves)
  def summary(os: List[Orphan]) = s"${gameCount(os.size)}, ${totalSize(os).pretty}"

  val nameW = (orphans.map(o => displayWidth(o.displayName)) ++
    orphans.flatMap(_.saves.map(s => displayWidth(s.path) + 5))).max.min(60)

  println()
  println(bold(s"Uninstalled games are using ${totalSize(orphans).pretty}") + dim(s" (${gameCount(orphans.size)})"))
  println()
  printSection(
    red(bold("✗ Saves not backed up")) + dim(s" (${summary(notBackedUp)}) — deleting loses them for good"),
    notBackedUp,
    nameW
  )
  printSection(
    green(bold("☁ Saves backed up by Steam Cloud")) + dim(s" (${summary(backedUp)}) — restored on reinstall"),
    backedUp,
    nameW
  )
  printSection(green(bold("✓ No save data found")) + dim(s" (${summary(noSaves)})"), noSaves, nameW)

  action match
    case CliAction.DryRun =>
      println(s"${cyan("Dry run")} — nothing deleted. Use ${bold("--interactive")} to choose per game, " +
        s"or ${bold("--delete")} to remove all.")
    case CliAction.Delete =>
      val freed = orphans.map(delete).sum
      println(green(s"\nFreed ${freed.pretty}."))
    case CliAction.Interactive =>
      var freed = Bytes.Zero
      var all = false
      var quit = false
      val it = orphans.iterator
      while it.hasNext && !quit do
        val o = it.next()
        val answer = if all then Answer.Yes else ask(o)
        answer match
          case Answer.Yes  => freed += delete(o)
          case Answer.All  => all = true; freed += delete(o)
          case Answer.No   => ()
          case Answer.Quit => quit = true
      println(green(s"\nFreed ${freed.pretty}."))

// ---------------------------------------------------------------------------------------------------------------------
// Desktop integration

/** Whether the desktop asks for dark mode. Tries, in order: the freedesktop appearance portal (GNOME, KDE, and
  * Hyprland/sway with xdg-desktop-portal-gtk), GNOME settings, KDE's colour scheme, the GTK_THEME variable.
  */
object DesktopDarkMode:
  /** Portal values: 1 = prefer dark, 2 = prefer light, 0 = no preference. */
  private def portal: Option[Boolean] =
    commandOutput("gdbus", "call", "--session", "--timeout", "2", "--dest", "org.freedesktop.portal.Desktop",
      "--object-path", "/org/freedesktop/portal/desktop", "--method", "org.freedesktop.portal.Settings.ReadOne",
      "org.freedesktop.appearance", "color-scheme")
      .flatMap("""uint32 (\d)""".r.findFirstMatchIn(_))
      .map(_.group(1))
      .collect { case "1" => true; case "2" => false }

  private def gsettings(key: String): Option[String] =
    commandOutput("gsettings", "get", "org.gnome.desktop.interface", key)

  private def gnome: Option[Boolean] =
    gsettings("color-scheme")
      .collect { case s if s.contains("prefer-dark") => true; case s if s.contains("prefer-light") => false }
      .orElse(gsettings("gtk-theme").filter(_.toLowerCase.contains("dark")).map(_ => true))

  /** KDE writes its window background colour to kdeglobals. */
  private def kde: Option[Boolean] =
    val isKde = sys.env.get("XDG_CURRENT_DESKTOP").exists(_.split(':').contains("KDE"))
    val config = sys.env.get("XDG_CONFIG_HOME").map(Paths.get(_)).getOrElse(Paths.get(sys.props("user.home"), ".config"))
    if !isKde then None
    else
      Try(Files.readAllLines(config.resolve("kdeglobals")).asScala.toList).toOption.flatMap { lines =>
        lines
          .dropWhile(_.trim != "[Colors:Window]").drop(1).takeWhile(!_.startsWith("["))
          .collectFirst { case l if l.startsWith("BackgroundNormal=") => l.stripPrefix("BackgroundNormal=") }
          .map(_.split(',').toList.flatMap(_.trim.toIntOption))
          .collect { case List(r, g, b) => luminance(Color(r, g, b)) < 0.5 }
      }

  private def gtkThemeVariable: Option[Boolean] =
    sys.env.get("GTK_THEME").filter(_.toLowerCase.contains("dark")).map(_ => true)

  def prefersDark: Boolean = portal.orElse(gnome).orElse(kde).orElse(gtkThemeVariable).getOrElse(false)

/** Relative luminance from 0 (black) to 1 (white). */
def luminance(c: Color): Double = (0.2126 * c.getRed + 0.7152 * c.getGreen + 0.0722 * c.getBlue) / 255

/** Java's X11 toolkit shows blank grey windows under tiling compositors (Hyprland, sway, niri, ...) unless it is told
  * that the window manager does not reparent. That is read from an environment variable when the JVM starts.
  */
def needsNonReparentingRelaunch: Boolean =
  val tilingSockets = List("HYPRLAND_INSTANCE_SIGNATURE", "SWAYSOCK", "NIRI_SOCKET")
  val tilingDesktops = Set("hyprland", "sway", "niri", "river", "wayfire", "labwc", "dwl", "qtile", "i3", "bspwm")
  val desktops = sys.env.get("XDG_CURRENT_DESKTOP").toList.flatMap(_.toLowerCase.split(':'))
  !sys.env.contains("_JAVA_AWT_WM_NONREPARENTING") &&
  (tilingSockets.exists(sys.env.contains) || desktops.exists(tilingDesktops.contains))

/** Starts this program again in a new JVM with _JAVA_AWT_WM_NONREPARENTING=1 and exits with its exit code.
  * Returns false (doing nothing) if the current command line can't be reconstructed.
  */
def relaunchNonReparenting(rawArgs: Seq[String]): Boolean =
  val command =
    for
      java      <- ProcessHandle.current.info.command.toScala
      mainClass <- sys.props.get("sun.java.command").flatMap(_.split(' ').headOption)
      classPath <- sys.props.get("java.class.path")
      if !mainClass.endsWith(".jar")
    yield (java :: ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList) ++
      List("-cp", classPath, mainClass) ++ rawArgs
  command.exists { command =>
    val builder = ProcessBuilder(command.asJava).inheritIO()
    val _ = builder.environment.put("_JAVA_AWT_WM_NONREPARENTING", "1")
    sys.exit(builder.start().waitFor())
  }

/** Shows a file or folder in the desktop's file manager (a file is shown by opening its folder). */
def openInFileManager(path: Path): Try[Unit] =
  val target = if Files.isDirectory(path) then path else path.getParent
  Try { val _ = ProcessBuilder("xdg-open", target.toString).start() }

// ---------------------------------------------------------------------------------------------------------------------
// GUI

object Gui:
  def start(steamDir: SteamDir, theme: Theme): Unit =
    // Runs commands, so do it before touching the event thread.
    val dark = theme == Theme.Auto && DesktopDarkMode.prefersDark
    SwingUtilities.invokeLater { () =>
      applyTheme(theme, autoDark = dark)
      MainWindow(steamDir, theme).show()
    }

  /** Sets the look and feel. Windows that already exist need SwingUtilities.updateComponentTreeUI afterwards. */
  def applyTheme(theme: Theme, autoDark: Boolean): Unit =
    def laf(className: String) = Try(UIManager.setLookAndFeel(className)).isSuccess
    val _ = theme match
      case Theme.Auto     => if autoDark then FlatDarkLaf.setup() else FlatLightLaf.setup()
      case Theme.Light    => FlatLightLaf.setup()
      case Theme.Dark     => FlatDarkLaf.setup()
      case Theme.Darcula  => FlatDarculaLaf.setup()
      case Theme.IntelliJ => FlatIntelliJLaf.setup()
      case Theme.Gtk      => laf("com.sun.java.swing.plaf.gtk.GTKLookAndFeel")
      case Theme.Metal    => laf("javax.swing.plaf.metal.MetalLookAndFeel")
      case Theme.Nimbus   => laf("javax.swing.plaf.nimbus.NimbusLookAndFeel")
    // FlatLaf uses the desktop font as is, which has no glyphs for e.g. CJK game names. A composite font falls back to
    // other installed fonts for those.
    Option(UIManager.getFont("defaultFont")).foreach { font =>
      val composite = StyleContext.getDefaultStyleContext.getFont(font.getFamily, font.getStyle, font.getSize)
      val _ = UIManager.put("defaultFont", FontUIResource(composite))
    }

/** Runs `work` off the event thread, then `onDone` on it. */
def inBackground[A](work: => A)(onDone: Try[A] => Unit): Unit =
  Thread(() => {
    val result = Try(work)
    SwingUtilities.invokeLater(() => onDone(result))
  }).start()

def escapeHtml(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
def hex(c: Color): String = f"#${c.getRGB & 0xffffff}%06x"

/** A game in the list. `orphan` loses the leftovers that get deleted, keeping the ones that failed. */
final class GameRow(var orphan: Orphan, var checked: Boolean)

/** A leftover folder or a save shown in the details table. */
final case class Detail(kind: String, location: String, path: Path, size: Bytes, cloud: Option[Cloud])

/** Text colours that suit the current look and feel's background. */
final case class Palette(red: Color, green: Color, yellow: Color, dimmed: Color)

object Palette:
  def current(): Palette =
    val dark = luminance(UIManager.getColor("Panel.background")) < 0.5
    Palette(
      red = if dark then Color(0xff6b68) else Color(0xc62828),
      green = if dark then Color(0x6cc470) else Color(0x2e7d32),
      yellow = if dark then Color(0xe5c07b) else Color(0x9a6700),
      dimmed = Option(UIManager.getColor("Label.disabledForeground")).getOrElse(Color.GRAY)
    )

final class MainWindow(steamDir: SteamDir, initialTheme: Theme):
  private var palette = Palette.current()

  private def riskColor(risk: Risk): Color = risk match
    case Risk.NoSaves => palette.dimmed
    case Risk.Safe    => palette.green
    case Risk.Unsure  => palette.yellow
    case Risk.Lost    => palette.red

  private def riskSummary(risk: Risk): String = risk match
    case Risk.NoSaves => "no saves found"
    case Risk.Safe    => "☁ saves in Steam Cloud"
    case Risk.Unsure  => "☁ cloud sync unconfirmed"
    case Risk.Lost    => "✗ saves not backed up"

  private val frame = JFrame("Steam leftovers cleanup")
  private val rows = ArrayBuffer.empty[GameRow]
  private var details = Vector.empty[Detail]
  private var scanned: Option[Scan] = None
  private var freed = Bytes.Zero
  private var busy = false

  private object gameModel extends AbstractTableModel:
    private val columns = Vector("", "Game", "Size", "Saves", "App ID")
    def getRowCount: Int = rows.size
    def getColumnCount: Int = columns.size
    override def getColumnName(column: Int): String = columns(column)
    override def getColumnClass(column: Int): Class[?] = column match
      case 0 => classOf[java.lang.Boolean]
      case 1 => classOf[String]
      case 3 => classOf[java.lang.Integer]
      case _ => classOf[java.lang.Long]
    override def isCellEditable(row: Int, column: Int): Boolean = column == 0 && !busy
    def getValueAt(row: Int, column: Int): Object =
      val o = rows(row).orphan
      column match
        case 0 => java.lang.Boolean.valueOf(rows(row).checked)
        case 1 => o.displayName
        case 2 => java.lang.Long.valueOf(o.size.unwrap)
        case 3 => java.lang.Integer.valueOf(o.risk.ordinal)
        case _ => java.lang.Long.valueOf(o.appId.unwrap)
    override def setValueAt(value: Object, row: Int, column: Int): Unit = value match
      case checked: java.lang.Boolean if column == 0 =>
        rows(row).checked = checked
        fireTableCellUpdated(row, column)
        refresh()
      case _ => ()

  private object detailModel extends AbstractTableModel:
    private val columns = Vector("Type", "Location", "Size", "Steam Cloud")
    def getRowCount: Int = details.size
    def getColumnCount: Int = columns.size
    override def getColumnName(column: Int): String = columns(column)
    def getValueAt(row: Int, column: Int): Object =
      val d = details(row)
      column match
        case 0 => d.kind
        case 1 => d.location
        case 2 => d.size.pretty
        case _ => d.cloud.fold("")(_.label)

  // Lazy: the models refer to this window, so they can only be handed out once it is constructed.
  private lazy val gameTable = JTable(gameModel)
  private lazy val detailTable = JTable(detailModel)
  private val title = JLabel("Scanning Steam libraries…")
  private val subtitle = JLabel(" ")
  private val status = JLabel(" ")
  private val progress = JProgressBar()
  private val selectSafeButton = JButton("Select safe")
  private val selectAllButton = JButton("Select all")
  private val selectNoneButton = JButton("Select none")
  private val openButton = JButton("Open folder")
  private val deleteButton = JButton("Delete selected…")
  private val themeBox = JComboBox[Theme](Theme.values)
  private val detailTitle = JLabel("Details of the selected game")
  private val dimLabels = ArrayBuffer[JLabel](subtitle)

  /** Fonts and colours set by hand; redone after a theme switch, since they don't come from the look and feel. */
  private def styleLabels(): Unit =
    val base = UIManager.getFont("Label.font")
    title.setFont(base.deriveFont(Font.BOLD, base.getSize2D * 1.5f))
    detailTitle.setFont(base.deriveFont(Font.BOLD))
    dimLabels.foreach(_.setForeground(palette.dimmed))

  private def switchTheme(): Unit =
    themeBox.getItemAt(themeBox.getSelectedIndex) match
      case null => ()
      case theme =>
        // Auto runs commands to read the desktop setting, so do that off the event thread.
        inBackground(theme == Theme.Auto && DesktopDarkMode.prefersDark) { dark =>
          Gui.applyTheme(theme, autoDark = dark.getOrElse(false))
          palette = Palette.current()
          SwingUtilities.updateComponentTreeUI(frame)
          styleLabels()
          frame.repaint()
        }

  /** A cell renderer that lets `style` adjust the label for the row (as a model index). */
  private def renderer(align: Int)(style: (JLabel, Int, Boolean) => Unit): DefaultTableCellRenderer =
    new DefaultTableCellRenderer:
      setHorizontalAlignment(align)
      override def getTableCellRendererComponent(
          table: JTable,
          value: Object,
          selected: Boolean,
          focused: Boolean,
          row: Int,
          column: Int
      ): Component =
        val _ = super.getTableCellRendererComponent(table, value, selected, focused, row, column)
        setToolTipText(null)
        style(this, table.convertRowIndexToModel(row), selected)
        this

  private def setupGameTable(): Unit =
    gameTable.setAutoCreateRowSorter(true)
    gameTable.setFillsViewportHeight(true)
    gameTable.setShowGrid(false)
    gameTable.setRowHeight((gameTable.getRowHeight * 1.4).toInt)
    val columns = gameTable.getColumnModel
    columns.getColumn(0).setMaxWidth(36)
    List(1 -> 380, 2 -> 90, 3 -> 220, 4 -> 90).foreach((i, w) => columns.getColumn(i).setPreferredWidth(w))

    columns.getColumn(1).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(o.kindNote.fold(o.displayName) { note =>
        s"<html>${escapeHtml(o.displayName)}&nbsp;&nbsp;<span style='color:${hex(palette.dimmed)}'>$note</span></html>"
      })
      if o.name.isEmpty && !selected then label.setForeground(palette.dimmed)
    })
    columns.getColumn(2).setCellRenderer(renderer(SwingConstants.TRAILING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(o.size.pretty)
      if o.size.isZero && !selected then label.setForeground(palette.dimmed)
    })
    columns.getColumn(3).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(riskSummary(o.risk))
      if !selected then label.setForeground(riskColor(o.risk))
      if o.saves.nonEmpty then
        label.setToolTipText(o.saves.map(s => s"${s.path} (${s.size.pretty}): ${s.cloud.label}").mkString("<html>", "<br>", "</html>"))
    })
    columns.getColumn(4).setCellRenderer(renderer(SwingConstants.TRAILING) { (label, row, selected) =>
      label.setText(s"#${rows(row).orphan.appId}")
      if !selected then label.setForeground(palette.dimmed)
    })

    gameTable.getSelectionModel.addListSelectionListener(e => if !e.getValueIsAdjusting then showDetails())
    gameTable.addMouseListener(new MouseAdapter:
      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 && gameTable.columnAtPoint(e.getPoint) != 0 then openSelected())
    // Space toggles the checkboxes of all selected rows.
    gameTable.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("SPACE"), "toggle-checked")
    gameTable.getActionMap.put("toggle-checked", new AbstractAction:
      override def actionPerformed(e: ActionEvent): Unit = if !busy then toggleSelected())

  private def setupDetailTable(): Unit =
    detailTable.setFillsViewportHeight(true)
    detailTable.setShowGrid(false)
    val columns = detailTable.getColumnModel
    List(0 -> 110, 1 -> 560, 2 -> 90, 3 -> 260).foreach((i, w) => columns.getColumn(i).setPreferredWidth(w))
    columns.getColumn(1).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, _) =>
      label.setToolTipText(details(row).path.toString)
    })
    columns.getColumn(2).setCellRenderer(renderer(SwingConstants.TRAILING)((_, _, _) => ()))
    columns.getColumn(3).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      details(row).cloud.foreach(c => if !selected then label.setForeground(riskColor(c.risk)))
    })
    detailTable.addMouseListener(new MouseAdapter:
      override def mouseClicked(e: MouseEvent): Unit =
        val row = detailTable.rowAtPoint(e.getPoint)
        if e.getClickCount == 2 && row >= 0 then open(details(row).path))

  private def layout(): Unit =
    val hints = List(
      "Games whose saves aren't backed up start unchecked. Space toggles the selected rows; double-click opens a folder.",
      "Save detection is a heuristic: games that save elsewhere aren't covered, so check the details before deleting."
    ).map { text =>
      val hint = JLabel(text)
      dimLabels += hint
      hint
    }
    val header = JPanel()
    header.setLayout(BoxLayout(header, BoxLayout.Y_AXIS))
    header.setBorder(BorderFactory.createEmptyBorder(12, 16, 10, 16))
    (title :: subtitle :: hints).foreach(c => { val _ = header.add(c) })

    val detailPanel = JPanel(BorderLayout(0, 4))
    detailPanel.add(detailTitle, BorderLayout.NORTH)
    detailPanel.add(JScrollPane(detailTable), BorderLayout.CENTER)
    detailPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0))

    val split = JSplitPane(JSplitPane.VERTICAL_SPLIT, JScrollPane(gameTable), detailPanel)
    split.setResizeWeight(0.7)
    split.setBorder(BorderFactory.createEmptyBorder(0, 16, 0, 16))

    val buttons = JPanel(FlowLayout(FlowLayout.TRAILING, 6, 0))
    List(selectSafeButton, selectAllButton, selectNoneButton, openButton, deleteButton)
      .foreach(b => { val _ = buttons.add(b) })
    val left = JPanel(FlowLayout(FlowLayout.LEADING, 8, 0))
    left.add(JLabel("Theme:"))
    left.add(themeBox)
    left.add(status)
    left.add(progress)
    val footer = JPanel(BorderLayout())
    footer.setBorder(BorderFactory.createEmptyBorder(10, 16, 12, 16))
    footer.add(left, BorderLayout.CENTER)
    footer.add(buttons, BorderLayout.EAST)

    selectSafeButton.setToolTipText("Select games whose saves are in Steam Cloud, or that have no saves")
    selectSafeButton.addActionListener(_ => setChecked(_.group != SaveGroup.NotBackedUp))
    selectAllButton.addActionListener(_ => setChecked(_ => true))
    selectNoneButton.addActionListener(_ => setChecked(_ => false))
    openButton.addActionListener(_ => openSelected())
    deleteButton.addActionListener(_ => deleteChecked())
    themeBox.setSelectedItem(initialTheme)
    themeBox.setToolTipText("Auto follows the desktop's dark mode setting")
    themeBox.addActionListener(_ => switchTheme())
    styleLabels()

    val content = frame.getContentPane
    content.add(header, BorderLayout.NORTH)
    content.add(split, BorderLayout.CENTER)
    content.add(footer, BorderLayout.SOUTH)

  def show(): Unit =
    setupGameTable()
    setupDetailTable()
    layout()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setSize(1150, 760)
    frame.setLocationRelativeTo(null)
    setBusy(true)
    progress.setIndeterminate(true)
    refresh()
    frame.setVisible(true)
    inBackground(scan(steamDir)) {
      case Success(Right(result)) => loaded(result)
      case Success(Left(error))   => fatal(error)
      case Failure(e)             => fatal(s"Scanning failed: ${e.toString}")
    }

  private def fatal(message: String): Unit =
    JOptionPane.showMessageDialog(frame, message, frame.getTitle, JOptionPane.ERROR_MESSAGE)
    sys.exit(1)

  private def loaded(result: Scan): Unit =
    scanned = Some(result)
    rows ++= result.orphans.map(o => GameRow(o, checked = o.group != SaveGroup.NotBackedUp))
    gameModel.fireTableDataChanged()
    setBusy(false)
    if rows.nonEmpty then gameTable.setRowSelectionInterval(0, 0)

  private def setBusy(value: Boolean): Unit =
    busy = value
    progress.setVisible(value)
    gameTable.setEnabled(!value)
    refresh()

  /** Updates the header, footer and buttons from the current rows. */
  private def refresh(): Unit =
    scanned.foreach { s =>
      val total = totalSize(rows.map(_.orphan))
      title.setText(if rows.isEmpty then "Nothing to clean up" else s"Uninstalled games are using ${total.pretty}")
      subtitle.setText(s"${gameCount(rows.size)} · keeping ${plural(s.installedCount, "installed app")} and " +
        s"${plural(s.shortcutCount, "non-Steam shortcut")} · ${s.libraries.mkString(", ")}")
    }
    val checked = rows.filter(_.checked)
    if !busy then
      val freedText = if freed.isZero then "" else s"Freed ${freed.pretty} · "
      status.setText(s"$freedText${checked.size} of ${rows.size} selected · ${totalSize(checked.map(_.orphan)).pretty}")
    List(selectSafeButton, selectAllButton, selectNoneButton).foreach(_.setEnabled(!busy && rows.nonEmpty))
    openButton.setEnabled(!busy && selectedRows.nonEmpty)
    deleteButton.setEnabled(!busy && checked.nonEmpty)

  private def selectedRows: List[GameRow] =
    gameTable.getSelectedRows.toList.map(gameTable.convertRowIndexToModel).map(rows(_))

  private def showDetails(): Unit =
    details = selectedRows.headOption.fold(Vector.empty) { row =>
      val o = row.orphan
      (o.leftovers.map(l => Detail(l.kind.label, l.path.toString, l.path, l.size, None)) ++
        o.saves.map(s => Detail("Save", s.path, s.fullPath, s.size, Some(s.cloud)))).toVector
    }
    detailModel.fireTableDataChanged()
    refresh()

  private def setChecked(f: Orphan => Boolean): Unit =
    rows.foreach(row => row.checked = f(row.orphan))
    gameModel.fireTableRowsUpdated(0, rows.size - 1)
    refresh()

  private def toggleSelected(): Unit =
    val selected = selectedRows
    val value = !selected.forall(_.checked)
    selected.foreach(_.checked = value)
    gameModel.fireTableRowsUpdated(0, rows.size - 1)
    refresh()

  private def open(path: Path): Unit =
    openInFileManager(path).failed.foreach { e =>
      JOptionPane.showMessageDialog(frame, s"Could not open ${path.toString}: ${e.getMessage}", frame.getTitle,
        JOptionPane.ERROR_MESSAGE)
    }

  private def openSelected(): Unit = selectedRows.headOption.flatMap(_.orphan.leftovers.headOption).foreach(l => open(l.path))

  private def deleteChecked(): Unit =
    val chosen = rows.filter(_.checked).toList
    if chosen.isEmpty then return
    val risky = chosen.filter(_.orphan.group == SaveGroup.NotBackedUp).map(_.orphan.displayName)
    val warning =
      if risky.isEmpty then ""
      else
        val listed = risky.take(8).map(n => s"  • $n").mkString("\n")
        val more = if risky.size > 8 then s"\n  … and ${risky.size - 8} more" else ""
        s"\n\nThese games have saves that are NOT backed up. Deleting loses them for good:\n$listed$more"
    val message = s"Delete the leftovers of ${gameCount(chosen.size)} (${totalSize(chosen.map(_.orphan)).pretty})?$warning"
    val options = Array[Object]("Delete", "Cancel")
    val messageType = if risky.isEmpty then JOptionPane.QUESTION_MESSAGE else JOptionPane.WARNING_MESSAGE
    val answer = JOptionPane.showOptionDialog(frame, message, "Delete leftovers", JOptionPane.YES_NO_OPTION,
      messageType, null, options, options(1))
    if answer != 0 then return

    setBusy(true)
    progress.setIndeterminate(false)
    progress.setMaximum(chosen.size)
    progress.setValue(0)
    inBackground {
      chosen.zipWithIndex.map { (row, i) =>
        SwingUtilities.invokeLater { () =>
          progress.setValue(i)
          status.setText(s"Deleting ${row.orphan.displayName}…")
        }
        // A failed delete may still have removed part of the tree, so measure what is left.
        val results = deleteLeftovers(row.orphan).map {
          case (l, None)    => (l, None)
          case (l, Some(e)) => (l.copy(size = if Files.exists(l.path) then treeSize(l.path) else Bytes.Zero), Some(e))
        }
        row -> results
      }
    }(deleted)

  private def deleted(result: Try[List[(GameRow, List[(Leftover, Option[Exception])])]]): Unit =
    val outcomes = result match
      case Success(outcomes) => outcomes
      case Failure(e) =>
        JOptionPane.showMessageDialog(frame, s"Deleting failed: ${e.toString}", frame.getTitle, JOptionPane.ERROR_MESSAGE)
        Nil
    outcomes.foreach { (row, results) =>
      freed += results.collect { case (l, None) => l.size }.sum
      val remaining = results.collect { case (l, Some(_)) => l }
      if remaining.isEmpty then
        val _ = rows -= row
      else
        val keepsPrefix = remaining.exists(_.kind == LeftoverKind.Prefix)
        row.orphan = row.orphan.copy(leftovers = remaining, saves = if keepsPrefix then row.orphan.saves else Nil)
    }
    gameModel.fireTableDataChanged()
    setBusy(false)
    showDetails()
    val failures = for
      (_, results)      <- outcomes
      case (l, Some(e)) <- results
    yield s"${l.path.toString}: ${e.getMessage}"
    if failures.nonEmpty then
      val more = if failures.size > 10 then s"\n… and ${failures.size - 10} more" else ""
      JOptionPane.showMessageDialog(frame, s"Some leftovers could not be deleted:\n${failures.take(10).mkString("\n")}$more",
        frame.getTitle, JOptionPane.WARNING_MESSAGE)

// ---------------------------------------------------------------------------------------------------------------------
// Main

@main def run(rawArgs: String*): Unit =
  val defaultSteamDir = SteamDir(Paths.get(sys.props("user.home"), ".local/share/Steam"))
  val args = parseArgs(rawArgs.toList, Args(defaultSteamDir, Mode.Gui, Theme.Auto))

  args.mode match
    case Mode.Cli(action) => runCli(args.steamDir, action)
    case Mode.Gui if GraphicsEnvironment.isHeadless =>
      System.err.println("No display found, so showing a dry run in the terminal. Pass --cli to skip this check.")
      runCli(args.steamDir, CliAction.DryRun)
    case Mode.Gui =>
      if !(needsNonReparentingRelaunch && relaunchNonReparenting(rawArgs)) then Gui.start(args.steamDir, args.theme)
