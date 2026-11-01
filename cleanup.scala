#!/usr/bin/env -S sh -c 'command -v scala-cli >/dev/null||{ echo "cleanup.scala needs scala-cli: https://scala-cli.virtuslab.org/install">&2;exit 127;};exec scala-cli shebang "$0" "$@"'
//> using scala 3.9.0
//> using options -Wall -Werror -language:strictEquality
//> using dep io.github.arturaz::yantl:0.4.3

// Removes Proton prefixes (steamapps/compatdata/<appid>) and shader caches (steamapps/shadercache/<appid>)
// for games that are no longer installed.
//
// Requires scala-cli (https://scala-cli.virtuslab.org/install); running without it prints that link.
//
// Usage:
//   ./cleanup.scala                  # dry run: list what would be deleted
//   ./cleanup.scala --interactive    # ask per game, delete the ones you confirm
//   ./cleanup.scala --delete         # delete everything listed, no questions
//   ./cleanup.scala --steam-dir /path/to/Steam
//
// An app is considered installed if any Steam library has steamapps/appmanifest_<appid>.acf.
// Non-Steam shortcuts (from userdata/*/config/shortcuts.vdf) and appid 0 are always kept.
// Game names and Steam Cloud rules come from Steam's local caches; no network access.

import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}
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
enum LeftoverKind(val dirName: String) derives CanEqual:
  /** Proton/Wine prefix: steamapps/compatdata/<appid>. */
  case Prefix extends LeftoverKind("compatdata")
  /** Compiled shader cache: steamapps/shadercache/<appid>. */
  case ShaderCache extends LeftoverKind("shadercache")

case class Leftover(kind: LeftoverKind, path: Path, size: Bytes)

// ---------------------------------------------------------------------------------------------------------------------
// Arguments

enum Mode derives CanEqual:
  case DryRun, Delete, Interactive

case class Args(steamDir: SteamDir, mode: Mode)

val usage = "Usage: cleanup.scala [--delete | --interactive|-i] [--steam-dir <path>]"

def parseArgs(args: List[String], acc: Args): Args = args match
  case Nil                              => acc
  case "--delete" :: rest               => parseArgs(rest, acc.copy(mode = Mode.Delete))
  case ("--interactive" | "-i") :: rest => parseArgs(rest, acc.copy(mode = Mode.Interactive))
  case "--steam-dir" :: dir :: rest     => parseArgs(rest, acc.copy(steamDir = SteamDir(Paths.get(dir))))
  case ("--help" | "-h") :: _           => println(usage); sys.exit(0)
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

/** A non-empty, game-created file or folder in one of a prefix's save folders. */
case class Save(folder: UserFolder, entry: EntryName, size: Bytes, cloud: Cloud):
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
  yield Save(folder, entry, size, cloud(folder, entry))

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
// Output

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

case class Orphan(appId: AppId, name: Option[GameName], leftovers: List[Leftover], saves: List[Save]):
  def size: Bytes = leftovers.map(_.size).sum
  def backedUp: Boolean = saves.nonEmpty && saves.forall(_.cloud == Cloud.Synced)
  def displayName: String = name.fold(s"Unknown app $appId")(_.unwrap)
  private def has(kind: LeftoverKind) = leftovers.exists(_.kind == kind)
  /** Extra note when leftovers aren't just a Proton prefix. */
  def kindNote: Option[String] = (has(LeftoverKind.Prefix), has(LeftoverKind.ShaderCache)) match
    case (true, true)  => Some("prefix + shader cache")
    case (false, true) => Some("shader cache only")
    case _             => None

def totalSize(os: List[Orphan]): Bytes = os.map(_.size).sum

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
      val (color, status) = save.cloud match
        case Cloud.Synced             => (green, "☁ in Steam Cloud")
        case Cloud.Unconfirmed(state) => (yellow, s"☁ Steam Cloud, last sync: ${state.fold("unknown")(_.unwrap)}")
        case Cloud.NotCovered         => (red, "✗ not synced by Steam Cloud")
        case Cloud.NoCloud            => (red, "✗ game has no Steam Cloud")
      println(s"      ${padRight(s"$branch ${save.path}", nameW - 2)}  ${padLeft(save.size.pretty, sizeW)}  ${color(status)}")
    }
  }
  println()

// ---------------------------------------------------------------------------------------------------------------------
// Deletion

def delete(o: Orphan): Bytes =
  o.leftovers.map { l =>
    try
      deleteTree(l.path)
      println(s"    ${green("✓")} deleted ${dim(l.path.toString)}")
      l.size
    catch case e: Exception =>
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

// ---------------------------------------------------------------------------------------------------------------------
// Main

@main def run(rawArgs: String*): Unit =
  val defaultSteamDir = SteamDir(Paths.get(sys.props("user.home"), ".local/share/Steam"))
  val args = parseArgs(rawArgs.toList, Args(defaultSteamDir, Mode.DryRun))

  val libraries = libraryDirs(args.steamDir)
  if libraries.isEmpty then
    System.err.println(s"No Steam libraries found under ${args.steamDir.unwrap.toString}")
    sys.exit(1)

  val installed = installedAppIds(libraries)
  val shortcuts = shortcutAppIds(args.steamDir)
  val keep = installed ++ shortcuts + AppId.Zero

  def plural(n: Int, word: String) = s"$n $word${if n == 1 then "" else "s"}"
  println(dim(s"Steam libraries: ${libraries.mkString(", ")}"))
  println(dim(s"Keeping ${plural(installed.size, "installed app")} and ${plural(shortcuts.size, "non-Steam shortcut")}."))
  if installed.isEmpty then
    System.err.println("No appmanifests found — refusing to continue (would delete everything).")
    sys.exit(1)

  val orphanDirs: Map[AppId, List[(LeftoverKind, Path)]] =
    (for
      lib   <- libraries
      kind  <- LeftoverKind.values.toList
      dir   <- listDir(lib.leftovers(kind))
      if Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)
      appId <- AppId.parse(dir.getFileName.toString)
      if !keep.contains(appId)
    yield appId -> (kind, dir)).groupMap(_._1)(_._2)

  if orphanDirs.isEmpty then
    println(green("\nNothing to clean up."))
    return

  val infos = appInfos(args.steamDir, orphanDirs.keySet)
  val syncStates = cloudSyncStates(args.steamDir)
  val all = orphanDirs.toList
    .map { (appId, dirs) =>
      val info = infos.get(appId)
      val saves = dirs.collect { case (LeftoverKind.Prefix, dir) => dir }
        .flatMap(likelySaves(_, cloudStatus(info, syncStates, appId)))
      val leftovers = dirs.map((kind, dir) => Leftover(kind, dir, treeSize(dir)))
      Orphan(appId, info.flatMap(gameName), leftovers, saves)
    }
    .sortBy(o => (-o.size.unwrap, o.appId)) // largest first
  val notBackedUp = all.filter(o => o.saves.nonEmpty && !o.backedUp)
  val backedUp = all.filter(_.backedUp)
  val noSaves = all.filter(_.saves.isEmpty)
  val orphans = notBackedUp ++ backedUp ++ noSaves // display order = interactive order

  def games(n: Int) = plural(n, "game")
  def summary(os: List[Orphan]) = s"${games(os.size)}, ${totalSize(os).pretty}"

  val nameW = (orphans.map(o => displayWidth(o.displayName)) ++
    orphans.flatMap(_.saves.map(s => displayWidth(s.path) + 5))).max.min(60)

  println()
  println(bold(s"Uninstalled games are using ${totalSize(orphans).pretty}") + dim(s" (${games(orphans.size)})"))
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

  args.mode match
    case Mode.DryRun =>
      println(s"${cyan("Dry run")} — nothing deleted. Use ${bold("--interactive")} to choose per game, " +
        s"or ${bold("--delete")} to remove all.")
    case Mode.Delete =>
      val freed = orphans.map(delete).sum
      println(green(s"\nFreed ${freed.pretty}."))
    case Mode.Interactive =>
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
