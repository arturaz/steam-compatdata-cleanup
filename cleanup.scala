#!/usr/bin/env -S scala-cli shebang
//> using scala 3.9.0

// Removes Proton prefixes (steamapps/compatdata/<appid>) and shader caches (steamapps/shadercache/<appid>)
// for games that are no longer installed.
//
// Usage:
//   ./cleanup.scala                  # dry run: list what would be deleted
//   ./cleanup.scala --interactive    # ask per game, delete the ones you confirm
//   ./cleanup.scala --delete         # delete everything listed, no questions
//   ./cleanup.scala --steam-dir /path/to/Steam
//
// An app is considered installed if any Steam library has steamapps/appmanifest_<appid>.acf.
// Non-Steam shortcuts (from userdata/*/config/shortcuts.vdf) and appid 0 are always kept.
// Game names come from Steam's local cache (appcache/appinfo.vdf); no network access.

import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

// ---------------------------------------------------------------------------------------------------------------------
// Arguments

enum Mode:
  case DryRun, Delete, Interactive

case class Args(steamDir: Path, mode: Mode)

val usage = "Usage: cleanup.scala [--delete | --interactive|-i] [--steam-dir <path>]"

def parseArgs(args: List[String], acc: Args): Args = args match
  case Nil                                       => acc
  case "--delete" :: rest                        => parseArgs(rest, acc.copy(mode = Mode.Delete))
  case ("--interactive" | "-i") :: rest          => parseArgs(rest, acc.copy(mode = Mode.Interactive))
  case "--steam-dir" :: dir :: rest              => parseArgs(rest, acc.copy(steamDir = Paths.get(dir)))
  case ("--help" | "-h") :: _                    => println(usage); sys.exit(0)
  case other :: _ =>
    System.err.println(s"Unknown argument: $other\n$usage")
    sys.exit(2)

// ---------------------------------------------------------------------------------------------------------------------
// Filesystem helpers

def listDir(dir: Path): List[Path] =
  if !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS) then Nil
  else Using.resource(Files.list(dir))(_.iterator.asScala.toList.sortBy(_.getFileName.toString))

/** Total size of regular files in a tree, not following symlinks. */
def treeSize(path: Path): Long =
  var total = 0L
  Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path]:
      override def visitFile(f: Path, attrs: BasicFileAttributes) =
        if attrs.isRegularFile then total += attrs.size
        FileVisitResult.CONTINUE
      override def visitFileFailed(f: Path, e: java.io.IOException) = FileVisitResult.CONTINUE
  )
  total

/** Recursively deletes a tree, deleting symlinks themselves rather than their targets. */
def deleteTree(dir: Path): Unit =
  Files.walkFileTree(
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

def human(bytes: Long): String =
  val units = List("B", "KiB", "MiB", "GiB", "TiB")
  var v = bytes.toDouble
  var u = 0
  while v >= 1024 && u < units.length - 1 do
    v /= 1024
    u += 1
  f"$v%.1f ${units(u)}"

// ---------------------------------------------------------------------------------------------------------------------
// Steam metadata

/** Library roots from steamapps/libraryfolders.vdf (always includes the main Steam dir). */
def libraryRoots(steamDir: Path): List[Path] =
  val vdf = steamDir.resolve("steamapps/libraryfolders.vdf")
  val fromVdf =
    if !Files.isRegularFile(vdf) then Nil
    else
      val PathLine = """(?m)^\s*"path"\s+"((?:[^"\\]|\\.)*)"""".r
      PathLine
        .findAllMatchIn(Files.readString(vdf))
        .map(m => Paths.get(m.group(1).replace("\\\\", "\\")))
        .toList
  (steamDir :: fromVdf).map(_.toAbsolutePath.normalize).distinct.filter(p => Files.isDirectory(p.resolve("steamapps")))

/** App ids that have an appmanifest in any library. */
def installedAppIds(libraries: List[Path]): Set[Long] =
  val Manifest = """appmanifest_(\d+)\.acf""".r
  libraries
    .flatMap(lib => listDir(lib.resolve("steamapps")))
    .flatMap(p => p.getFileName.toString match
      case Manifest(id) => Some(id.toLong)
      case _            => None
    )
    .toSet

/** App ids of non-Steam shortcuts. shortcuts.vdf is binary VDF: 0x02 "appid" 0x00 <int32 LE>. */
def shortcutAppIds(steamDir: Path): Set[Long] =
  val marker = "\u0002appid\u0000".getBytes(StandardCharsets.ISO_8859_1)
  listDir(steamDir.resolve("userdata"))
    .map(_.resolve("config/shortcuts.vdf"))
    .filter(Files.isRegularFile(_))
    .flatMap { file =>
      val bytes = Files.readAllBytes(file)
      // Case-insensitive match: older Steam versions wrote "AppID".
      val lower = bytes.map(b => if b >= 'A' && b <= 'Z' then (b + 32).toByte else b)
      (0 to bytes.length - marker.length - 4)
        .filter(i => java.util.Arrays.equals(lower, i, i + marker.length, marker, 0, marker.length))
        .map(i => Integer.toUnsignedLong(ByteBuffer.wrap(bytes, i + marker.length, 4).order(ByteOrder.LITTLE_ENDIAN).getInt))
    }
    .toSet

/** Reads app names (appinfo > common > name) for the given ids from appcache/appinfo.vdf.
  *
  * Format (versions 0x27, 0x28, 0x29 of magic 0x075644xx):
  *   header: u32 magic, u32 universe, [v29: i64 string table offset]
  *   per app: u32 appid (0 = end), u32 size, then `size` bytes:
  *     u32 infoState, u32 lastUpdated, u64 picsToken, 20b sha1, u32 changeNumber, [v28+: 20b binary sha1], binary KV
  *   binary KV entry: u8 type (0x08 = end of map), key (v29: i32 string table index, else C string), value.
  * Returns an empty map if the file is missing or in an unknown format.
  */
type KV = Map[String, Any]

extension (kv: KV)
  /** Follows a path of keys through nested maps. */
  def at(path: String*): Option[Any] =
    path.foldLeft(Option[Any](kv)) {
      case (Some(m: Map[?, ?]), k) => m.asInstanceOf[KV].get(k)
      case _                       => None
    }
  def str(path: String*): Option[String] = at(path*).collect { case s: String => s }
  def obj(path: String*): Option[KV] = at(path*).collect { case m: Map[?, ?] => m.asInstanceOf[KV] }
  /** Like `at`, but matches keys case-insensitively (Steam's text VDF files are inconsistent about case). */
  def ci(path: String*): Option[Any] =
    path.foldLeft(Option[Any](kv)) {
      case (Some(m: Map[?, ?]), k) => m.asInstanceOf[KV].collectFirst { case (key, v) if key.equalsIgnoreCase(k) => v }
      case _                       => None
    }

def appInfos(steamDir: Path, wanted: Set[Long]): Map[Long, KV] =
  val file = steamDir.resolve("appcache/appinfo.vdf")
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

    def key(): String = if version >= 0x29 then stringTable(buf.getInt) else cString()

    /** Parses a map body until 0x08. Values: nested maps, strings (also for numbers), others skipped. */
    def readMap(): KV =
      val b = Map.newBuilder[String, Any]
      var tpe = buf.get & 0xff
      while tpe != 0x08 do
        val k = key()
        tpe match
          case 0x00        => b += k -> readMap()
          case 0x01        => b += k -> cString()
          case 0x02        => b += k -> buf.getInt.toString
          case 0x03        => b += k -> buf.getFloat.toString
          case 0x04 | 0x06 => buf.getInt
          case 0x07 | 0x0a => b += k -> buf.getLong.toString
          case 0x05        => while buf.getShort != 0 do ()
          case other       => throw IllegalStateException(s"unknown KV type 0x${other.toHexString}")
        tpe = buf.get & 0xff
      b.result()

    val infos = Map.newBuilder[Long, KV]
    var appId = Integer.toUnsignedLong(buf.getInt)
    while appId != 0 do
      val size = buf.getInt
      val next = buf.position + size
      if wanted.contains(appId) then
        buf.position(buf.position + 4 + 4 + 8 + 20 + 4 + (if version >= 0x28 then 20 else 0))
        readMap().obj("appinfo").foreach(info => infos += appId -> info)
      buf.position(next)
      appId = Integer.toUnsignedLong(buf.getInt)
    infos.result()
  }.recover { case e =>
    System.err.println(s"Warning: could not read Steam app cache $file: $e")
    Map.empty
  }.get

// ---------------------------------------------------------------------------------------------------------------------
// Save data detection

/** Folders inside a prefix where games commonly keep saves and settings (relative to drive_c/users/steamuser). */
val saveRoots = List("AppData/Local", "AppData/LocalLow", "AppData/Roaming", "Documents", "Saved Games")

/** Entries that Wine/Proton/runtimes create in every prefix and that never hold game saves. */
val notSaves = Set("Microsoft", "Temp", "openvr", "dxvk", "vkd3d", "vkd3d-proton", "mesa_shader_cache", "UnrealEngine",
  "Downloads", "Music", "Pictures", "Templates", "Videos")

enum Cloud:
  /** An Auto-Cloud rule covers this folder and Steam's last sync for the game succeeded. */
  case Synced
  /** An Auto-Cloud rule covers this folder, but the last sync state is not "synchronized" (or unknown). */
  case Unconfirmed(state: Option[String])
  /** The game uses Steam Cloud, but none of its rules cover this folder. */
  case NotCovered
  /** The game does not use Steam Cloud at all. */
  case NoCloud

/** A non-empty, game-created folder in a prefix's usual save locations. `root` is e.g. "AppData/LocalLow". */
case class Save(root: String, entry: String, size: Long, cloud: Cloud):
  def path: String = s"$root/$entry"

/** Non-empty, game-created entries in the usual save locations of a prefix. */
def likelySaves(compatdata: Path, cloud: (String, String) => Cloud): List[Save] =
  val user = compatdata.resolve("pfx/drive_c/users/steamuser")
  for
    root  <- saveRoots
    entry <- listDir(user.resolve(root))
    name = entry.getFileName.toString
    if !Files.isSymbolicLink(entry) && !notSaves.contains(name)
    size = treeSize(entry)
    if size > 0
  yield Save(root, name, size, cloud(root, name))

// ---------------------------------------------------------------------------------------------------------------------
// Steam Cloud

/** Minimal parser for Steam's text VDF (KeyValues) format. Nested maps and string values. */
def parseTextVdf(s: String): KV =
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
  def obj(): KV =
    val b = Map.newBuilder[String, Any]
    skipWs()
    while i < s.length && s(i) != '}' do
      val k = token()
      skipWs()
      if i < s.length && s(i) == '{' then
        i += 1
        b += k -> obj()
        i += 1 // closing brace
      else b += k -> token()
      skipWs()
    b.result()
  obj()

/** Steam Auto-Cloud root names → folder inside a Proton prefix's drive_c/users/steamuser. */
val cloudRoots = Map(
  "WinAppDataLocal"    -> "AppData/Local",
  "WinAppDataLocalLow" -> "AppData/LocalLow",
  "WinAppDataRoaming"  -> "AppData/Roaming",
  "WinMyDocuments"     -> "Documents",
  "WinSavedGames"      -> "Saved Games"
)

/** Last Steam Cloud sync state per app id ("synchronized", ...), across all local Steam users. */
def cloudSyncStates(steamDir: Path): Map[Long, List[String]] =
  listDir(steamDir.resolve("userdata"))
    .map(_.resolve("config/localconfig.vdf"))
    .filter(Files.isRegularFile(_))
    .flatMap { file =>
      Try(parseTextVdf(Files.readString(file))).toOption.toList.flatMap { vdf =>
        val apps = vdf.values.collect { case m: Map[?, ?] => m.asInstanceOf[KV] }
          .flatMap(_.ci("Software", "Valve", "Steam", "apps").collect { case m: Map[?, ?] => m.asInstanceOf[KV] })
          .headOption.getOrElse(Map.empty)
        apps.toList.flatMap { (id, app) =>
          for
            appId <- id.toLongOption
            state <- app.asInstanceOf[Any] match
              case m: Map[?, ?] => m.asInstanceOf[KV].ci("cloud", "last_sync_state").collect { case s: String => s }
              case _            => None
          yield appId -> state
        }
      }
    }
    .groupMap(_._1)(_._2)

/** Decides whether a prefix folder (`root`/`entry`) of an app is backed up by Steam Cloud. */
def cloudStatus(info: Option[KV], syncStates: Map[Long, List[String]], appId: Long)(root: String, entry: String): Cloud =
  info.flatMap(_.obj("ufs")) match
    case None => Cloud.NoCloud
    case Some(ufs) =>
      val rules = ufs.obj("savefiles").toList.flatMap(_.values).collect { case m: Map[?, ?] => m.asInstanceOf[KV] }
      val covered = rules.exists { rule =>
        val windows = rule.obj("platforms").forall(_.values.exists(v => v == "Windows" || v == "all"))
        val firstSegment = rule.str("path").getOrElse("").split("[/\\\\]").headOption.getOrElse("")
        windows && rule.str("root").flatMap(cloudRoots.get).contains(root) &&
          (firstSegment.isEmpty || firstSegment.equalsIgnoreCase(entry))
      }
      if !covered then Cloud.NotCovered
      else
        // Demos often sync into the full game's cloud storage.
        val cloudId = ufs.str("appidredirect").flatMap(_.toLongOption).getOrElse(appId)
        val states = syncStates.getOrElse(cloudId, Nil) ++ syncStates.getOrElse(appId, Nil)
        if states.contains("synchronized") then Cloud.Synced else Cloud.Unconfirmed(states.headOption)

// ---------------------------------------------------------------------------------------------------------------------
// Main

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

case class Orphan(appId: Long, name: Option[String], dirs: List[(Path, Long)], saves: List[Save]):
  def size: Long = dirs.map(_._2).sum
  def backedUp: Boolean = saves.nonEmpty && saves.forall(_.cloud == Cloud.Synced)
  def displayName: String = name.getOrElse(s"Unknown app $appId")
  private def has(kind: String) = dirs.exists(_._1.getParent.getFileName.toString == kind)
  /** Extra note when leftovers aren't just a Proton prefix. */
  def kindNote: Option[String] = (has("compatdata"), has("shadercache")) match
    case (true, true)  => Some("prefix + shader cache")
    case (false, true) => Some("shader cache only")
    case _             => None

val sizeW = 10

/** Prints one section of games with a heading; `nameW` keeps size columns aligned across sections. */
def printSection(heading: String, games: List[Orphan], nameW: Int): Unit =
  if games.isEmpty then return
  println(heading)
  games.foreach { o =>
    val size = if o.size == 0 then dim(human(o.size)) else human(o.size)
    val name = if o.name.isEmpty then dim(o.displayName) else bold(o.displayName)
    val note = o.kindNote.map(n => s"  ${dim(n)}").getOrElse("")
    println(s"    ${padRight(name, nameW)}  ${padLeft(size, sizeW)}  ${dim(s"#${o.appId}")}$note")
    o.saves.zipWithIndex.foreach { (save, i) =>
      val branch = if i == o.saves.length - 1 then "└─" else "├─"
      val (color, status) = save.cloud match
        case Cloud.Synced             => (green, "☁ in Steam Cloud")
        case Cloud.Unconfirmed(state) => (yellow, s"☁ Steam Cloud, last sync: ${state.getOrElse("unknown")}")
        case Cloud.NotCovered         => (red, "✗ not synced by Steam Cloud")
        case Cloud.NoCloud            => (red, "✗ game has no Steam Cloud")
      println(s"      ${padRight(s"$branch ${save.path}", nameW - 2)}  ${padLeft(human(save.size), sizeW)}  ${color(status)}")
    }
  }
  println()

def delete(o: Orphan): Long =
  o.dirs.map { (d, size) =>
    try
      deleteTree(d)
      println(s"    ${green("✓")} deleted ${dim(d.toString)}")
      size
    catch case e: Exception =>
      System.err.println(s"    ${red("✗")} failed to delete $d: ${e.getMessage}")
      0L
  }.sum

enum Answer:
  case Yes, No, All, Quit

def ask(o: Orphan): Answer =
  val warning =
    if o.saves.isEmpty then ""
    else if o.backedUp then green(" — saves are in Steam Cloud")
    else red(" — saves NOT backed up!")
  print(s"Delete ${bold(o.displayName)} (${human(o.size)})$warning ${dim("[y/N/a(ll)/q(uit)]")} ")
  Console.flush()
  Option(scala.io.StdIn.readLine()).map(_.trim.toLowerCase) match
    case None                       => Answer.Quit // EOF
    case Some("y" | "yes")          => Answer.Yes
    case Some("a" | "all")          => Answer.All
    case Some("q" | "quit")         => Answer.Quit
    case Some(_)                    => Answer.No

@main def run(rawArgs: String*): Unit =
  val args = parseArgs(rawArgs.toList, Args(Paths.get(sys.props("user.home"), ".local/share/Steam"), Mode.DryRun))

  val libraries = libraryRoots(args.steamDir)
  if libraries.isEmpty then
    System.err.println(s"No Steam libraries found under ${args.steamDir}")
    sys.exit(1)

  val installed = installedAppIds(libraries)
  val shortcuts = shortcutAppIds(args.steamDir)
  val keep = installed ++ shortcuts + 0L

  println(dim(s"Steam libraries: ${libraries.mkString(", ")}"))
  def plural(n: Int, word: String) = s"$n $word${if n == 1 then "" else "s"}"
  println(dim(s"Keeping ${plural(installed.size, "installed app")} and ${plural(shortcuts.size, "non-Steam shortcut")}."))
  if installed.isEmpty then
    System.err.println("No appmanifests found — refusing to continue (would delete everything).")
    sys.exit(1)

  val orphanDirs: Map[Long, List[Path]] =
    (for
      lib  <- libraries
      kind <- List("compatdata", "shadercache")
      dir  <- listDir(lib.resolve("steamapps").resolve(kind))
      if Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)
      id   <- dir.getFileName.toString.toLongOption
      if !keep.contains(id)
    yield id -> dir).groupMap(_._1)(_._2)

  if orphanDirs.isEmpty then
    println(green("\nNothing to clean up."))
    return

  val infos = appInfos(args.steamDir, orphanDirs.keySet)
  val syncStates = cloudSyncStates(args.steamDir)
  val all = orphanDirs.toList
    .map { (id, dirs) =>
      val info = infos.get(id)
      val saves = dirs
        .filter(_.getParent.getFileName.toString == "compatdata")
        .flatMap(likelySaves(_, cloudStatus(info, syncStates, id)))
      Orphan(id, info.flatMap(_.str("common", "name")), dirs.map(d => d -> treeSize(d)), saves)
    }
    .sortBy(o => (-o.size, o.appId))
  val notBackedUp = all.filter(o => o.saves.nonEmpty && !o.backedUp)
  val backedUp = all.filter(_.backedUp)
  val noSaves = all.filter(_.saves.isEmpty)
  val orphans = notBackedUp ++ backedUp ++ noSaves // display order = interactive order

  def games(n: Int) = s"$n game${if n == 1 then "" else "s"}"
  def summary(os: List[Orphan]) = s"${games(os.size)}, ${human(os.map(_.size).sum)}"

  val nameW = (orphans.map(o => displayWidth(o.displayName)) ++
    orphans.flatMap(_.saves.map(s => displayWidth(s.path) + 5))).max.min(60)

  println()
  println(bold(s"Uninstalled games are using ${human(orphans.map(_.size).sum)}") + dim(s" (${games(orphans.size)})"))
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
      println(green(s"\nFreed ${human(freed)}."))
    case Mode.Interactive =>
      var freed = 0L
      var all = false
      val it = orphans.iterator
      var quit = false
      while it.hasNext && !quit do
        val o = it.next()
        val answer = if all then Answer.Yes else ask(o)
        answer match
          case Answer.Yes  => freed += delete(o)
          case Answer.All  => all = true; freed += delete(o)
          case Answer.No   => ()
          case Answer.Quit => quit = true
      println(green(s"\nFreed ${human(freed)}."))
