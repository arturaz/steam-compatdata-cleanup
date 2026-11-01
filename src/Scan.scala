import java.nio.file.{Files, LinkOption, Path}

/** The three groups games are shown in, in display order. */
enum SaveGroup derives CanEqual:
  /** Has saves that Steam Cloud does not verifiably hold: deleting may lose them for good. */
  case NotBackedUp
  /** All saves are in Steam Cloud. */
  case BackedUp
  case NoSaves

/** An app with leftover folders (Proton prefix, shader cache). */
trait HasLeftovers:
  def appId: AppId
  def name: Option[GameName]
  def leftovers: List[Leftover]
  def size: Bytes = leftovers.map(_.size).sum
  def displayName: String = name.fold(s"Unknown app $appId")(_.unwrap)
  private def has(kind: LeftoverKind) = leftovers.exists(_.kind == kind)
  /** Extra note when leftovers aren't just a Proton prefix. */
  def kindNote: Option[String] = (has(LeftoverKind.Prefix), has(LeftoverKind.ShaderCache)) match
    case (true, true)  => Some("prefix + shader cache")
    case (false, true) => Some("shader cache only")
    case _             => None

/** Leftovers of an uninstalled game: what this script offers to delete. */
case class Orphan(appId: AppId, name: Option[GameName], leftovers: List[Leftover], saves: List[Save])
    extends HasLeftovers:
  def backedUp: Boolean = saves.nonEmpty && saves.forall(_.cloud == Cloud.Synced)
  def group: SaveGroup =
    if saves.isEmpty then SaveGroup.NoSaves else if backedUp then SaveGroup.BackedUp else SaveGroup.NotBackedUp
  /** The riskiest of the game's saves. */
  def risk: Risk = saves.map(_.cloud.risk).maxByOption(_.ordinal).getOrElse(Risk.NoSaves)

/** Why a prefix or shader cache is never deleted. */
enum KeepReason(val label: String) derives CanEqual:
  case Installed extends KeepReason("installed")
  case Shortcut extends KeepReason("non-Steam shortcut")
  /** compatdata/0, which Proton sometimes creates. */
  case AppZero extends KeepReason("appid 0")

/** Leftovers of an app that is kept: shown so you can see where the rest of the space goes. */
case class Kept(appId: AppId, name: Option[GameName], reason: KeepReason, leftovers: List[Leftover])
    extends HasLeftovers:
  override def displayName: String =
    if reason == KeepReason.AppZero then "Proton (appid 0)" else super.displayName

def totalSize(os: Iterable[HasLeftovers]): Bytes = os.map(_.size).sum

/** `orphans` are ordered by group, then largest first; `kept` largest first. */
case class Scan(
    libraries: List[LibraryDir],
    installedCount: Int,
    shortcutCount: Int,
    orphans: List[Orphan],
    kept: List[Kept]
)

/** Finds leftovers of uninstalled games, and of the apps that are kept. Left is a reason to stop. */
def scan(steamDir: SteamDir): Either[String, Scan] =
  val libraries = libraryDirs(steamDir)
  if libraries.isEmpty then return Left(s"No Steam libraries found under ${steamDir.unwrap.toString}")
  val installed = installedAppIds(libraries)
  if installed.isEmpty then return Left("No appmanifests found — refusing to continue (would delete everything).")
  val shortcuts = shortcutAppIds(steamDir)
  val keep = installed ++ shortcuts + AppId.Zero

  val allDirs: Map[AppId, List[(LeftoverKind, Path)]] =
    (for
      lib   <- libraries
      kind  <- LeftoverKind.values.toList
      dir   <- listDir(lib.leftovers(kind))
      if Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)
      appId <- AppId.parse(dir.getFileName.toString)
    yield appId -> (kind, dir)).groupMap(_._1)(_._2)
  val (keptDirs, orphanDirs) = allDirs.partition((appId, _) => keep.contains(appId))
  def measure(dirs: List[(LeftoverKind, Path)]) = dirs.map((kind, dir) => Leftover(kind, dir, treeSize(dir)))

  val infos = appInfos(steamDir, allDirs.keySet)
  val syncStates = if orphanDirs.isEmpty then Map.empty else cloudSyncStates(steamDir)
  val orphans = orphanDirs.toList
    .map { (appId, dirs) =>
      val info = infos.get(appId)
      val saves = dirs.collect { case (LeftoverKind.Prefix, dir) => dir }
        .flatMap(likelySaves(_, cloudStatus(info, syncStates, appId)))
      Orphan(appId, info.flatMap(gameName), measure(dirs), saves)
    }
    .sortBy(o => (o.group.ordinal, -o.size.unwrap, o.appId))

  val names = installedNames(libraries, keptDirs.keySet & installed) ++
    (if (keptDirs.keySet & shortcuts).isEmpty then Map.empty else shortcutNames(steamDir))
  val kept = keptDirs.toList
    .map { (appId, dirs) =>
      val reason =
        if installed.contains(appId) then KeepReason.Installed
        else if shortcuts.contains(appId) then KeepReason.Shortcut
        else KeepReason.AppZero
      Kept(appId, names.get(appId).orElse(infos.get(appId).flatMap(gameName)), reason, measure(dirs))
    }
    .sortBy(k => (-k.size.unwrap, k.appId))
  Right(Scan(libraries, installed.size, shortcuts.size, orphans, kept))

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
