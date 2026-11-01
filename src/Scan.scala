import java.nio.file.{Files, LinkOption, Path}

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
