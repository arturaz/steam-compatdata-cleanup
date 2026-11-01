import java.nio.file.{Files, Path}

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
