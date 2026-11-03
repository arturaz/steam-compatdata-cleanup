import java.nio.file.Path
import yantl.*

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
  given CanEqual[SteamDir, SteamDir] = CanEqual.derived
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
