import java.awt.Color
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

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
