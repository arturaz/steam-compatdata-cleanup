import java.awt.Color
import java.nio.file.Path
import javax.swing.{SwingUtilities, UIManager}
import javax.swing.plaf.FontUIResource
import javax.swing.text.StyleContext
import com.formdev.flatlaf.{FlatDarculaLaf, FlatDarkLaf, FlatIntelliJLaf, FlatLightLaf}
import scala.util.Try

object Gui:
  /** `choices` are the Steam installs to offer in the window; with fewer than two there is no selector. */
  def start(choices: List[SteamDir], steamDir: SteamDir, theme: Theme): Unit =
    // Runs commands, so do it before touching the event thread.
    val dark = theme == Theme.Auto && DesktopDarkMode.prefersDark
    SwingUtilities.invokeLater { () =>
      applyTheme(theme, autoDark = dark)
      MainWindow(choices, steamDir, theme).show()
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
