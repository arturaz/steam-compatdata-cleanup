import java.io.File
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Try

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

/** `steamDir` is None unless --steam-dir was given; then the script looks for Steam installs and lets you pick. */
case class Args(steamDir: Option[SteamDir], mode: Mode, theme: Theme)

/** The comment lines between `// help:begin` and `// help:end` in cleanup.scala, read from the source scala-cli
  * reports.
  */
def usage: String =
  val fromComment = for
    sources <- sys.props.get("scala.sources")
    script <- sources.split(File.pathSeparator).find(_.endsWith("cleanup.scala"))
    lines <- Try(Files.readAllLines(Paths.get(script)).asScala.toList).toOption
    (block, rest) = lines.dropWhile(_ != "// help:begin").drop(1).span(_ != "// help:end")
    if block.nonEmpty && rest.nonEmpty
  yield block.map(_.stripPrefix("//").stripPrefix(" ")).mkString("\n")
  fromComment.getOrElse("See the comment at the top of cleanup.scala for usage.")

def parseArgs(args: List[String], acc: Args): Args = args match
  case Nil                              => acc
  case "--gui" :: rest                  => parseArgs(rest, acc.copy(mode = Mode.Gui))
  case "--cli" :: rest                  => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.DryRun)))
  case "--delete" :: rest               => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.Delete)))
  case ("--interactive" | "-i") :: rest => parseArgs(rest, acc.copy(mode = Mode.Cli(CliAction.Interactive)))
  case "--steam-dir" :: dir :: rest     => parseArgs(rest, acc.copy(steamDir = Some(SteamDir(Paths.get(dir)))))
  case "--theme" :: name :: rest =>
    Theme.parse(name) match
      case Some(theme) => parseArgs(rest, acc.copy(theme = theme))
      case None =>
        System.err.println(s"Unknown theme: $name\n$usage")
        sys.exit(2)
  case ("--help" | "-h") :: _ => println(s"\n$usage\n"); sys.exit(0)
  case other :: _ =>
    System.err.println(s"Unknown argument: $other\n$usage")
    sys.exit(2)
