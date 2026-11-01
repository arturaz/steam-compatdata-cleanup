import java.nio.file.Paths

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
