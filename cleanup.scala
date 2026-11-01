#!/usr/bin/env -S sh -c 'eval "$(sed -n "s|^//sh ||p" "$0")"'
//sh # Shell bootstrap, run by the line above: find scala-cli, or explain how to get it (a dialog when not in a terminal).
//sh # A GUI double-click does not read shell rc files, so also look in the usual install locations.
//sh for d in "$HOME/.local/share/coursier/bin" "$HOME/Library/Application Support/Coursier/bin" \
//sh   "$HOME/.sdkman/candidates/scalacli/current/bin" "$HOME/.nix-profile/bin" "$HOME/.local/bin" \
//sh   /opt/homebrew/bin /usr/local/bin; do PATH="$PATH:$d"; done
//sh has() { command -v "$1" >/dev/null 2>&1; }; title="Steam compatdata cleanup"
//sh if has scala-cli; then
//sh   # Use the system JDK (JAVA_HOME first, then PATH, like scala-cli) when it is Java 17+ with GUI support
//sh   # (headless JDK packages lack libawt_xawt / libawt_lwawt). Otherwise let scala-cli fetch JDK 21 once.
//sh   java="${JAVA_HOME:+$JAVA_HOME/bin/java}"; [ -x "${java:-}" ] || java=$(command -v java 2>/dev/null)
//sh   props=$([ -n "$java" ] && "$java" -XshowSettings:properties -version 2>&1)
//sh   ver=$(printf '%s\n' "$props" | sed -n 's/^ *java\.specification\.version = \([0-9]*\).*/\1/p')
//sh   home=$(printf '%s\n' "$props" | sed -n 's/^ *java\.home = //p')
//sh   jvm="--jvm 21"; [ "${ver:-0}" -ge 17 ] 2>/dev/null && ls "$home"/lib/libawt_*awt.* >/dev/null 2>&1 && jvm=
//sh   # Outside a terminal scala-cli's download/compile progress is invisible, so if the program has not started
//sh   # within a second, show a notification until it does. The program deletes $CLEANUP_STARTING_FILE on start.
//sh   if [ ! -t 2 ] && CLEANUP_STARTING_FILE=$(mktemp 2>/dev/null); then
//sh     export CLEANUP_STARTING_FILE; ready=$CLEANUP_STARTING_FILE; shell=$$
//sh     (
//sh       waiting() { [ -e "$ready" ] && kill -0 "$shell"; }
//sh       sleep 1; waiting || { rm -f "$ready"; exit; }
//sh       msg="Starting… The first run downloads Scala, the libraries and maybe Java, which can take a few minutes."
//sh       if has notify-send && id=$(notify-send -p -t 0 -a "$title" -i system-software-update "$title" "$msg") \
//sh         && [ -n "$id" ]; then
//sh         while waiting; do sleep 1; done
//sh         gdbus call --session --dest org.freedesktop.Notifications --object-path /org/freedesktop/Notifications \
//sh           --method org.freedesktop.Notifications.CloseNotification "$id" ||
//sh           notify-send -r "$id" -t 1 -a "$title" "$title" "Started."
//sh       elif has zenity; then
//sh         zenity --progress --pulsate --no-cancel --title="$title" --text="$msg" & dialog=$!
//sh         while waiting; do sleep 1; done; kill "$dialog"
//sh       elif has osascript; then
//sh         osascript -e "display notification \"$msg\" with title \"$title\""
//sh         while waiting; do sleep 1; done
//sh       fi
//sh       rm -f "$ready"
//sh     ) </dev/null >/dev/null 2>&1 &
//sh   fi
//sh   exec scala-cli shebang $jvm "$0" "$@"
//sh fi
//sh url=https://scala-cli.virtuslab.org/install
//sh msg="cleanup.scala needs scala-cli to run. Install it from $url and start the script again."
//sh echo "$msg" >&2
//sh if [ -t 2 ]; then [ -t 0 ] && printf "Press Enter to close." >&2 && read -r _; exit 127; fi
//sh ask="Open install page"
//sh if has osascript; then
//sh   osascript -e "display dialog \"$msg\" with title \"$title\" buttons {\"Close\", \"$ask\"} default button 2 with icon stop" \
//sh     -e "if button returned of result is \"$ask\" then open location \"$url\"" >/dev/null 2>&1
//sh elif has kdialog && case "$XDG_CURRENT_DESKTOP" in *KDE*) true;; *) ! has zenity;; esac; then
//sh   kdialog --title "$title" --yes-label "$ask" --no-label Close --yesno "$msg" && xdg-open "$url"
//sh elif has zenity; then
//sh   zenity --question --title="$title" --ok-label="$ask" --cancel-label=Close --text="$msg" && xdg-open "$url"
//sh elif has xmessage; then
//sh   xmessage -center -title "$title" -buttons "$ask:0,Close:1" "$msg" && xdg-open "$url"
//sh elif has notify-send; then
//sh   notify-send -u critical "$title" "$msg"
//sh fi >/dev/null 2>&1
//sh exit 127
//> using scala 3.9.0
//> using options -Wall -Werror -language:strictEquality
//> using dep io.github.arturaz::yantl:0.4.3
//> using dep com.formdev:flatlaf:3.7.2
//> using file src

// --help prints the lines between the help markers (see `usage` in src/Args.scala), so keep them user-facing.
// help:begin
// Removes Proton prefixes (steamapps/compatdata/<appid>) and shader caches (steamapps/shadercache/<appid>)
// for games that are no longer installed.
//
// Usage:
//   ./cleanup.scala                  # GUI: pick games to delete (dry run in the terminal if there is no display)
//   ./cleanup.scala --theme dark     # GUI look: auto (default, follows the desktop), light, dark, darcula, intellij,
//                                    #   gtk, metal, nimbus
//   ./cleanup.scala --cli            # dry run in the terminal: list what would be deleted
//   ./cleanup.scala --interactive    # terminal: ask per game, delete the ones you confirm
//   ./cleanup.scala --delete         # terminal: delete everything listed, no questions
//   ./cleanup.scala --steam-dir /path/to/Steam
//
// An app is considered installed if any Steam library has steamapps/appmanifest_<appid>.acf.
//
// Non-Steam shortcuts (from userdata/*/config/shortcuts.vdf) and appid 0 are always kept.
//
// Game names and Steam Cloud rules come from Steam's local caches; no network access.
// help:end

// Requires scala-cli (https://scala-cli.virtuslab.org/install); running without it shows that link (in a dialog
// when not started from a terminal).

import java.awt.GraphicsEnvironment
import java.nio.file.{Files, Paths}
import scala.util.Try

@main def run(rawArgs: String*): Unit =
  // Tells the shell bootstrap that scala-cli is done preparing, which closes its "Starting…" notification.
  sys.env.get("CLEANUP_STARTING_FILE").foreach(file => Try(Files.deleteIfExists(Paths.get(file))))
  val defaultSteamDir = SteamDir(Paths.get(sys.props("user.home"), ".local/share/Steam"))
  val args = parseArgs(rawArgs.toList, Args(defaultSteamDir, Mode.Gui, Theme.Auto))

  args.mode match
    case Mode.Cli(action) => runCli(args.steamDir, action)
    case Mode.Gui if GraphicsEnvironment.isHeadless =>
      System.err.println("No display found, so showing a dry run in the terminal. Pass --cli to skip this check.")
      runCli(args.steamDir, CliAction.DryRun)
    case Mode.Gui =>
      if !(needsNonReparentingRelaunch && relaunchNonReparenting(rawArgs)) then Gui.start(args.steamDir, args.theme)
