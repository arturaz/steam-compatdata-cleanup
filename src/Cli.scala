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

val sizeW = 10

/** Prints one section of games with a heading; `nameW` keeps size columns aligned across sections. */
def printSection(heading: String, games: List[Orphan], nameW: Int): Unit =
  if games.isEmpty then return
  println(heading)
  games.foreach { o =>
    val size = if o.size.isZero then dim(o.size.pretty) else o.size.pretty
    val name = if o.name.isEmpty then dim(o.displayName) else bold(o.displayName)
    val note = o.kindNote.map(n => s"  ${dim(n)}").getOrElse("")
    println(s"    ${padRight(name, nameW)}  ${padLeft(size, sizeW)}  ${dim(s"#${o.appId}")}$note")
    o.saves.zipWithIndex.foreach { (save, i) =>
      val branch = if i == o.saves.length - 1 then "└─" else "├─"
      val color = save.cloud.risk match
        case Risk.Safe                 => green
        case Risk.Unsure               => yellow
        case Risk.Lost | Risk.NoSaves  => red
      println(s"      ${padRight(s"$branch ${save.path}", nameW - 2)}  ${padLeft(save.size.pretty, sizeW)}  ${color(save.cloud.label)}")
    }
  }
  println()

/** Prints the kept apps, so it's clear where the space that cannot be cleaned up goes. */
def printKept(kept: List[Kept], nameW: Int): Unit =
  if kept.isEmpty then return
  println(bold("Kept: installed games and non-Steam shortcuts") +
    dim(s" (${plural(kept.size, "app")}, ${totalSize(kept).pretty}) — never deleted"))
  kept.foreach { k =>
    val size = if k.size.isZero then dim(k.size.pretty) else k.size.pretty
    val name = if k.name.isEmpty then dim(k.displayName) else k.displayName
    val notes = (Option.when(k.reason == KeepReason.Shortcut)(k.reason.label) ++ k.kindNote).mkString(", ")
    val note = if notes.isEmpty then "" else s"  ${dim(notes)}"
    println(s"    ${padRight(name, nameW)}  ${padLeft(size, sizeW)}  ${dim(s"#${k.appId}")}$note")
  }
  println()

def delete(o: Orphan): Bytes =
  deleteLeftovers(o).map {
    case (l, None) =>
      println(s"    ${green("✓")} deleted ${dim(l.path.toString)}")
      l.size
    case (l, Some(e)) =>
      System.err.println(s"    ${red("✗")} failed to delete ${l.path.toString}: ${e.getMessage}")
      Bytes.Zero
  }.sum

enum Answer derives CanEqual:
  case Yes, No, All, Quit

def ask(o: Orphan): Answer =
  val warning =
    if o.saves.isEmpty then ""
    else if o.backedUp then green(" — saves are in Steam Cloud")
    else red(" — saves NOT backed up!")
  print(s"Delete ${bold(o.displayName)} (${o.size.pretty})$warning ${dim("[y/N/a(ll)/q(uit)]")} ")
  Console.flush()
  Option(scala.io.StdIn.readLine()).map(_.trim.toLowerCase) match
    case None               => Answer.Quit // EOF
    case Some("y" | "yes")  => Answer.Yes
    case Some("a" | "all")  => Answer.All
    case Some("q" | "quit") => Answer.Quit
    case Some(_)            => Answer.No

def runCli(steamDir: SteamDir, action: CliAction): Unit =
  val result = scan(steamDir) match
    case Left(error) =>
      System.err.println(error)
      sys.exit(1)
    case Right(result) => result
  println(dim(s"Steam libraries: ${result.libraries.mkString(", ")}"))
  println(dim(s"Keeping ${plural(result.installedCount, "installed app")} and " +
    s"${plural(result.shortcutCount, "non-Steam shortcut")}."))

  val orphans = result.orphans // display order = interactive order
  val nameW = (result.kept.map(k => displayWidth(k.displayName)) ++ orphans.map(o => displayWidth(o.displayName)) ++
    orphans.flatMap(_.saves.map(s => displayWidth(s.path) + 5))).maxOption.getOrElse(0).min(60)
  if orphans.isEmpty then
    println(green("\nNothing to clean up.\n"))
    printKept(result.kept, nameW)
    return

  def inGroup(g: SaveGroup) = orphans.filter(_.group == g)
  val notBackedUp = inGroup(SaveGroup.NotBackedUp)
  val backedUp = inGroup(SaveGroup.BackedUp)
  val noSaves = inGroup(SaveGroup.NoSaves)
  def summary(os: List[Orphan]) = s"${gameCount(os.size)}, ${totalSize(os).pretty}"

  println()
  println(bold(s"Uninstalled games are using ${totalSize(orphans).pretty}") + dim(s" (${gameCount(orphans.size)})"))
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
  printKept(result.kept, nameW)

  action match
    case CliAction.DryRun =>
      println(s"${cyan("Dry run")} — nothing deleted. Use ${bold("--interactive")} to choose per game, " +
        s"or ${bold("--delete")} to remove all.")
    case CliAction.Delete =>
      val freed = orphans.map(delete).sum
      println(green(s"\nFreed ${freed.pretty}."))
    case CliAction.Interactive =>
      var freed = Bytes.Zero
      var all = false
      var quit = false
      val it = orphans.iterator
      while it.hasNext && !quit do
        val o = it.next()
        val answer = if all then Answer.Yes else ask(o)
        answer match
          case Answer.Yes  => freed += delete(o)
          case Answer.All  => all = true; freed += delete(o)
          case Answer.No   => ()
          case Answer.Quit => quit = true
      println(green(s"\nFreed ${freed.pretty}."))
