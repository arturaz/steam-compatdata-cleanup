import java.awt.{BorderLayout, Color, Component, FlowLayout, Font}
import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import java.nio.file.{Files, Path}
import javax.swing.{AbstractAction, BorderFactory, BoxLayout, JButton, JComboBox, JComponent, JFrame, JLabel, JOptionPane,
  JPanel, JProgressBar, JScrollPane, JSplitPane, JTable, KeyStroke, SwingConstants, SwingUtilities, UIManager,
  WindowConstants}
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

final class MainWindow(steamDir: SteamDir, initialTheme: Theme):
  private var palette = Palette.current()

  private def riskColor(risk: Risk): Color = risk match
    case Risk.NoSaves => palette.dimmed
    case Risk.Safe    => palette.green
    case Risk.Unsure  => palette.yellow
    case Risk.Lost    => palette.red

  private def riskSummary(risk: Risk): String = risk match
    case Risk.NoSaves => "no saves found"
    case Risk.Safe    => "☁ saves in Steam Cloud"
    case Risk.Unsure  => "☁ cloud sync unconfirmed"
    case Risk.Lost    => "✗ saves not backed up"

  private val frame = JFrame("Steam leftovers cleanup")
  private val rows = ArrayBuffer.empty[GameRow]
  private var details = Vector.empty[Detail]
  private var scanned: Option[Scan] = None
  private var freed = Bytes.Zero
  private var busy = false

  private object gameModel extends AbstractTableModel:
    private val columns = Vector("", "Game", "Size", "Saves", "App ID")
    def getRowCount: Int = rows.size
    def getColumnCount: Int = columns.size
    override def getColumnName(column: Int): String = columns(column)
    override def getColumnClass(column: Int): Class[?] = column match
      case 0 => classOf[java.lang.Boolean]
      case 1 => classOf[String]
      case 3 => classOf[java.lang.Integer]
      case _ => classOf[java.lang.Long]
    override def isCellEditable(row: Int, column: Int): Boolean = column == 0 && !busy
    def getValueAt(row: Int, column: Int): Object =
      val o = rows(row).orphan
      column match
        case 0 => java.lang.Boolean.valueOf(rows(row).checked)
        case 1 => o.displayName
        case 2 => java.lang.Long.valueOf(o.size.unwrap)
        case 3 => java.lang.Integer.valueOf(o.risk.ordinal)
        case _ => java.lang.Long.valueOf(o.appId.unwrap)
    override def setValueAt(value: Object, row: Int, column: Int): Unit = value match
      case checked: java.lang.Boolean if column == 0 =>
        rows(row).checked = checked
        fireTableCellUpdated(row, column)
        refresh()
      case _ => ()

  private object detailModel extends AbstractTableModel:
    private val columns = Vector("Type", "Location", "Size", "Steam Cloud")
    def getRowCount: Int = details.size
    def getColumnCount: Int = columns.size
    override def getColumnName(column: Int): String = columns(column)
    def getValueAt(row: Int, column: Int): Object =
      val d = details(row)
      column match
        case 0 => d.kind
        case 1 => d.location
        case 2 => d.size.pretty
        case _ => d.cloud.fold("")(_.label)

  // Lazy: the models refer to this window, so they can only be handed out once it is constructed.
  private lazy val gameTable = JTable(gameModel)
  private lazy val detailTable = JTable(detailModel)
  private val title = JLabel("Scanning Steam libraries…")
  private val subtitle = JLabel(" ")
  private val status = JLabel(" ")
  private val progress = JProgressBar()
  private val selectSafeButton = JButton("Select safe")
  private val selectAllButton = JButton("Select all")
  private val selectNoneButton = JButton("Select none")
  private val openButton = JButton("Open folder")
  private val deleteButton = JButton("Delete selected…")
  private val themeBox = JComboBox[Theme](Theme.values)
  private val detailTitle = JLabel("Details of the selected game")
  private val dimLabels = ArrayBuffer[JLabel](subtitle)

  /** Fonts and colours set by hand; redone after a theme switch, since they don't come from the look and feel. */
  private def styleLabels(): Unit =
    val base = UIManager.getFont("Label.font")
    title.setFont(base.deriveFont(Font.BOLD, base.getSize2D * 1.5f))
    detailTitle.setFont(base.deriveFont(Font.BOLD))
    dimLabels.foreach(_.setForeground(palette.dimmed))

  private def switchTheme(): Unit =
    themeBox.getItemAt(themeBox.getSelectedIndex) match
      case null => ()
      case theme =>
        // Auto runs commands to read the desktop setting, so do that off the event thread.
        inBackground(theme == Theme.Auto && DesktopDarkMode.prefersDark) { dark =>
          Gui.applyTheme(theme, autoDark = dark.getOrElse(false))
          palette = Palette.current()
          SwingUtilities.updateComponentTreeUI(frame)
          styleLabels()
          frame.repaint()
        }

  /** A cell renderer that lets `style` adjust the label for the row (as a model index). */
  private def renderer(align: Int)(style: (JLabel, Int, Boolean) => Unit): DefaultTableCellRenderer =
    new DefaultTableCellRenderer:
      setHorizontalAlignment(align)
      override def getTableCellRendererComponent(
          table: JTable,
          value: Object,
          selected: Boolean,
          focused: Boolean,
          row: Int,
          column: Int
      ): Component =
        val _ = super.getTableCellRendererComponent(table, value, selected, focused, row, column)
        setToolTipText(null)
        style(this, table.convertRowIndexToModel(row), selected)
        this

  private def setupGameTable(): Unit =
    gameTable.setAutoCreateRowSorter(true)
    gameTable.setFillsViewportHeight(true)
    gameTable.setShowGrid(false)
    gameTable.setRowHeight((gameTable.getRowHeight * 1.4).toInt)
    val columns = gameTable.getColumnModel
    columns.getColumn(0).setMaxWidth(36)
    List(1 -> 380, 2 -> 90, 3 -> 220, 4 -> 90).foreach((i, w) => columns.getColumn(i).setPreferredWidth(w))

    columns.getColumn(1).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(o.kindNote.fold(o.displayName) { note =>
        s"<html>${escapeHtml(o.displayName)}&nbsp;&nbsp;<span style='color:${hex(palette.dimmed)}'>$note</span></html>"
      })
      if o.name.isEmpty && !selected then label.setForeground(palette.dimmed)
    })
    columns.getColumn(2).setCellRenderer(renderer(SwingConstants.TRAILING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(o.size.pretty)
      if o.size.isZero && !selected then label.setForeground(palette.dimmed)
    })
    columns.getColumn(3).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      val o = rows(row).orphan
      label.setText(riskSummary(o.risk))
      if !selected then label.setForeground(riskColor(o.risk))
      if o.saves.nonEmpty then
        label.setToolTipText(o.saves.map(s => s"${s.path} (${s.size.pretty}): ${s.cloud.label}").mkString("<html>", "<br>", "</html>"))
    })
    columns.getColumn(4).setCellRenderer(renderer(SwingConstants.TRAILING) { (label, row, selected) =>
      label.setText(s"#${rows(row).orphan.appId}")
      if !selected then label.setForeground(palette.dimmed)
    })

    gameTable.getSelectionModel.addListSelectionListener(e => if !e.getValueIsAdjusting then showDetails())
    gameTable.addMouseListener(new MouseAdapter:
      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 && gameTable.columnAtPoint(e.getPoint) != 0 then openSelected())
    // Space toggles the checkboxes of all selected rows.
    gameTable.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("SPACE"), "toggle-checked")
    gameTable.getActionMap.put("toggle-checked", new AbstractAction:
      override def actionPerformed(e: ActionEvent): Unit = if !busy then toggleSelected())

  private def setupDetailTable(): Unit =
    detailTable.setFillsViewportHeight(true)
    detailTable.setShowGrid(false)
    val columns = detailTable.getColumnModel
    List(0 -> 110, 1 -> 560, 2 -> 90, 3 -> 260).foreach((i, w) => columns.getColumn(i).setPreferredWidth(w))
    columns.getColumn(1).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, _) =>
      label.setToolTipText(details(row).path.toString)
    })
    columns.getColumn(2).setCellRenderer(renderer(SwingConstants.TRAILING)((_, _, _) => ()))
    columns.getColumn(3).setCellRenderer(renderer(SwingConstants.LEADING) { (label, row, selected) =>
      details(row).cloud.foreach(c => if !selected then label.setForeground(riskColor(c.risk)))
    })
    detailTable.addMouseListener(new MouseAdapter:
      override def mouseClicked(e: MouseEvent): Unit =
        val row = detailTable.rowAtPoint(e.getPoint)
        if e.getClickCount == 2 && row >= 0 then open(details(row).path))

  private def layout(): Unit =
    val hints = List(
      "Games whose saves aren't backed up start unchecked. Space toggles the selected rows; double-click opens a folder.",
      "Save detection is a heuristic: games that save elsewhere aren't covered, so check the details before deleting."
    ).map { text =>
      val hint = JLabel(text)
      dimLabels += hint
      hint
    }
    val header = JPanel()
    header.setLayout(BoxLayout(header, BoxLayout.Y_AXIS))
    header.setBorder(BorderFactory.createEmptyBorder(12, 16, 10, 16))
    (title :: subtitle :: hints).foreach(c => { val _ = header.add(c) })

    val detailPanel = JPanel(BorderLayout(0, 4))
    detailPanel.add(detailTitle, BorderLayout.NORTH)
    detailPanel.add(JScrollPane(detailTable), BorderLayout.CENTER)
    detailPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0))

    val split = JSplitPane(JSplitPane.VERTICAL_SPLIT, JScrollPane(gameTable), detailPanel)
    split.setResizeWeight(0.7)
    split.setBorder(BorderFactory.createEmptyBorder(0, 16, 0, 16))

    val buttons = JPanel(FlowLayout(FlowLayout.TRAILING, 6, 0))
    List(selectSafeButton, selectAllButton, selectNoneButton, openButton, deleteButton)
      .foreach(b => { val _ = buttons.add(b) })
    val left = JPanel(FlowLayout(FlowLayout.LEADING, 8, 0))
    left.add(JLabel("Theme:"))
    left.add(themeBox)
    left.add(status)
    left.add(progress)
    val footer = JPanel(BorderLayout())
    footer.setBorder(BorderFactory.createEmptyBorder(10, 16, 12, 16))
    footer.add(left, BorderLayout.CENTER)
    footer.add(buttons, BorderLayout.EAST)

    selectSafeButton.setToolTipText("Select games whose saves are in Steam Cloud, or that have no saves")
    selectSafeButton.addActionListener(_ => setChecked(_.group != SaveGroup.NotBackedUp))
    selectAllButton.addActionListener(_ => setChecked(_ => true))
    selectNoneButton.addActionListener(_ => setChecked(_ => false))
    openButton.addActionListener(_ => openSelected())
    deleteButton.addActionListener(_ => deleteChecked())
    themeBox.setSelectedItem(initialTheme)
    themeBox.setToolTipText("Auto follows the desktop's dark mode setting")
    themeBox.addActionListener(_ => switchTheme())
    styleLabels()

    val content = frame.getContentPane
    content.add(header, BorderLayout.NORTH)
    content.add(split, BorderLayout.CENTER)
    content.add(footer, BorderLayout.SOUTH)

  def show(): Unit =
    setupGameTable()
    setupDetailTable()
    layout()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setSize(1150, 760)
    frame.setLocationRelativeTo(null)
    setBusy(true)
    progress.setIndeterminate(true)
    refresh()
    frame.setVisible(true)
    inBackground(scan(steamDir)) {
      case Success(Right(result)) => loaded(result)
      case Success(Left(error))   => fatal(error)
      case Failure(e)             => fatal(s"Scanning failed: ${e.toString}")
    }

  private def fatal(message: String): Unit =
    JOptionPane.showMessageDialog(frame, message, frame.getTitle, JOptionPane.ERROR_MESSAGE)
    sys.exit(1)

  private def loaded(result: Scan): Unit =
    scanned = Some(result)
    rows ++= result.orphans.map(o => GameRow(o, checked = o.group != SaveGroup.NotBackedUp))
    gameModel.fireTableDataChanged()
    setBusy(false)
    if rows.nonEmpty then gameTable.setRowSelectionInterval(0, 0)

  private def setBusy(value: Boolean): Unit =
    busy = value
    progress.setVisible(value)
    gameTable.setEnabled(!value)
    refresh()

  /** Updates the header, footer and buttons from the current rows. */
  private def refresh(): Unit =
    scanned.foreach { s =>
      val total = totalSize(rows.map(_.orphan))
      title.setText(if rows.isEmpty then "Nothing to clean up" else s"Uninstalled games are using ${total.pretty}")
      subtitle.setText(s"${gameCount(rows.size)} · keeping ${plural(s.installedCount, "installed app")} and " +
        s"${plural(s.shortcutCount, "non-Steam shortcut")} · ${s.libraries.mkString(", ")}")
    }
    val checked = rows.filter(_.checked)
    if !busy then
      val freedText = if freed.isZero then "" else s"Freed ${freed.pretty} · "
      status.setText(s"$freedText${checked.size} of ${rows.size} selected · ${totalSize(checked.map(_.orphan)).pretty}")
    List(selectSafeButton, selectAllButton, selectNoneButton).foreach(_.setEnabled(!busy && rows.nonEmpty))
    openButton.setEnabled(!busy && selectedRows.nonEmpty)
    deleteButton.setEnabled(!busy && checked.nonEmpty)

  private def selectedRows: List[GameRow] =
    gameTable.getSelectedRows.toList.map(gameTable.convertRowIndexToModel).map(rows(_))

  private def showDetails(): Unit =
    details = selectedRows.headOption.fold(Vector.empty) { row =>
      val o = row.orphan
      (o.leftovers.map(l => Detail(l.kind.label, l.path.toString, l.path, l.size, None)) ++
        o.saves.map(s => Detail("Save", s.path, s.fullPath, s.size, Some(s.cloud)))).toVector
    }
    detailModel.fireTableDataChanged()
    refresh()

  private def setChecked(f: Orphan => Boolean): Unit =
    rows.foreach(row => row.checked = f(row.orphan))
    gameModel.fireTableRowsUpdated(0, rows.size - 1)
    refresh()

  private def toggleSelected(): Unit =
    val selected = selectedRows
    val value = !selected.forall(_.checked)
    selected.foreach(_.checked = value)
    gameModel.fireTableRowsUpdated(0, rows.size - 1)
    refresh()

  private def open(path: Path): Unit =
    openInFileManager(path).failed.foreach { e =>
      JOptionPane.showMessageDialog(frame, s"Could not open ${path.toString}: ${e.getMessage}", frame.getTitle,
        JOptionPane.ERROR_MESSAGE)
    }

  private def openSelected(): Unit = selectedRows.headOption.flatMap(_.orphan.leftovers.headOption).foreach(l => open(l.path))

  private def deleteChecked(): Unit =
    val chosen = rows.filter(_.checked).toList
    if chosen.isEmpty then return
    val risky = chosen.filter(_.orphan.group == SaveGroup.NotBackedUp).map(_.orphan.displayName)
    val warning =
      if risky.isEmpty then ""
      else
        val listed = risky.take(8).map(n => s"  • $n").mkString("\n")
        val more = if risky.size > 8 then s"\n  … and ${risky.size - 8} more" else ""
        s"\n\nThese games have saves that are NOT backed up. Deleting loses them for good:\n$listed$more"
    val message = s"Delete the leftovers of ${gameCount(chosen.size)} (${totalSize(chosen.map(_.orphan)).pretty})?$warning"
    val options = Array[Object]("Delete", "Cancel")
    val messageType = if risky.isEmpty then JOptionPane.QUESTION_MESSAGE else JOptionPane.WARNING_MESSAGE
    val answer = JOptionPane.showOptionDialog(frame, message, "Delete leftovers", JOptionPane.YES_NO_OPTION,
      messageType, null, options, options(1))
    if answer != 0 then return

    setBusy(true)
    progress.setIndeterminate(false)
    progress.setMaximum(chosen.size)
    progress.setValue(0)
    inBackground {
      chosen.zipWithIndex.map { (row, i) =>
        SwingUtilities.invokeLater { () =>
          progress.setValue(i)
          status.setText(s"Deleting ${row.orphan.displayName}…")
        }
        // A failed delete may still have removed part of the tree, so measure what is left.
        val results = deleteLeftovers(row.orphan).map {
          case (l, None)    => (l, None)
          case (l, Some(e)) => (l.copy(size = if Files.exists(l.path) then treeSize(l.path) else Bytes.Zero), Some(e))
        }
        row -> results
      }
    }(deleted)

  private def deleted(result: Try[List[(GameRow, List[(Leftover, Option[Exception])])]]): Unit =
    val outcomes = result match
      case Success(outcomes) => outcomes
      case Failure(e) =>
        JOptionPane.showMessageDialog(frame, s"Deleting failed: ${e.toString}", frame.getTitle, JOptionPane.ERROR_MESSAGE)
        Nil
    outcomes.foreach { (row, results) =>
      freed += results.collect { case (l, None) => l.size }.sum
      val remaining = results.collect { case (l, Some(_)) => l }
      if remaining.isEmpty then
        val _ = rows -= row
      else
        val keepsPrefix = remaining.exists(_.kind == LeftoverKind.Prefix)
        row.orphan = row.orphan.copy(leftovers = remaining, saves = if keepsPrefix then row.orphan.saves else Nil)
    }
    gameModel.fireTableDataChanged()
    setBusy(false)
    showDetails()
    val failures = for
      (_, results)      <- outcomes
      case (l, Some(e)) <- results
    yield s"${l.path.toString}: ${e.getMessage}"
    if failures.nonEmpty then
      val more = if failures.size > 10 then s"\n… and ${failures.size - 10} more" else ""
      JOptionPane.showMessageDialog(frame, s"Some leftovers could not be deleted:\n${failures.take(10).mkString("\n")}$more",
        frame.getTitle, JOptionPane.WARNING_MESSAGE)
