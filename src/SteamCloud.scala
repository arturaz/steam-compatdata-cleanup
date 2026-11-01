/** Steam Auto-Cloud roots → the matching folder inside a Proton prefix. */
val cloudRoots: Map[CloudRoot, UserFolder] = Map(
  "WinAppDataLocal"    -> "AppData/Local",
  "WinAppDataLocalLow" -> "AppData/LocalLow",
  "WinAppDataRoaming"  -> "AppData/Roaming",
  "WinMyDocuments"     -> "Documents",
  "WinSavedGames"      -> "Saved Games"
).map((root, folder) => CloudRoot(root) -> UserFolder(folder))

/** Last Steam Cloud sync state per app, across all local Steam users (userdata/<user>/config/localconfig.vdf). */
def cloudSyncStates(steamDir: SteamDir): Map[AppId, List[SyncState]] =
  steamDir.userdataDirs
    .flatMap(user => readTextVdf(user.resolve("config/localconfig.vdf")))
    .flatMap(_.block("UserLocalConfigStore", "Software", "Valve", "Steam", "apps").toList)
    .flatMap(_.childBlocks)
    .flatMap { (id, app) =>
      for
        appId <- AppId.parse(id.unwrap)
        state <- app.str("cloud", "last_sync_state")
      yield appId -> SyncState(state)
    }
    .groupMap(_._1)(_._2)

/** Decides whether a prefix entry (`folder`/`entry`) of an app is backed up by Steam Cloud. */
def cloudStatus(info: Option[Vdf.BinaryBlock], syncStates: Map[AppId, List[SyncState]], appId: AppId)(
    folder: UserFolder,
    entry: EntryName
): Cloud =
  info.flatMap(_.block("ufs")) match
    case None => Cloud.NoCloud
    case Some(ufs) =>
      val rules = ufs.block("savefiles").toList.flatMap(_.childBlocks).map(_._2)
      val covered = rules.exists { rule =>
        val forWindows = rule.block("platforms").forall(_.strings.exists(p => p == "Windows" || p == "all"))
        val ruleFolder = rule.str("root").map(CloudRoot(_)).flatMap(cloudRoots.get)
        val ruleEntry = rule.str("path").getOrElse("").split("[/\\\\]").headOption.filter(_.nonEmpty)
        forWindows && ruleFolder.contains(folder) && ruleEntry.forall(_.equalsIgnoreCase(entry.unwrap))
      }
      if !covered then Cloud.NotCovered
      else
        // Demos often sync into the full game's cloud storage.
        val cloudId = ufs.long("appidredirect").map(AppId(_)).getOrElse(appId)
        val states = syncStates.getOrElse(cloudId, Nil) ++ syncStates.getOrElse(appId, Nil)
        if states.contains(SyncState.Synchronized) then Cloud.Synced else Cloud.Unconfirmed(states.headOption)
