# steam-compatdata-cleanup

When you uninstall a game, Steam on Linux leaves its Proton prefix (`steamapps/compatdata/<appid>`) and shader cache
(`steamapps/shadercache/<appid>`) behind. These can add up to many gigabytes. This script finds the leftovers for
games that are no longer installed and removes them. It also warns you about save files that Steam Cloud has not
backed up.

## Requirements

- [scala-cli](https://scala-cli.virtuslab.org/install). It downloads Scala, a JVM and the dependencies on the first
  run. If scala-cli is missing, the script prints the install link and exits.
- Linux with native Steam at `~/.local/share/Steam`, or another Steam directory passed with `--steam-dir`.

## Usage

```sh
./cleanup.scala                  # dry run: list what would be deleted
./cleanup.scala --interactive    # ask per game, delete the ones you confirm (also -i)
./cleanup.scala --delete         # delete everything listed, no questions
./cleanup.scala --steam-dir /path/to/Steam
```

The script defaults to a dry run, so run it without arguments first and review the list.

In interactive mode you answer `y` (delete), `N` (skip, default), `a` (delete this and all remaining) or `q` (quit).

## What gets deleted

An app counts as installed if any Steam library (from `steamapps/libraryfolders.vdf`) has
`steamapps/appmanifest_<appid>.acf`. The script lists every `compatdata` or `shadercache` directory whose app is not
installed. It always keeps:

- non-Steam shortcuts (from `userdata/*/config/shortcuts.vdf`),
- appid `0`.

If no appmanifests are found at all, the script refuses to continue, since everything would look orphaned.

## Save detection

The script looks for likely save data in each prefix under `drive_c/users/steamuser`: `AppData/Local`,
`AppData/LocalLow`, `AppData/Roaming`, `Documents` and `Saved Games`. It skips entries that Wine and Proton create
themselves. For each save it checks the game's Steam Auto-Cloud rules and the last sync state, and puts the game in one
of three groups:

- **✗ Saves not backed up**: deleting the prefix loses these saves for good. Back them up first if you care.
- **☁ Saves backed up by Steam Cloud**: Steam restores them on reinstall.
- **✓ No save data found**

Interactive mode asks about the not-backed-up games first.

Save detection is a heuristic. Games that keep saves elsewhere, for example in the install directory or under
Steam's `userdata`, are not covered. Check the dry run before you delete.

Game names and cloud information come from Steam's local caches (`appcache/appinfo.vdf`,
`userdata/*/config/localconfig.vdf`). The script makes no network requests.
