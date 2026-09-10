# Audacity test VST3 plugin — a VST3 plugin with a controllable load moment

A stereo effect whose _module load_ (`ModuleEntry` on Linux, `bundleEntry` on
macOS, i.e. the first thing a host does after loading the module) is driven by a
validation gate file. Use it to hold, release, or break plugin validation/loading
on purpose while testing the non-blocking plugin validation (#11746).

While processing it applies an amplitude tremolo (~5 Hz) whose depth is the
`Effect depth` parameter (default full). The parameter is persisted with the
project, so it doubles as a check that a plugin's saved settings survive a
reload. The effect being active vs bypassed (and any non-default depth) is
immediately audible.

The plugin builds on Linux and macOS (Windows isn't implemented; the controller
below still builds there).

## Build

    cmake -DAU_BUILD_VST3_TEST_PLUGIN=ON <build dir>
    ninja -C <build dir> au_vst3_test_plugin_validation_gate au_vst3_test_plugin_validation_controller
    ninja -C <build dir> deploy_test_vst3     # symlink/copy the bundle into your VST3 folder

The bundle is `<build dir>/test-plugins/AuVst3TestPlugin.vst3`:
`Contents/<arch>-linux/AuVst3TestPlugin.so` on Linux, a regular macOS bundle
(`Contents/MacOS/AuVst3TestPlugin` + `Info.plist`) on macOS, where the linker's
ad-hoc signature is all a locally loaded plugin needs.

Deploying is deliberately _not_ part of `install` (that option is meant to be ON
on CI so QA can find the built plugin, and `install` must not touch the agent's
VST3 folder). For the edit-then-F5 workflow, `.vscode/tasks.json` defines
`CMake: install + deploy test VST3`, which the Linux and macOS launch configs use
as their `preLaunchTask`: edit the plugin, press F5, Audacity picks up the rebuilt
plugin. The target is a harmless no-op when the plugin isn't built, so it never
breaks F5 for anyone else. (On Linux/macOS the deploy is a symlink into the build
output, so it's idempotent and rebuilds are live; on Windows it's a copy.)

### macOS notes

- The deploy symlinks the bundle into `~/Library/Audio/Plug-Ins/VST3`.
- `run audacity (MacOS, CodeLLDB, validate test VST3)` in `.vscode/launch.json`
  runs the plugin-validation subprocess itself (`--register-audio-plugin`) on the
  deployed bundle, so a crash while loading is caught in the debugger rather than
  only reported by the parent app.
- A crashing validation makes macOS show its "quit unexpectedly" dialog for the
  registration subprocess, once per crashing plugin; dismiss it, the parent app
  carries on. The report lands in `~/Library/Logs/DiagnosticReports`.
- The default install step runs `macdeployqt` and is slow; configure with
  `-DAU_INSTALL_QT_RUNTIME=OFF` for local builds to keep F5 quick.

## Getting it from CI (QA)

The Linux CI build has the plugin enabled and uploads the built bundle, the
controller (as a self-contained AppImage, so no Qt install is needed) and this
README as the `test-vst3-plugin-linux-<arch>` artifact of each `au4_build_linux`
run. Download it, drop `AuVst3TestPlugin.vst3` into `~/.vst3` (or run the controller
AppImage and use its Install button) and rescan plugins in Audacity. macOS CI
doesn't publish it; build it locally as above.

## Controller app

`<build dir>/test-plugins/au_vst3_test_plugin_validation_controller` is a small Qt (Widgets)
app so you never have to edit the validation gate file by hand. It

- **installs** the built bundle into the platform VST3 folder (`~/.vst3`,
  `~/Library/Audio/Plug-Ins/VST3`, `%COMMONPROGRAMFILES%\VST3`) - as a symlink on
  Linux/macOS so rebuilds stay live, a copy on Windows;
- sets the **load result**: succeed / crash / refuse / succeed-then-abort-at-exit, either _immediately_ or _after_
  a delay (default 180 s = the 3 min plugin-load timeout), by writing the validation gate file;
- shows the validation gate file's path and current content.

The controller and the plugin resolve the validation gate path identically, so they always agree.

## Validation gate file

`$AU_VST3_TEST_PLUGIN_VALIDATION_GATE_FILE`, or `<temp dir>/au_vst3_test_plugin_validation_gate`
(`/tmp/au_vst3_test_plugin_validation_gate` on Linux, `$TMPDIR/au_vst3_test_plugin_validation_gate`
on macOS, where `$TMPDIR` is the per-user `/var/folders/.../T/`). Contents are
`<code> [delaySeconds]`: the code below is applied after waiting the optional
delay (default 0), counted from when the load started. The file keeps being
polled meanwhile, so writing a new value overrides a pending one.

| value          | behaviour                                                       |
| -------------- | --------------------------------------------------------------- |
| `1` or no file | load normally                                                   |
| `0`            | wait, re-reading the file every 250 ms, until it changes        |
| `-1`           | crash (null dereference) while loading                          |
| `2`            | refuse to load (`ModuleEntry` / `bundleEntry` returns false)    |
| `3`            | load, then abort the host process when it exits                 |

While waiting it prints `[AuVst3TestPlugin] validation gate closed, waiting ...` to stderr about
once a second; the validation subprocess timeout is an _inactivity_ timeout, so
a waiting plugin isn't killed.

    GATE=/tmp/au_vst3_test_plugin_validation_gate      # Linux; macOS: GATE=$TMPDIR/au_vst3_test_plugin_validation_gate
    echo 0 > $GATE          # hold
    echo 1 > $GATE          # release
    echo -1 > $GATE         # crash on next load
    echo "1 180" > $GATE    # load, but only after 3 minutes
    echo "-1 180" > $GATE   # crash after 3 minutes
    echo 3 > $GATE          # load, then abort the process at exit

If Audacity and the controller might see different temp dirs (e.g. one launched
with `TMPDIR` unset), pin the path for both with `AU_VST3_TEST_PLUGIN_VALIDATION_GATE_FILE`.

`3` models a plugin (seen in the wild, wrapped in a copy-protection SDK) that starts
a worker thread on load and only cleans up in static destructors. The load succeeds
and the result is written, but if the process then unwinds through `exit()` a static
destructor destroys a mutex the worker still uses, the worker's next lock throws, and
the process aborts (SIGABRT). A host that lets this happen counts the validation as
failed (`Could not register plugin ... error code: -1`) and lists a plugin that
validated fine as broken; Audacity's registration subprocess ends with `_Exit` right
after writing the result, so it is immune (see `PluginRegistrationApp`). On Linux the
module pins itself in memory so the host's `dlclose` after discovery doesn't trigger
this early; on macOS the SDK host never unloads a bundle, so nothing extra is needed.
With the gate left at `3`, the in-process load makes Audacity itself abort on quit.

The validation gate applies to every process that loads the module: the validation
subprocess _and_ the in-process load in the app. Keep it at `1` once the plugin
has been validated unless you want the in-process load itself to hang/crash.
