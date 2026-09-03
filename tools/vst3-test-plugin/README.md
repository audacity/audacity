# Audacity test VST3 plugin — a VST3 plugin with a controllable load moment

A stereo effect whose _module load_ (`ModuleEntry`, i.e. the first thing a host
does after `dlopen`) is driven by a gate file. Use it to hold, release, or break
plugin validation/loading on purpose while testing the non-blocking plugin
validation (#11746).

While processing it applies an amplitude tremolo (~5 Hz) whose depth is the
`Effect depth` parameter (default full). The parameter is persisted with the
project, so it doubles as a check that a plugin's saved settings survive a
reload. The effect being active vs bypassed (and any non-default depth) is
immediately audible.

## Build

    cmake -DAU_BUILD_TEST_VST3_GATE_PLUGIN=ON <build dir>
    ninja -C <build dir> au_test_vst3_gate au_test_vst3_gate_controller
    ninja -C <build dir> deploy_test_vst3     # symlink/copy the bundle into your VST3 folder

Deploying is deliberately _not_ part of `install` (that option is meant to be ON
on CI so QA can find the built plugin, and `install` must not touch the agent's
VST3 folder). For the edit-then-F5 workflow, `.vscode/tasks.json` defines
`CMake: install + deploy test VST3`, which the Linux launch configs use as their
`preLaunchTask`: edit the plugin, press F5, Audacity picks up the rebuilt plugin.
The target is a harmless no-op when the plugin isn't built, so it never breaks
F5 for anyone else. (On Linux/macOS the deploy is a symlink into the build
output, so it's idempotent and rebuilds are live; on Windows it's a copy.)

## Getting it from CI (QA)

The Linux CI build has the plugin enabled and uploads the built bundle, the
controller (as a self-contained AppImage, so no Qt install is needed) and this
README as the `test-vst3-plugin-linux-<arch>` artifact of each `au4_build_linux`
run. Download it, drop `AuTestGate.vst3` into `~/.vst3` (or run the controller
AppImage and use its Install button) and rescan plugins in Audacity.

## Controller app

`<build dir>/test-plugins/au_test_vst3_gate_controller` is a small Qt (Widgets)
app so you never have to edit the gate file by hand. It

- **installs** the built bundle into the platform VST3 folder (`~/.vst3`,
  `~/Library/Audio/Plug-Ins/VST3`, `%COMMONPROGRAMFILES%\VST3`) - as a symlink on
  Linux/macOS so rebuilds stay live, a copy on Windows;
- sets the **load result**: succeed / crash / refuse, either _immediately_ or _after_
  a delay (default 180 s = the 3 min plugin-load timeout), by writing the gate file;
- shows the gate file's path and current content.

The controller and the plugin resolve the gate path identically, so they always agree.

## Gate file

`$AU_TEST_VST3_GATE_FILE`, or `<temp dir>/au_test_vst3_gate` (`/tmp/au_test_vst3_gate`
on Linux). Contents are `<code> [delaySeconds]`: the code below is applied after
waiting the optional delay (default 0), counted from when the load started. The
file keeps being polled meanwhile, so writing a new value overrides a pending one.

| value          | behaviour                                                |
| -------------- | -------------------------------------------------------- |
| `1` or no file | load normally                                            |
| `0`            | wait, re-reading the file every 250 ms, until it changes |
| `-1`           | crash (null dereference) while loading                   |
| `2`            | refuse to load (`ModuleEntry` returns false)             |

While waiting it prints `[AuTestGate] gate closed, waiting ...` to stderr about
once a second; the validation subprocess timeout is an _inactivity_ timeout, so
a waiting plugin isn't killed.

    echo 0 > /tmp/au_test_vst3_gate       # hold
    echo 1 > /tmp/au_test_vst3_gate       # release
    echo -1 > /tmp/au_test_vst3_gate      # crash on next load
    echo "1 180" > /tmp/au_test_vst3_gate # load, but only after 3 minutes
    echo "-1 180" > /tmp/au_test_vst3_gate# crash after 3 minutes

The gate applies to every process that loads the module: the validation
subprocess _and_ the in-process load in the app. Keep it at `1` once the plugin
has been validated unless you want the in-process load itself to hang/crash.
