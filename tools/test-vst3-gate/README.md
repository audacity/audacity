# Audacity test VST3 plugin — a VST3 plugin with a controllable load moment

A stereo effect whose *module load* (`ModuleEntry`, i.e. the first thing a host
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
    ninja -C <build dir> au_test_vst3_gate
    ln -s <build dir>/test-plugins/AuTestGate.vst3 ~/.vst3/AuTestGate.vst3

## Gate file

`$AU_TEST_VST3_GATE_FILE`, or `<temp dir>/au_test_vst3_gate` (`/tmp/au_test_vst3_gate`
on Linux). The plugin reads the first integer:

| value          | behaviour                                                     |
|----------------|---------------------------------------------------------------|
| `1` or no file | load normally                                                 |
| `0`            | wait, re-reading the file every 250 ms, until it changes      |
| `-1`           | crash (null dereference) while loading                        |
| `2`            | refuse to load (`ModuleEntry` returns false)                   |

While waiting it prints `[AuTestGate] gate closed, waiting ...` to stderr about
once a second; the validation subprocess timeout is an *inactivity* timeout, so
a waiting plugin isn't killed.

    echo 0 > /tmp/au_test_vst3_gate    # hold
    echo 1 > /tmp/au_test_vst3_gate    # release
    echo -1 > /tmp/au_test_vst3_gate   # crash on next load

The gate applies to every process that loads the module: the validation
subprocess *and* the in-process load in the app. Keep it at `1` once the plugin
has been validated unless you want the in-process load itself to hang/crash.
