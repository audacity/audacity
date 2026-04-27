# Audacity (AU4) — Build Instructions

This project uses CMake presets. User-local config lives in `CMakeUserPresets.json` and `.vscode/settings.json` (both gitignored). See `BUILDING.md` for full setup details.

## Build

```sh
cmake --preset default
cmake --build --preset build
cmake --build --preset install   # installs to build/<preset>/install/
```

On Windows with the Ninja generator, run from a **VS Developer Command Prompt** (or source `vcvars64.bat` first). The VS Code CMake Tools extension handles this automatically via `cmake.useVsDeveloperEnvironment`.

The executable is `build/<preset>/install/bin/Audacity4` (`.exe` on Windows).

## Known Issues

1. **Never use the `Debug` build type.** The `libcurl` debug artifact is missing from `muse_deps`. Use `RelWithDebInfo` or `Release`.
2. **Always set `CMAKE_INSTALL_PREFIX` to a subdirectory** (e.g. `build/install`). The repo root contains a file named `INSTALL` which collides with CMake's install directory on case-insensitive filesystems.
3. **Ninja on Windows requires the VS Developer environment.** Without it, `rc.exe` and `mt.exe` are missing and configure fails.
