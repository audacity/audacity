# Copilot Instructions for Audacity

## Repository Overview

Audacity is an open-source, multi-track audio editor and recorder. This repository is currently undergoing a major transition to **Audacity 4**, which includes:
- An entirely new UI based on Qt/QML
- Integration with the MuseScore framework
- Significant architectural refactoring

The `master` branch contains Audacity 4 development. For Audacity 3.x work, use the `audacity3` branch.

## Build System & Setup

### Prerequisites
- **CMake** (minimum 3.24)
- **C++ Compiler** with C++17 support
- **Qt 6.2.4 minimum** (Qt 6.9.1 or higher recommended) with Desktop components and additional libraries:
  - Qt 5 Compatibility Module (Core5Compat)
  - Qt Network Authorization (NetworkAuth)
  - Qt Shader Tools (ShaderTools)
  - Note: CI uses Qt 6.10.1 with additional modules: qt5compat, qtnetworkauth, qtshadertools, qtwebsockets, qtgraphs, qtquick3d
- **Ninja** (recommended build generator)
- **Git** with submodules

### Building
```bash
# Clone with submodules
git clone --recurse-submodules https://github.com/audacity/audacity.git

# Configure (first time only)
cmake -S . -B build/ -G Ninja

# Or use CMake presets (defined in CMakePresets.json):
# cmake --preset audacity-debug      # Debug build
# cmake --preset audacity-asan       # Debug with AddressSanitizer
# cmake --preset audacity-release    # Release with debug info

# Build
cmake --build build/

# Install
cmake --install build/
```

### Quick Development Builds
- **Qt Creator**: Open `CMakeLists.txt`, configure with Qt kit, and build
- **Visual Studio (Windows)**: Run `generate_sln.bat` in the repository root to generate solution in `./build/audacity.sln`
- **VSCode**: Open workspace file `.vscode/audacity.code-workspace` (note: BUILDING.md indicates this is currently Windows-only, but workspace file exists and should work on all platforms)

## Architecture & Project Structure

### Key Directories
- **`/src/`** - Main Audacity 4 source code (C++ and QML)
  - `app/` - Application layer
  - `appshell/` - Application shell and startup
  - `au3wrap/` - Audacity 3 wrapper/integration
  - `au3audio/` - AU3 audio integration
  - `audio/` - Audio engine
  - `context/` - Global context management
  - `effects/` - Audio effects (builtin, VST, LV2, Audio Unit, Nyquist, Vamp)
  - `importexport/` - Import/export functionality
  - `playback/` - Playback engine
  - `preferences/` - Preferences/settings
  - `project/` - Project management
  - `projectscene/` - Project UI/scene
  - `record/` - Recording functionality
  - `spectrogram/` - Spectrogram visualization
  - `toast/` - Toast notifications
  - `trackedit/` - Track editing
  - `uicomponents/` - Reusable UI components
  - `stubs/` - Stub implementations for testing
- **`/au3/`** - Audacity 3.x legacy code
  - `src/` - AU3 C++ source
  - `lib-src/` - Third-party libraries (libnyquist, libsoxr, portmixer, soundtouch, sqlite, etc.)
  - `libraries/` - AU3 modular libraries (70+ libraries with au3- prefix)
- **`/muse_framework/`** - MuseScore framework (submodule)
- **`/buildscripts/`** - Build and CI scripts
- **`/docs/`** - Documentation (currently minimal, contains effect-view-architecture.md)
- **`/share/`** - Shared resources (icons, locale, workspaces, autobotscripts)

### Technologies
- **Languages**: C++17, QML, Python (scripts)
- **UI Framework**: Qt 6+ with QML
- **Base Framework**: MuseScore framework
- **Build System**: CMake
- **Legacy Integration**: Audacity 3.x code wrapped and integrated

## Coding Standards & Conventions

### General Guidelines
- Follow the [Audacity Coding Standards](https://audacity.gitbook.io/dev/getting-started/coding-standards)
- Use **C++17** features appropriately
- **Indent**: 4 spaces (no tabs, except Makefiles)
- **Encoding**: UTF-8
- **Line endings**: LF (Unix-style), except `.bat`/`.ps1` files (CRLF)
- Trim trailing whitespace (except in Markdown)
- Insert final newline in files

### Code Style
- **Formatting**: Some subdirectories have `.clang-format` files
- Use meaningful variable and function names
- Prefer modern C++ idioms (smart pointers, RAII, etc.)
- Comment only when necessary for clarity

### EditorConfig
The repository has a `.editorconfig` file at the root. IDEs should automatically apply these settings:
- 4 spaces for most files (Python, shell scripts)
- 2 spaces for YAML/JSON files
- Tab indentation for Makefiles
- UTF-8 encoding, LF line endings (except `.bat`/`.ps1` which use CRLF)
- Trim trailing whitespace (except Markdown)
- Insert final newline

## Testing

### Unit Tests
- Tests are located in various directories:
  - `/au3/tests/` - Audacity 3 tests
  - Component-specific test directories (e.g., `/au3/libraries/*/tests/`)
  - Module-specific test directories (e.g., `/src/context/tests/`, `/src/playback/tests/`, `/src/projectscene/tests/`, `/src/record/tests/`, `/src/trackedit/tests/`)
- **Run tests**: Use CI workflow scripts or manual CMake test targets

### CI Workflows
- **Code Style**: `.github/workflows/au4_check_codestyle.yml`
  - Runs on Ubuntu 24.04
  - Executes: `cmake -P ./buildscripts/ci/checkcodestyle/_deps/checkcodestyle.cmake ./src/`
  - Checks only `/src/` directory for AU4 code style compliance
- **Unit Tests**: `.github/workflows/au4_check_unit_tests.yml`
  - Runs on Ubuntu 22.04
  - Builds with Qt 6.10.1
  - Runs unit tests with ASAN (AddressSanitizer)
  - Optional code coverage (enabled on scheduled runs every Thursday at 03:00)
- **Platform Builds**:
  - Linux: Ubuntu 22.04, Qt 6.10.1 (`.github/workflows/au4_build_linux.yml`)
  - macOS: Xcode 15.2, Qt 6.10.1 (`.github/workflows/au4_build_macos.yml`)
  - Windows: MSVC 2022, Qt 6.10.1 (`.github/workflows/au4_build_windows.yml`)
  - All scheduled daily at 03:00 UTC

### Testing Guidelines
- Write tests for new features and bug fixes when applicable
- Follow existing test patterns in the codebase
- Ensure tests are reproducible

## Important Development Notes

### Current State (Audacity 4)
- The `master` branch is under heavy development
- Not all features are stable or fully documented
- Structural changes are frequent
- Some MuseScore dependencies are still being cleaned up

### Contributing
- **CLA Required**: Contributors of non-trivial code (more than just a line or two) must [sign the CLA](https://www.audacityteam.org/cla/)
- **PR Template**: Use the pull request template at `.github/pull_request_template.md`
- **Review Process**: Changes are reviewed by the Audacity team
- **Communication**: Use [Audacity Dev Discord](https://discord.gg/N3XKxzTrq3) for help

### License
- **Primary License**: GPLv3
- Most code files are GPLv2-or-later (default license where no other license is specified)
- Exceptions: `/au3/lib-src/` (third-party libraries), VST3-related code
- Documentation: CC-BY 3.0

## Common Tasks

### Adding a New Feature
1. Familiarize yourself with the relevant modules in `/src/`
2. Follow the existing architectural patterns
3. Add tests if applicable
4. Update documentation if user-facing
5. Ensure code style compliance

### Working with Qt/QML
- QML files are in `qml/` subdirectories within modules
- C++ models/controllers are in corresponding module directories
- Follow Qt best practices for model-view separation

### Debugging
- **Qt Creator**: Provides the best QML debugging support
- **Visual Studio (Windows)**: Good C++ debugging with `.vscode/qt6.natvis` visualizers
- **GDB/LLDB (Linux/macOS)**: Use with VSCode launch configurations in `.vscode/launch.json`
- **VSCode**: Launch configurations available for Windows (cdb), macOS (lldb), and Linux (gdb)
- Check `buildscripts/ci/` for CI debugging scripts

### Dependencies
- External dependencies are in `/au3/lib-src/` (third-party libraries like libnyquist, libsoxr, portmixer, soundtouch, sqlite, etc.)
- MuseScore framework provides many core utilities (located in `/muse_framework/`)
- Qt modules are specified in `buildscripts/cmake/SetupQt6.cmake` and CI workflows
- Core Qt modules used: Core, Gui, Widgets, Network, Qml, Quick, QuickControls2, QuickWidgets, ShaderTools, Xml, Svg, Core5Compat, NetworkAuth, PrintSupport

## Additional Resources

- **Website**: https://www.audacityteam.org
- **Developer Resources**: https://audacity.gitbook.io/dev/
- **Forum**: https://forum.audacityteam.org/
- **Discord**: https://discord.gg/audacity
- **YouTube**: https://youtube.com/@audacity

## CI/CD & Automation

### Continuous Integration
- All PRs trigger code style checks and builds
- Unit tests run on PRs
- Code coverage is tracked (scheduled runs)
- Multi-platform builds ensure cross-platform compatibility

### Build Artifacts
- Development builds available in GitHub Actions
- Artifacts are tied to specific PRs or commits

## Tips for Copilot

- **Minimal Changes**: Make surgical, targeted changes
- **Test Early**: Run code style checks and builds frequently
- **Context Matters**: Audacity 4 is a hybrid of new Qt/QML UI and legacy AU3 code
- **Submodules**: Be aware of the MuseScore framework submodule at `/muse_framework/`
- **Branch Awareness**: Confirm whether changes target `master` (AU4) or `audacity3` (AU3.x)
- **Build Time**: Initial builds can be slow; use ccache if available
- **Qt Version**: Minimum Qt 6.2.4 required (Qt 6.9.1+ recommended, CI uses Qt 6.10.1)

## File Patterns to Recognize

- `*.cpp`, `*.h` - C++ source/header files
- `*.qml` - QML UI files
- `*.qrc` - Qt resource files
- `CMakeLists.txt` - CMake build configuration
- `.clang-format` - C++ code formatting rules
- `*module.cpp`, `*module.h` - Module initialization code
- `*types.h` - Type definitions
- `*configuration.h` - Configuration interfaces
- `i*.h` - Interface definitions (e.g., `ieffectsprovider.h`, `iplayback.h`)
- `*.bat` - Windows batch scripts (CRLF line endings)
- `*.sh` - Shell scripts (LF line endings)
- `*.cmake` - CMake scripts and modules
