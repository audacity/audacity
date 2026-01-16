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
- **Qt 6.9.1+** with Desktop components and additional libraries:
  - Qt 5 Compatibility Module
  - Qt Network Authorization
  - Qt Shader Tools
  - Qt State Machines
- **Ninja** (recommended build generator)
- **Git** with submodules

### Building
```bash
# Clone with submodules
git clone --recurse-submodules https://github.com/audacity/audacity.git

# Configure (first time only)
cmake -S . -B build/ -G Ninja

# Build
cmake --build build/

# Install
cmake --install build/
```

### Quick Development Builds
- **Qt Creator**: Open `CMakeLists.txt`, configure with Qt kit, and build
- **Visual Studio (Windows)**: Run `generate_sln.bat` to generate solution
- **VSCode**: Open workspace file `.vscode/audacity.code-workspace`

## Architecture & Project Structure

### Key Directories
- **`/src/`** - Main Audacity 4 source code (C++ and QML)
  - `app/` - Application layer
  - `appshell/` - Application shell and startup
  - `au3wrap/` - Audacity 3 wrapper/integration
  - `effects/` - Audio effects
  - `playback/` - Playback engine
  - `project/` - Project management
  - `projectscene/` - Project UI/scene
  - `record/` - Recording functionality
  - `trackedit/` - Track editing
- **`/au3/`** - Audacity 3.x legacy code
  - `src/` - AU3 C++ source
  - `lib-src/` - Third-party libraries
  - `libraries/` - AU3 modular libraries
- **`/muse_framework/`** - MuseScore framework (submodule)
- **`/buildscripts/`** - Build and CI scripts
- **`/docs/`** - Documentation

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
The repository has a `.editorconfig` file at the root. IDEs should automatically apply these settings.

## Testing

### Unit Tests
- Tests are located in various directories:
  - `/au3/tests/` - Audacity 3 tests
  - Component-specific test directories (e.g., `/au3/libraries/*/tests/`)
- **Run tests**: Use CI workflow scripts or manual CMake test targets

### CI Workflows
- **Code Style**: `.github/workflows/au4_check_codestyle.yml`
  - Runs: `cmake -P ./buildscripts/ci/checkcodestyle/_deps/checkcodestyle.cmake ./src/`
- **Unit Tests**: `.github/workflows/au4_check_unit_tests.yml`
  - Builds with Qt 6.10.1+ on Ubuntu
  - Runs unit tests with ASAN
  - Optional code coverage
- **Platform Builds**: Linux, macOS, Windows workflows

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
- **CLA Required**: Contributors must [sign the CLA](https://www.audacityteam.org/cla/)
- **PR Template**: Use the pull request template in `.github/`
- **Review Process**: Changes are reviewed by the Audacity team
- **Communication**: Use [Audacity Dev Discord](https://discord.gg/N3XKxzTrq3) for help

### License
- **Primary License**: GPLv3
- Most code files are GPLv2-or-later
- Exceptions: `/au3/lib-src/` (third-party), VST3-related code
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
- Qt Creator provides the best QML debugging support
- For C++ debugging, Visual Studio (Windows) or GDB (Linux) work well
- Check `buildscripts/ci/` for CI debugging scripts

### Dependencies
- External dependencies are in `/au3/lib-src/`
- MuseScore framework provides many core utilities
- Qt modules are specified in CMake and CI workflows

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
- **Submodules**: Be aware of the MuseScore framework submodule
- **Branch Awareness**: Confirm whether changes target `master` (AU4) or `audacity3` (AU3.x)
- **Build Time**: Initial builds can be slow; use ccache if available
- **Qt Version**: Ensure Qt 6.9.1+ for compatibility

## File Patterns to Recognize

- `*.cpp`, `*.h` - C++ source/header files
- `*.qml` - QML UI files
- `CMakeLists.txt` - CMake build configuration
- `.clang-format` - C++ code formatting rules
- `*module.cpp` - Module initialization code
- `*types.h` - Type definitions
- `*configuration.h` - Configuration interfaces
