# Copilot Instructions for Audacity

## Repository Overview

Audacity is an open-source, multi-track audio editor and recorder. This repository is currently undergoing a major transition to **Audacity 4**, which includes:
- An entirely new UI based on Qt/QML
- Integration with the MuseScore framework
- Significant architectural refactoring

The `master` branch contains Audacity 4 development. For Audacity 3.x work, use the `audacity3` branch.

## Build System

- **Build System**: CMake (minimum 3.24)
- **C++ Standard**: C++17
- **Qt Version**: Qt 6.9.1 or higher (CI uses Qt 6.10.1)
- **Required Qt modules**: Core5Compat, NetworkAuth, ShaderTools, and others
- **Build Generator**: Ninja (recommended)
- **CMake Presets**: Available in `CMakePresets.json` (debug, asan, release)

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
- **Languages**: C++17, QML
- **UI Framework**: Qt 6+ with QML
- **Base Framework**: MuseScore framework
- **Build System**: CMake
- **Legacy Integration**: Audacity 3.x code wrapped and integrated

## Coding Standards & Conventions

- Use **C++17** features appropriately
- **Indent**: 4 spaces (no tabs, except Makefiles)
- **Encoding**: UTF-8
- **Line endings**: LF (Unix-style), except `.bat`/`.ps1` files (CRLF)
- Use meaningful variable and function names
- Prefer modern C++ idioms (smart pointers, RAII, etc.)

## Testing

- Tests are located in various directories:
  - `/au3/tests/` - Audacity 3 tests
  - Component-specific test directories (e.g., `/au3/libraries/*/tests/`)
  - Module-specific test directories (e.g., `/src/context/tests/`, `/src/playback/tests/`, `/src/projectscene/tests/`, `/src/record/tests/`, `/src/trackedit/tests/`)
- Write tests for new features and bug fixes when applicable
- Follow existing test patterns in the codebase

## Important Development Notes

- The `master` branch is under heavy development
- Not all features are stable or fully documented
- Structural changes are frequent
- Some MuseScore dependencies are still being cleaned up
- **License**: GPLv3 (most code files are GPLv2-or-later)

## Common Development Tasks

### Working with Qt/QML
- QML files are in `qml/` subdirectories within modules
- C++ models/controllers are in corresponding module directories
- Follow Qt best practices for model-view separation

### Module Pattern
- Module initialization code in `*module.cpp`, `*module.h` files
- Type definitions in `*types.h` files
- Configuration interfaces in `*configuration.h` files
- Interface definitions use `i*.h` pattern (e.g., `ieffectsprovider.h`, `iplayback.h`)

### Dependencies
- External dependencies are in `/au3/lib-src/` (libnyquist, libsoxr, portmixer, soundtouch, sqlite, etc.)
- MuseScore framework provides core utilities (located in `/muse_framework/`)
- Core Qt modules: Core, Gui, Widgets, Network, Qml, Quick, QuickControls2, QuickWidgets, ShaderTools, Xml, Svg, Core5Compat, NetworkAuth, PrintSupport
