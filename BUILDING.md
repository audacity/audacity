# Building Audacity

## NOTE: These instructions are work-in-progress and will be finalized closer to release of Audacity 4.

## Requirements & Dependencies

* Git
* CMake
* A package manager (tested: Choco on Windows, homebrew on macOS)
* A CMake generator (tested: Ninja)
* A C++ compiler (tested: MSVC on Windows, g++ on Linux)
* Qt 6.2.4, with modules:
  * Qt Network Authentification
  * Qt 5 Compatibility
  * Qt State Machines

## Setup

As large parts of Audacity 4 are based on MuseScore Studio, the general setup steps from it are applicable here too:

1. [Set up a developer environment](https://github.com/musescore/MuseScore/wiki/Set-up-developer-environment)
2. [Install Qt and Qt Creator](https://github.com/musescore/MuseScore/wiki/Install-Qt-and-Qt-Creator) (with Qt version 6.2.4; not the latest version)

### Get the Audacity source

To get the source, `git clone https://github.com/audacity/audacity.git` in the folder of your choice. See the [Github help](https://docs.github.com/en/get-started/getting-started-with-git/about-remote-repositories) for more information. 

### Get dependencies

If you haven't installed the dependencies listed above, now is the time to do so. 

Ninja should be able to handle the other dependencies, if it doesn't, the list may be inferred from the "setup" file in buildscripts/ci/{your OS}/. 

NB: At the moment, the list is rather long due to MuseScore dependencies that have not yet been cleaned up.

### Add relevant tools to PATH

Git, CMake, Ninja, Package manager, Compiler and Qt should all be added to the PATH variable in your OS. Otherwise, you'll need to specify them in the CMakeCache later on.

## Compiling

Using QtCreator to edit and compile the code is generally preferred as, perhaps unsurprisingly, knows most things about Qt.

To compile, just open CMakeLists.txt with QtCreator, configure the project with the auto-detected Qt 6.2.4 kit, and hit Build.

Otherwise, standard cmake building applies: 

```
# inside the audacity source:
cmake -S . -B build/ [options]  # configure (first build only)
cmake --build build/            # build (every build)
cmake --install build/          # install (every successful build)
```
