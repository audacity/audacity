# Building Audacity

## NOTE: These instructions are work-in-progress and will be finalized closer to release of Audacity 4.

## Requirements & Dependencies

* Git
* CMake
* A package manager (tested: Choco on Windows, homebrew on macOS)
* A CMake generator (tested: Ninja)
* A C++ compiler (tested: MSVC on Windows, g++ on Linux)
* Qt 6.2.4, with modules:
  * Qt Network Authorization
  * Qt 5 Compatibility Module
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

### With QtCreator

Using QtCreator to edit and compile the will provide the best intellisense and debugging support when interacting with QML. Nevertheless, debugging on Windows is slow, and if you mostly interact with the C++ code, you might want to other options, such as those listed below.

To compile, just open CMakeLists.txt with QtCreator, configure the project with the auto-detected Qt 6.2.4 kit, and hit Build.

### From the command line

Standard cmake building applies: 

```
# inside the audacity source:
cmake -S . -B build/ [options]  # configure (first build only)
cmake --build build/            # build (every build)
cmake --install build/          # install (every successful build)
```

### With Visual Studio (Windows only)

Double-click the `generate_sln.bat` script in the root of the repository. This will generate a Visual Studio solution in the `build` directory and build the `install` target. Open the generated solution (./build/audacity.sln) in Visual Studio and press F5 to run Audacity.

### With VSCode (Windows only)

TODO: generalize to other OSes

Note: the default generator is Ninja. Have it installed, or change the "cmake.generator" value in `.vscode/settings.json` to something else (eg `Visual Studio 16 2019`).

#### Open the workspace
One way of doing this is by executing Ctrl+Shift+P, and choosing "Open Workspace from File". Navigate to the root of the repository and select the `.vscode/audacity.code-workspace` file. Else, you can open the folder, then the workspace file, and click the "Open Workspace" button.

#### Install recommended extensions
When opening the repository in VSCode, you will be prompted to install recommended extensions. You can do it then. If you missed it, you can install them later by executing Ctrl+Shift+P, typing `Extensions: Show Recommended Extensions`, and installing the recommended extensions.

You should only have to do this once.

##### Configure and build install target

Execute Ctrl+Shift+P and choose "CMake: Configure".

#### Build and run audacity

Just **press F5**. It will build and install everything the first time, but afterwards it should be very fast, especially if you're using Ninja `:)`.

