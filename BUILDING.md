# Building Sneedacity

## Prerequisites

* **python3** >= 3.5
* **conan** >= 1.32.0
* **cmake** >= 3.16
* A working C++ 14 compiler

### Conan

[The best way to install Conan is `pip`.](https://docs.conan.io/en/latest/installation.html)

To install Conan on Windows:

```
$ pip install conan
```

To install Conan on macOS and Linux:

```
$ sudo pip3 install conan
```

Alternatively, on macOS, Conan is available from `brew`.

### CMake

On Windows, please use the [prebuilt binaries](https://cmake.org/download/).

On macOS, the easiest way to install CMake is `brew install cmake`.

On Linux, `cmake` is usually available from the system package manager.

### Windows

We build Sneedacity using [Microsoft Visual Studio 2019](https://visualstudio.microsoft.com/vs/community/). In order to build Sneedacity **Desktop development with C++** workload is required.

As we require only C++14 - MSVC 2017 should work just fine too.

### MacOS

We build Sneedacity using XCode 12. However, it is likely possible to build it with XCode 7.

### Linux

We use GCC 9, but any C++14 compliant compiler should work.

On Debian or Ubuntu, you can install everything required using the following commands:

```
$ sudo apt-get update
$ sudo apt-get install -y build-essential cmake git python3-pip
$ sudo pip3 install conan
$ sudo apt-get install libgtk2.0-dev libasound2-dev libavformat-dev libjack-jackd2-dev uuid-dev
```

## Building on Windows

1. Clone Sneedacity from the Sneedacity GitHub project. 
  
   For example, in the **git-bash** run:

    ```
    $ git clone https://github.com/formerlychuck/sneedacity/
    ```

2. Open CMake GUI. 
   
   Set the **Where is the source code** to the location where Sneedacity was cloned. 
   
   Set **Where to build the binaries** to the location you want to place your build in. It is preferred that this location is not within the directory with the source code.

3. Press **Configure**. You can choose which version of Visual Studio to use and the platform to build for in the pop-up. We support **x64** and **Win32** platforms. The **x64** platform is a default option. Press **Finish** to start the configuration process.

4. After successful configuration, you will see `Configuring done` in the last line of the log. Press **Generate** to generate the Visual Studio project. 

5. After you see "Generating done", press **Open Project** to open the project in Visual Studio.
   
6. Select "Build -> Build Solution".
   
7. You can now run and debug Sneedacity!
      
Generally, steps 1-5 are only needed the first-time you configure. Then, after you've generated the solution, you can open it in Visual Studio next time. If the project configuration has changed, the IDE will invoke CMake internally. 

> Conan Center provides prebuilt binaries only for **x64**. Configuring the project for Win32 will take much longer, as all the 3rd party libraries will be built during the configuration.

## macOS

1. Clone Sneedacity from the Sneedacity GitHub project. 
  
    ```
    $ git clone https://github.com/formerlychuck/sneedacity/
    ```

2. Configure Sneedacity using CMake:
   ```
   $ mkdir build && cd build
   $ cmake -GXcode -T buildsystem=1 ../sneedacity
   ```

3. Open Sneedacity XCode project:
   ```
   $ open Sneedacity.xcodeproj
   ```
   and build Sneedacity using the IDE. 

Steps 1 and 2 are only required for first-time builds. 

Alternatively, you can use **CLion**. If you chose to do so, open the directory where you have cloned Sneedacity using CLion and you are good to go.

At the moment we only support **x86_64** builds. It is possible to build using AppleSilicon hardware but **mad** and **id3tag** should be disabled:

```
cmake -GXCode -T buildsystem=1 -Dsneedacity_use_mad="off" -Dsneedacity_use_id3tag=off ../sneedacity
```

## Linux & Other OS

1. Clone Sneedacity from the Sneedacity GitHub project. 
  
    ```
    $ git clone https://github.com/formerlychuck/sneedacity/
    ```

2. Configure Sneedacity using CMake:
   ```
   $ mkdir build && cd build
   $ cmake -G "Unix Makefiles" -Dsneedacity_use_ffmpeg=loaded ../sneedacity
   ```
   By default, Debug build will be configured. To change that, pass `-DCMAKE_BUILD_TYPE=Release` to CMake.

3. Build Sneedacity:
   ```
   $ make -j`nproc`
   ```

4. Testing the build:
   Adding a "Portable Settings" folder allows Sneedacity to ignore the settings of any existing Sneedacity installation.
   ```
   $ cd bin/Debug
   $ mkdir "Portable Settings"
   $ ./sneedacity
   ```

5. Installing Sneedacity
   ```
   $ cd <build directory>
   $ sudo make install
   ```

## Advanced

### CMake options

You can use `cmake -LH` to get a list of the options available (or use CMake GUI or `ccmake`). The list will include documentation about each option. For convenience, [here is a list](CMAKE_OPTIONS.md) of the most notable options.

### Building using system libraries

On Linux it is possible to build Sneedacity using (almost) only the libraries provided by the package manager. Please, see the list of required libraries [here](linux/required_libraries.md).

```
$ mkdir build && cd build
$ cmake -G "Unix Makefiles" \
        -Dsneedacity_use_ffmpeg=loaded \
        -Dsneedacity_lib_preference=system \
        -Dsneedacity_obey_system_dependencies=On \
         ../sneedacity
```

There are a few cases when the local library build is preferred:

1. **wxWidgets**: While Sneedacity on **Linux** uses vanilla version of wxWidgets, we **require** that version **3.1.3** is used. This version is not available in most of the distributions.
2. **portaudio-v19**: Sneedacity currently uses [some private APIs](https://github.com/sneedacity/sneedacity/issues/871), so using system portaudio is not yet possible.
3. **vamp-host-sdk**: Development packages are not available in Ubuntu 20.04.
4. **libnyquist** & **portmixer**: Libraries are not available in Ubuntu 20.04.
5. **sqlite3** & **libsmbs**: Libraries are very outdated in Ubuntu 20.04.

It is not advised to mix system and local libraries, except for the list above. `ZLib` is a very common dependency; it is possible to mix system and local libraries in one build. However, we advise against doing so.

There is a [`Dockerfile`](linux/build-environment/Dockerfile) that can be used as an example of how to build Sneedacity using system libraries: 

```
$ docker build -t sneedacity_linux_env .\linux\build-environment\
$ docker run --rm -v ${pwd}:/sneedacity/sneedacity/ -v ${pwd}/../build/linux-system:/sneedacity/build -it sneedacity_linux_env
```

To find system packages, we rely on `pkg-config`. There are several packages that have broken `*.pc` or do not use `pkg-config` at all. For the docker image - we handle this issue by installing the correct [`pc` files](linux/build-environment/pkgconfig/).
