# Building Audacity

## Prerequisites

* **python3** >= 3.9.1
* **conan** >= 1.32.0
* **cmake** >= 1.36
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

On Linux, `cmake` is usually available from the system package manager. The latest CMake version is available from Snap: `sudo snap instal cmake --classic`.

### Windows

We build Audacity using [Microsoft Visual Studio 2019](https://visualstudio.microsoft.com/vs/community/). In order to build Audacity **Desktop development with C++** workload is required.

As we require only C++14 - MSVC 2017 should work just well too.

### MacOS

We build Audacity using XCode 12. However, it is likely possible to build it with XCode 7.

### Linux

We use GCC 9, but any C++14 compliant compiler should work.

On Debian or Ubuntu, you can install everything required using the following commands:

```
$ sudo apt-get update
$ sudo apt-get install -y build-essential cmake git python3-pip
$ sudo pip3 install conan
$ sudo apt-get install libgtk2.0-dev libasound2-dev libavformat-dev libjack-jackd2-dev
```

## Building on Windows

1. Clone Audacity from the Audacity GitHub project. 
  
   For example, in the **git-bash** run:

    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Open CMake GUI. 
   
   Set the **Where is the source code** to the location Audicity was cloned. 
   
   Set **Where to build the binaries** to the location you want to place your build in. It is preffered, that this location is not within the directory with the source code.

3. Press **Configure**. You can choose which version of Visual Studio to use and the platform to build for in the popup. We support **x64** and **Win32** platforms. The **x64** platform is a default option. Press **Finish** to start the configuration process.

4. After the successful configuration, you will see `Configuring done` in the last line of the log. Press **Generate** to generate the Visual Studio project. 

5. After you see "Generating done", press **Open Project** to open the project in Visual Studio.
   
6. Select "Build -> Build Solution".
   
7. You can now run and debug Audacity!
      
Generally, steps 1-5 are only needed for the first-time configuration. Then, after you've generated the solution - you can open it in Visual Studio next time. In case when the project configuration has changed - the IDE will invoke CMake internally. 

> Conan Center provides prebuilt binaries only for **x64**. Configuring the project for Win32 will take much longer, as all the 3d party libraries will be built during the configuration.

## macOS

1. Clone Audacity from the Audacity GitHub project. 
  
    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Configure Audacity using CMake:
   ```
   $ mkdir build && cd build
   $ cmake -GXCode -T buildsystem=1 ../audacity
   ```

3. Open Audacity XCode project:
   ```
   $ open Audacity.xcodeproj
   ```
   and build Audacity using the IDE. 

Steps 1 and 2 are only required for the first-time builds. 

Alternatively, you can use **CLion**. If you chose so, open the directory where you have cloned Audacity using CLion and you are good to go.

At the moment, we only support **x86_64** builds. It is possible to build using AppleSilicon harware, but **mad** and **id3tag** should be disabled:

```
cmake -GXCode -T buildsystem=1 -Daudacity_use_mad="off" -Daudacity_use_id3tag=off ../audacity
```

## Linux & Other OS

1. Clone Audacity from the Audacity GitHub project. 
  
    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Configure Audacity using CMake:
   ```
   $ mkdir build && cd build
   $ cmake -G "Unix Makefiles" -Daudacity_use_ffmpeg=loaded ../audacity
   ```

3. Build Audacity:
   ```
   $ make -j'nproc`
   ```

4. Testing the build:
   Adding a "Portable Settings" folder allows Audacity to ignore the settings of any existing Audacity installation.
   ```
   $ cd bin/Release
   $ mkdir "Portable Settings"
   $ ./audacity
   ```

5. Installing Audacity
   ```
   $ cd <build directory>
   $ sudo make install
   ```

## Advanced

### CMake options

You can use `cmake -LH` to get a list of the options available (or use CMake GUI or `ccmake`).

The most notable options are:

| Name                              | Type   | Default    | Description                                                     |
| :-------------------------------- | :----- | :--------- | :-------------------------------------------------------------- |
| CMAKE_BUILD_TYPE                  | STRING | Debug      | Type of the build: Debug, Release, RelWithDebInfo, MinSizeRel   |
| CMAKE_INSTALL_PREFIX              | PATH   | /usr/local | Install path prefix, prepended onto install directories.        |
| audacity_lib_preference           | STRING | local      | Library preference [system (if available), local]               |
| audacity_obey_system_dependencies | BOOL   | Off        | Use only system packages to satisfy dependencies                |
| audacity_use_expat                | STRING | system     | Use expat library [system (if available), local, off]           |
| audacity_use_ffmpeg               | STRING | loaded     | Use ffmpeg library [loaded, linked, off]                        |
| audacity_use_flac                 | STRING | local      | Use flac library [system (if available), local, off]            |
| audacity_use_id3tag               | STRING | local      | Use id3tag library [system (if available), local, off]          |
| audacity_use_ladspa               | BOOL   | ON         | Use LADSPA plug-in support [on, off]                            |
| audacity_use_libmad               | STRING | local      | Use libmad library [system (if available), local, off]          |
| audacity_use_libmp3lame           | STRING | local      | Use libmp3lame library [system (if available), local, off]      |
| audacity_use_lv2                  | STRING | local      | Use lv2 library [system (if available), local, off]             |
| audacity_use_mad                  | STRING | local      | Use mad library [system (if available), local, off]             |
| audacity_use_midi                 | STRING | local      | Use midi library [system (if available), local, off]            |
| audacity_use_nyquist              | STRING | local      | Use nyquist library [local, off]                                |
| audacity_use_ogg                  | STRING | local      | Use ogg library [system (if available), local, off]             |
| audacity_use_pa_alsa              | BOOL   | YES        | Use the portaudio ALSA interface if available                   |
| audacity_use_pa_jack              | STRING | linked     | Use the JACK audio interface if available [loaded, linked, off] |
| audacity_use_pa_oss               | BOOL   | YES        | Use the OSS audio interface if available                        |
| audacity_use_pch                  | BOOL   | YES        | Use precompiled headers [yes, no]                               |
| audacity_use_portaudio            | STRING | local      | Use portaudio library [local]                                   |
| audacity_use_portmixer            | STRING | local      | Use portmixer library [local, off]                              |
| audacity_use_portsmf              | STRING | local      | Use portsmf library [system (if available), local, off]         |
| audacity_use_sbsms                | STRING | local      | Use sbsms library [system (if available), local, off]           |
| audacity_use_sndfile              | STRING | local      | Use sndfile library [system (if available), local]              |
| audacity_use_soundtouch           | STRING | local      | Use soundtouch library [system (if available), local, off]      |
| audacity_use_soxr                 | STRING | local      | Use soxr library [system (if available), local]                 |
| audacity_use_sqlite               | STRING | local      | Use sqlite library [system (if available), local]               |
| audacity_use_twolame              | STRING | local      | Use twolame library [system (if available), local, off]         |
| audacity_use_vamp                 | STRING | local      | Use vamp library [system (if available), local, off]            |
| audacity_use_vorbis               | STRING | local      | Use vorbis library [system (if available), local, off]          |
| audacity_use_vst                  | BOOL   | ON         | Use VST2 plug-in support [on, off]                              |
| audacity_use_wxwidgets            | STRING | local      | Use wxwidgets library [system (if available), local, off]       |
| audacity_use_zlib                 | STRING | system     | Use zlib library [system (if available), local, off]            |


### Building using system libraries

On Linux, it is possible to build Audacity using (almost) only the libraries provided by the package manager. We require the following version of the packages to be installed:

| Name              | Version       | Ubuntu 20.04 package                         |
| :---------------- | :------------ | :------------------------------------------- |
| wxWidgets         | >= 3.1.3      | **N/A**                                      |
| expat             | >= 2.2.9      | libexpat1-dev                                |
| libmp3lame        | >= 3.100      | libmp3lame-dev                               |
| libsndfile        | >= 1.0.28     | libsndfile-dev                               |
| libsoxr           | >= 0.1.3      | libsoxr-dev                                  |
| ~~portaudio-v19~~ | ~~>= 19.6.0~~ | ~~portaudio19-dev~~                          |
| sqlite3           | >= 3.31.1     | libsqlite3-dev                               |
| libcurl           | >= 7.68.0     | libcurl-dev                                  |
| ffmpeg            | >= 4.2.4      | libavcodec-dev libavformat-dev libavutil-dev |
| libid3tag         | >= 0.15.1b    | libid3tag0-dev                               |
| libmad            | >= 0.15.1b    | libmad0-dev                                  |
| vamp-host-sdk     | >= 2.9.0      | N/A                                          |
| libogg            | >= 1.3.4      | libogg-dev                                   |
| libvorbis         | >= 1.3.6      | libvorbis-dev                                |
| libflac           | >= 1.3.3      | libflac-dev libflac++-dev                    |
| lilv              | >= 0.24.6     | liblilv-dev                                  |
| lv2               | >= 1.16.0     | lv2-dev                                      |
| serd              | >= 0.30.2     | libserd-dev                                  |
| sord              | >= 0.16.4     | libsord-dev                                  |
| sratom            | >= 0.6.4      | libsratom-dev                                |
| suil              | >= 0.10.6     | libsuil-dev                                  |
| portmidi          | >= 217        | libportmidi-dev                              |
| portsmf           | >= 0.1        | libportsmf-dev                               |
| libsbsms          | >= 2.0.2      | libsbsms-dev                                 |
| soundtouch        | >= 2.1.2      | libsoundtouch-dev                            |
| twolame           | >= 0.4.0      | libtwolame-dev                               |
| zlib              | >= 1.2.11     | libzlib1g-dev                                |

There are a few cases when the local library build is preferred:

1. **wxWidgets**: While Audacity on **Linux** uses vanilla version of wxWidgets, we **require** that version **3.1.3** is used. This version is not available in most of the distributives.
2. **portaudio-v19**: Audacity currently uses some private APIs, so using system portaudio is not yet possible.
3. **vamp-host-sdk**: Development packages are not available in Ubuntu 20.04.
4. **libnyquist**, **portmixer**: Libraries is not available in Ubuntu 20.04.
5. **sqlite3**, **libsmbs**: Libraries are very outdated in Ubuntu 20.04.

It is not advised to mix system and local libraries, except for the list above. `ZLib` is very common dependency; it is possible to mix system and local libraries in one build. However, we try to mitigate this issue to some extent.

There is a [`Dockerfile`](linux/build-environment/Dockerfile) that can be used as an example of how to build the Audacity using system libraries: 

```
$ docker build -t audacity_linux_env .\linux\build-environment\
$ docker run --rm -v ${pwd}:/audacity/audacity/ -v ${pwd}/../build/linux-system:/audacity/build -it audacity_linux_env
```

To find system packages, we rely on `pkg-config`. There are several packages, that have broken `*.pc` or do not use `pkg-config` at all. For the docker image - we mitigate this issue by installing the correct [`pc` files](linux/build-environment/pkgconfig/).