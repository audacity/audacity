# libsndfile

[![Build Status](https://secure.travis-ci.org/erikd/libsndfile.svg?branch=master)](http://travis-ci.org/erikd/libsndfile)
[![Build status](https://ci.appveyor.com/api/projects/status/github/erikd/libsndfile?branch=master?svg=true)](https://ci.appveyor.com/project/Erik13183/libsndfile)

libsndfile is a C library for reading and writing files containing sampled audio
data.

## Hacking

The canonical source code repository for libsndfile is at
[https://github.com/erikd/libsndfile/][github].

You can grab the source code using:

    git clone git://github.com/erikd/libsndfile.git

For building for Android see [BuildingForAndroid][BuildingForAndroid].

There are currently two build systems; the official GNU autotool based one and
a more limited and experimental CMake based build system. Use of the CMake build
system is documented below.

Setting up a build environment for libsndfile on Debian or Ubuntu is as simple as:

    sudo apt install autoconf autogen automake build-essential libasound2-dev \
      libflac-dev libogg-dev libtool libvorbis-dev libopus-dev pkg-config python

For other Linux distributions or any of the *BSDs, the setup should be similar
although the package install tools and package names may be slightly different.

Similarly on Mac OS X, assuming [brew] is already installed:

    brew install autoconf autogen automake flac libogg libtool libvorbis libopus pkg-config

Once the build environment has been set up, building and testing libsndfile is
as simple as:

    ./autogen.sh
    ./configure --enable-werror
    make
    make check

## The CMake build system

Although Autotools is the primary and recommended build toolchain, experimental
CMake meta build generator is also available. The build process with CMake takes
place in two stages. First, standard build files are created from configuration
scripts. Then the platform's native build tools are used for the actual
building. CMake can produce Microsoft Visual Studio project and solution files,
Unix Makefiles, Xcode projects and [many more](https://cmake.org/cmake/help/latest/manual/cmake-generators.7.html).

Some IDE support CMake natively or with plugins, check you IDE documentation
 for details.

### Requirements

1. C99-compliant compiler toolchain (tested with GCC, Clang and Visual
   Studio 2015)
2. CMake 3.1.3 or newer

There are some recommended packages to enable all features of libsndfile:

1. Ogg, Vorbis and FLAC libraries and headers to enable these formats support
2. ALSA development package under Linux to build sndfile-play utility
3. Sndio development package under BSD to build sndfile-play utility

### Building from command line

CMake can handle out-of-place builds, enabling several builds from
the same source tree, and cross-compilation. The ability to build a directory
tree outside the source tree is a key feature, ensuring that if a build
directory is removed, the source files remain unaffected.

    mkdir CMakeBuild
    cd CMakeBuild

Then run `cmake` command with directory where CMakeLists.txt script is located
as argument (relative paths are supported):

    cmake ..

This command will configure and write build script or solution to CMakeBuild
directory. CMake is smart enough to create Unix makefiles under Linux or Visual
Studio solution if you have Visual Studio installed, but you can configure
[generator](https://cmake.org/cmake/help/latest/manual/cmake-generators.7.html)
with `-G` command line parameter:

    cmake .. -G"Unix Makefiles"

The build procedure depends on the selected generator. With "Unix Makefiles" you
can type:

    make & make install

With "Visual Studio" and some other generators you can open solution or project
from `CMakeBuild` directory and build using IDE.

Finally, you can use unified command:

    cmake --build .

CMake also provides Qt-based cross platform GUI, cmake-gui. Using it is trivial
and does not require detailed explanations.

### Configuring CMake

You can pass additional options with `/D<parameter>=<value>` when you run
`cmake` command. Some useful system options:

* `CMAKE_C_FLAGS` - additional C compiler flags
* `CMAKE_BUILD_TYPE` - configuration type, `DEBUG`, `RELEASE`, `RELWITHDEBINFO`
  or `MINSIZEREL`. `DEBUG` is default
* `CMAKE_INSTALL_PREFIX` - build install location, the same as `--prefix` option
  of `configure` script

 Useful libsndfile options:

* `BUILD_SHARED_LIBS` - build shared library (DLL under Windows) when `ON`,
  build static library othervise. This option is `OFF` by default.
* `BUILD_PROGRAMS` - build libsndfile's utilities from `programs/` directory,
  `ON` by default.
* `BUILD_EXAMPLES` - build examples, `ON` by default.
* `BUILD_TESTING` - build tests. Then you can run tests with `ctest` command,
  `ON` by default. Setting `BUILD_SHARED_LIBS` to `ON` disables this option.
* `ENABLE_EXTERNAL_LIBS` - enable Ogg, Vorbis, FLAC and Opus support. This
  option is available and set to `ON` if all dependency libraries were found.
* `ENABLE_CPU_CLIP` - enable tricky cpu specific clipper. Enabled and set to
  `ON` when CPU clips negative\positive. Don't touch it if you are not sure
* `ENABLE_BOW_DOCS` - enable black-on-white documentation theme, `OFF` by
  default.
* `ENABLE_EXPERIMENTAL` - enable experimental code. Don't use it if you are
  not sure. This option is `OFF` by default.
* `ENABLE_CPACK` - enable [CPack](https://cmake.org/cmake/help/latest/module/CPack.html) support.
  This option is `ON` by default.
* `ENABLE_PACKAGE_CONFIG` - generate and install [package config file](https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#config-file-packages).
  This option is `ON` by default.
* `ENABLE_STATIC_RUNTIME` - enable static runtime on Windows platform, `OFF` by
  default.

  **Note**: For MSVC compiler this option is depecated. Use `CMAKE_MSVC_RUNTIME_LIBRARY`
  option instead (CMake >= 3.15).
* `ENABLE_COMPATIBLE_LIBSNDFILE_NAME` - set DLL name to `libsndfile-1.dll`
  (canonical name) on Windows platform, `sndfile.dll` otherwise, `OFF` by
  default. Library name can be different depending on platform. The well known
  DLL name on Windows platform is `libsndfile-1.dll`, because the only way to
  build Windows library before was MinGW toolchain with Autotools. This name
  is native for MinGW ecosystem, Autotools constructs it using MinGW platform
  rules from `sndfile` target. But when you build with CMake using native
  Windows compiler, the name is `sndfile.dll`. This is name for native Windows
  platform, because Windows has no library naming rules. It is preffered
  because you can search library using package manager or CMake's
  `find_library` command on any platform using the same `sndfile` name.

Deprecated options:

* `DISABLE_EXTERNAL_LIBS` - disable Ogg, Vorbis and FLAC support. Replaced by
  `ENABLE_EXTERNAL_LIBS`
* `DISABLE_CPU_CLIP` - disable tricky cpu specific clipper. Replaced by
  `ENABLE_CPU_CLIP`
* `BUILD_STATIC_LIBS` - build static library. Use `BUILD_SHARED_LIBS` instead

### Linking from CMake projects

First you need to add `FindOgg.cmake`, `FindVorbis.cmake`, `FindVorbisEnc.cmake`,
 `FindFLAC.cmake` and `FindOpus.cmake` files to some directory inside your CMake
 project (usually `cmake`) and add it to `CMAKE_MODULE_PATH`:

    project(SomeApplication)
    
    list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

Now you can search `libsndfile` library from your `CMakeLists.txt`
 with this command:

    find_package(SndFile)

`SndFile_FOUND` is set to `ON` when library is found.

If `libsndfile` dependency is critical, you can add `REQUIRED` to
 `find_package`:

    find_package(SndFile REQUIRED)

With with option `find_package` will terminate configuration process
 if `libsndfile` is not found.

You can also add version check:

    find_package(SndFile 1.0.29)

`find_package` will report error, if `libsndfile` version is < 1.0.29.

You can combine `REQUIRED` and version if you need.

To link `libsndfile` library use:

    target_link_libraries(my_application PRIVATE SndFile::sndfile)

### Notes for Windows users

First advice - set `ENABLE_STATIC_RUNTIME` to ON. This will remove dependencies
on runtime DLLs.

Second advice is about Ogg, Vorbis FLAC and Opus support. Searching external
libraries under Windows is a little bit tricky. The best way is to use
[Vcpkg](https://github.com/Microsoft/vcpkg). You need to install static libogg,
libvorbis, libflac and libopus libraries:

    vcpkg install libogg:x64-windows-static libvorbis:x64-windows-static
    libflac:x64-windows-static opus:x64-windows-static libogg:x86-windows-static
    libvorbis:x86-windows-static libflac:x86-windows-static opus:x86-windows-static

Then and add this parameter to cmake command line:

    -DCMAKE_TOOLCHAIN_FILE=<path-to-vcpkg>/scripts/buildsystems/vcpkg.cmake

You also need to set `VCPKG_TARGET_TRIPLET` because you use static libraries:

    -DVCPKG_TARGET_TRIPLET=x64-windows-static

## Submitting Patches

See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

[brew]: http://brew.sh/
[github]: https://github.com/erikd/libsndfile/
[BuildingForAndroid]: https://github.com/erikd/libsndfile/blob/master/Building-for-Android.md
