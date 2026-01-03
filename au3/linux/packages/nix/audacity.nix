{
  lib,
  stdenv,
  cmake,
  pkg-config,
  python3,
  git,
  qt6Packages,
  linuxHeaders,
  alsa-lib,
  libsndfile,
  portaudio,
  portmidi,
  libjack2,
  lame,
  libvorbis,
  libogg,
  flac,
  libopus,
  opusfile,
  mpg123,
  libid3tag,
  wavpack,
  expat,
  zlib,
  libpng,
  libjpeg,
  sqlite,
  libuuid,
  rapidjson,
  gtk3,
  harfbuzz,
  freetype,
  wxGTK32,
  gitignoreSource,
}:
stdenv.mkDerivation {
  pname = "audacity";
  version = "4.0.0-alpha-2";

  # nix-build doesn't respect .gitignore for some reason--when you have build artifacts at /build,
  # it tries to include them in the source tree, which breaks the build.
  src = gitignoreSource [] ../../../../.;

  nativeBuildInputs = [
    cmake
    pkg-config
    python3
    git
    qt6Packages.wrapQtAppsHook
    linuxHeaders
  ];

  buildInputs = [
    qt6Packages.qtbase
    qt6Packages.qttools
    qt6Packages.qtsvg
    qt6Packages.qt5compat
    qt6Packages.qtnetworkauth
    qt6Packages.qtdeclarative # Provides Qml, Quick, QuickControls2
    qt6Packages.qtshadertools
    alsa-lib
    libsndfile
    portaudio
    portmidi
    libjack2
    lame
    libvorbis
    libogg
    flac
    libopus
    opusfile
    mpg123
    libid3tag
    wavpack
    expat
    zlib
    libpng
    libjpeg
    sqlite
    libuuid
    rapidjson
    gtk3
    harfbuzz
    freetype
    wxGTK32
  ];

  postPatch = ''
    # Skip the SetupDevEnvironment step that tries to download dev tools
    substituteInPlace CMakeLists.txt \
      --replace-fail "include(SetupDevEnvironment)" "# include(SetupDevEnvironment) - disabled for Nix"

    # Replace SetupDependencies with our system library setup
    substituteInPlace CMakeLists.txt \
      --replace-fail "include(SetupDependencies)" '
# include(SetupDependencies) - disabled for Nix, using system libraries instead
# Create CMake targets for system libraries
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PORTAUDIO REQUIRED IMPORTED_TARGET portaudio-2.0)
    add_library(portaudio::portaudio ALIAS PkgConfig::PORTAUDIO)

    pkg_check_modules(EXPAT REQUIRED IMPORTED_TARGET expat)
    add_library(expat::expat ALIAS PkgConfig::EXPAT)

    pkg_check_modules(SNDFILE REQUIRED IMPORTED_TARGET sndfile)
    add_library(SndFile::sndfile ALIAS PkgConfig::SNDFILE)

    pkg_check_modules(WAVPACK REQUIRED IMPORTED_TARGET wavpack)
    add_library(wavpack::wavpack ALIAS PkgConfig::WAVPACK)

    pkg_check_modules(OGG REQUIRED IMPORTED_TARGET ogg)
    add_library(Ogg::ogg ALIAS PkgConfig::OGG)

    pkg_check_modules(VORBIS REQUIRED IMPORTED_TARGET vorbis vorbisenc vorbisfile)
    add_library(Vorbis::vorbis ALIAS PkgConfig::VORBIS)

    pkg_check_modules(FLAC REQUIRED IMPORTED_TARGET flac flac++)
    add_library(FLAC::FLAC ALIAS PkgConfig::FLAC)
    add_library(FLAC::FLAC++ ALIAS PkgConfig::FLAC)

    pkg_check_modules(OPUS REQUIRED IMPORTED_TARGET opus)
    add_library(Opus::opus ALIAS PkgConfig::OPUS)

    pkg_check_modules(OPUSFILE REQUIRED IMPORTED_TARGET opusfile)
    add_library(opusfile::opusfile ALIAS PkgConfig::OPUSFILE)

    pkg_check_modules(MPG123 REQUIRED IMPORTED_TARGET libmpg123)
    add_library(mpg123::libmpg123 ALIAS PkgConfig::MPG123)

    pkg_check_modules(ID3TAG REQUIRED IMPORTED_TARGET id3tag)
    add_library(id3tag::id3tag ALIAS PkgConfig::ID3TAG)

    # LAME MP3 encoder
    find_library(MP3LAME_LIBRARY mp3lame REQUIRED)
    add_library(libmp3lame::libmp3lame INTERFACE IMPORTED)
    set_target_properties(libmp3lame::libmp3lame PROPERTIES INTERFACE_LINK_LIBRARIES "''${MP3LAME_LIBRARY}")

    # wxWidgets
    find_package(wxWidgets REQUIRED)
    add_library(wxwidgets::wxwidgets INTERFACE IMPORTED)
    set_target_properties(wxwidgets::wxwidgets PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "''${wxWidgets_INCLUDE_DIRS}" INTERFACE_LINK_LIBRARIES "''${wxWidgets_LIBRARIES}" INTERFACE_COMPILE_DEFINITIONS "''${wxWidgets_DEFINITIONS}")
'

    # Fix hard-coded /usr/include/linux/magic.h path
    substituteInPlace au3/libraries/au3-files/FileNames.cpp \
      --replace-fail '"/usr/include/linux/magic.h"' '"${linuxHeaders}/include/linux/magic.h"'

    # Fix VST stub include (it should include its own header, not the full module's header)
    substituteInPlace src/stubs/vst/vsteffectsstubmodule.cpp \
      --replace-fail '#include "vsteffectsmodule.h"' '#include "vsteffectsstubmodule.h"'

    # Fix effects_builtin not linking to effects_base (needed for AbstractViewLauncher)
    substituteInPlace src/effects/builtin/CMakeLists.txt \
      --replace-fail 'set(MODULE_LINK au3wrap)' 'set(MODULE_LINK au3wrap effects_base)'
  '';

  cmakeFlags = [
    "-DAUDACITY_BUILD_LEVEL=2"
    "-DAUDACITY_REV_LONG=nixpkgs"
    "-DAUDACITY_REV_TIME=nixpkgs"
    "-DDISABLE_DYNAMIC_LOADING_FFMPEG=ON"
    "-Daudacity_conan_enabled=Off"
    "-Daudacity_use_ffmpeg=loaded"
    "-Daudacity_has_vst3=Off"
    "-Daudacity_has_crashreports=Off"

    # RPATH of binary /nix/store/.../bin/... contains a forbidden reference to /build/
    "-DCMAKE_SKIP_BUILD_RPATH=ON"

    # Fix duplicate store paths
    "-DCMAKE_INSTALL_LIBDIR=lib"

    # Use system libraries instead of downloading them
    "-DMUE_COMPILE_USE_SYSTEM_HARFBUZZ=ON"
    "-DMUE_COMPILE_USE_SYSTEM_FREETYPE=ON"
    "-DMUE_COMPILE_USE_SYSTEM_FLAC=ON"
    "-DMUE_COMPILE_USE_SYSTEM_OPUS=ON"
    "-DMUE_COMPILE_USE_SYSTEM_OPUSENC=ON"

    # Disable LV2 and VST to avoid network fetching
    "-DAU_MODULE_EFFECTS_LV2=OFF"
    "-DAU_MODULE_EFFECTS_VST=OFF"
  ];
}
