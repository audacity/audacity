
# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
#  au3wrapDefs.cmake
#
#  AU3 wrap module definitions for AU4
#
#  This file provides AU4-specific setup for the au3wrap module.
#  Most AU3 library setup is handled by au3/au3defs.cmake and
#  au3/libraries/CMakeLists.txt.
#
# **********************************************************************

# Include AU3 compatibility layer
# This provides the audacity_library() macro and import_export_symbol() function
include(${AUDACITY_ROOT}/au3defs.cmake)

# Path variables for AU3 code
set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(AU3_MODULES ${AUDACITY_ROOT}/modules)
set(IMPORT_EXPORT_MODULE ${AU3_MODULES}/import-export)

# Generate *_API definitions for all AU3 libraries
# This is needed for AU3 source files compiled directly in au3wrap and for tests
set(_AU3_API_DEFS "")
if(DEFINED AU3_ALL_LIBRARIES)
    foreach(lib ${AU3_ALL_LIBRARIES})
        # Generate API macro for this library (same logic as in au3defs.cmake)
        import_export_symbol(api_symbol "${lib}")
        list(APPEND _AU3_API_DEFS -D${api_symbol}=)
    endforeach()
endif()

# AU4-specific definitions for au3wrap module
set(PKGLIBDIR "${_PKGLIBDIR}")
set(LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")

set(AU3_DEF
    # Audacity version information (needed by AU3 source files compiled directly in au3wrap)
    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"

    # safenew macro for AU3 compatibility
    -Dsafenew=new

    # Experimental features
    -DEXPERIMENTAL_SPECTRAL_EDITING

    # LV2 support flag
    -DUSE_LV2

    # Path definitions for plugins
    -DPKGLIBDIR="${PKGLIBDIR}"
    -DLIBDIR="${LIBDIR}"

    # FFmpeg support (not yet a library)
    -DFFMPEG_SUPPORT_API=

    # AU3 library API definitions (auto-generated from AU3_ALL_LIBRARIES)
    ${_AU3_API_DEFS}
)

# AU3 include directories for au3wrap module
# These are external dependencies and special paths needed by AU3 source files
# that are compiled directly in au3wrap (not yet converted to libraries)
# Note: AU3 library-specific includes are handled by au3defs.cmake
set(AU3_INCLUDE
    ${wxwidgets_INCLUDE_DIRS}
    ${expat_INCLUDE_DIRS}
    ${PortAudio_INCLUDE_DIR}
    ${libmp3lame_INCLUDE_DIRS}
    ${wavpack_INCLUDE_DIRS}
    ${libsndfile_INCLUDE_DIRS}
    ${vorbis_INCLUDE_DIRS}
    ${flac_INCLUDE_DIRS}
    ${ogg_INCLUDE_DIRS}
    ${opus_INCLUDE_DIRS}

    ${AUDACITY_ROOT}/lib-src/sqlite
    ${AUDACITY_ROOT}/lib-src/pffft
    ${AUDACITY_ROOT}/lib-src/libsoxr/src
    ${AUDACITY_ROOT}/lib-src/portmixer
    # As for the above libraries, SoundTouch includes: Even though soundtouch is linked via AU3_LINK, the include paths
    # don't propagate through the static library chain, so we need to add them explicitly
    ${AUDACITY_ROOT}/lib-src/soundtouch/include
    # also include the Generated soundtouch_config.h (needed by STTypes.h when compiling AU3 code that uses SoundTouch)
    ${CMAKE_BINARY_DIR}/src/au3wrap/au3-soundtouch/private
    # SBSMS includes: Similar to SoundTouch, we need to add SBSMS include paths explicitly
    ${AUDACITY_ROOT}/lib-src/libsbsms/include
    # also include the Generated config.h for SBSMS
    ${CMAKE_BINARY_DIR}/src/au3wrap/au3-sbsms/private

    # AU3 libraries directory for namespaced includes like #include "au3-tags/Tags.h"
    ${AU3_LIBRARIES}

    # for modules like mod-mp3, mod-ffmpeg that are still compiled as sources
    ${IMPORT_EXPORT_MODULE}
)

set(WXBASE_RESTRICTIONS
   "wxUSE_GUI=0"

   # Don't use app.h
   _WX_APP_H_BASE_

   # Don't use evtloop.h
   _WX_EVTLOOP_H_

   # Don't use image.h
   _WX_IMAGE_H

   # Don't use colour.h
   _WX_COLOUR_H_BASE_

   # Don't use brush.h
   _WX_BRUSH_H_BASE_

   # Don't use pen.h
   _WX_PEN_H_BASE_
)

set(AU3_LINK
    wxwidgets::wxwidgets
    expat::expat
    portaudio::portaudio # portaudio might only be required internally in au3-audio-io
    libmp3lame::libmp3lame
    wavpack::wavpack
    mpg123::libmpg123
    SndFile::sndfile
    Vorbis::vorbis
    FLAC::FLAC
    Ogg::ogg
    Opus::opus
    sqlite
    soxr
    pffft
    portmixer
    soundtouch
    libsbsms
)

set(WXBASE_DEFS
    ${WXBASE_RESTRICTIONS}
)

include(GetPlatformInfo)

if (OS_IS_LIN)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXGTK__)
elseif(OS_IS_MAC)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXMAC__)
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(AU3_LINK ${AU3_LINK} zlib::zlib ${CoreAudio} ${CoreAudioKit})
elseif(OS_IS_WIN)
   set(WXBASE_DEFS ${WXBASE_DEFS} __WXMSW__ WXUSINGDLL)
   set(AU3_LINK ${AU3_LINK} zlib::zlib winmm mmdevapi mfplat)
endif()

set(AU3_DEF ${AU3_DEF} ${WXBASE_DEFS})
