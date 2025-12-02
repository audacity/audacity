
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

# AU4-specific definitions for au3wrap module
set(PKGLIBDIR "${_PKGLIBDIR}")
set(LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")

set(AU3_DEF
    # Audacity version information (defined in au3defs.cmake, included above)
    ${AUDACITY_VERSION_DEFS}

    # Path definitions for plugins
    -DPKGLIBDIR="${PKGLIBDIR}"
    -DLIBDIR="${LIBDIR}"

    # FFmpeg support (not yet a library)
    -DFFMPEG_SUPPORT_API=

    # AU3 library API definitions (defined in au3defs.cmake, included above)
    # Note: AU3_API_DEFS is generated in au3defs.cmake (included above) and reused here
    # to avoid duplicating the API generation logic
    ${AU3_API_DEFS}

    # Platform-specific wxWidgets definitions
    # Note: WXBASE_RESTRICTIONS and WXPLATFORM_DEFS are defined in au3defs.cmake (included above)
    # and are reused here for AU3 source files compiled directly in au3wrap
    ${WXBASE_RESTRICTIONS} ${WXPLATFORM_DEFS}
)

# AU3 include directories for au3wrap module
# These are external dependencies and special paths needed by AU3 source files
# that are compiled directly in au3wrap (not yet converted to libraries)
# Note: AU3 library-specific includes are handled by au3defs.cmake
set(AU3_INCLUDE
#    ${libsndfile_INCLUDE_DIRS} # TODO: mod-pcm
#    ${vorbis_INCLUDE_DIRS} # TODO: mod-ogg
#    ${flac_INCLUDE_DIRS} # TODO: mod-flac
#    ${ogg_INCLUDE_DIRS} # TODO: mod-opus
#    ${opus_INCLUDE_DIRS} # TODO: mod-opus

    # AU3 libraries directory for namespaced includes like #include "au3-tags/Tags.h"
    ${AU3_LIBRARIES}

    # for modules like mod-mp3, mod-ffmpeg that are still compiled as sources
    ${IMPORT_EXPORT_MODULE}
)

set(AU3_LINK
    libmp3lame::libmp3lame # used by mod-mp3 not yet a library
    wavpack::wavpack # used by mod-wavpack not yet a library
    mpg123::libmpg123 # used by mod-mpg123 not yet a library
    SndFile::sndfile # mod-pcm
    Vorbis::vorbis # mod-ogg
    FLAC::FLAC # mod-flac
    Ogg::ogg # mod-opus
    Opus::opus # mod-opus
)

# Platform-specific libraries for au3wrap
if(OS_IS_MAC)
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(AU3_LINK ${AU3_LINK} zlib::zlib ${CoreAudio} ${CoreAudioKit})
elseif(OS_IS_WIN)
   set(AU3_LINK ${AU3_LINK} zlib::zlib winmm mmdevapi mfplat)
endif()
