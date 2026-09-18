
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
# It also defines PKGLIBDIR and LIBDIR which are shared by all AU3 code
include(${AUDACITY_ROOT}/au3defs.cmake)

# Path variables for AU3 code
set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(AU3_MODULES ${AUDACITY_ROOT}/modules)

set(AU3_DEF
    # Audacity version information (defined in au3defs.cmake, included above)
    ${AUDACITY_VERSION_DEFS}

    # Path definitions for plugins (defined in au3defs.cmake, included above)
    -DPKGLIBDIR="${PKGLIBDIR}"
    -DLIBDIR="${LIBDIR}"

    # Note: the per-library API definitions (e.g. UTILITY_API) are deliberately NOT
    # part of AU3_DEF: each au3 library exports its own API define publicly, so
    # consumers must link the au3 libraries whose headers they use. Relying on
    # AU3_API_DEFS here would also be non-deterministic, because it is generated from
    # the AU3_ALL_LIBRARIES cache variable which is only populated later in the
    # first configure run.

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
    # AU3 libraries directory for namespaced includes like #include "au3-tags/Tags.h"
    ${AU3_LIBRARIES}
)

set(AU3_LINK "")

# Platform-specific libraries for au3wrap
# These belong on the portmixer/portaudio wrappers that actually need them -
# moving them (and retiring AU3_LINK) is tracked in
# https://github.com/audacity/audacity/issues/11806
if(OS_IS_MAC)
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(AU3_LINK ${AU3_LINK} zlib::zlib ${CoreAudio} ${CoreAudioKit})
elseif(OS_IS_WIN)
   set(AU3_LINK ${AU3_LINK} zlib::zlib winmm mmdevapi mfplat)
endif()
