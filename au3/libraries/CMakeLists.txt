#[[
A directory containing library targets

This is the master CMakeLists.txt for all AU3 libraries.
It sets up the AU3_ALL_LIBRARIES cache variable and includes au3defs.cmake
before processing individual library subdirectories.
]]

# Create wxBase INTERFACE target for toolkit-neutral wxWidgets usage
# This is a simple wrapper around wxwidgets::wxwidgets that provides include paths
# and linking, but does NOT include GUI restrictions as compile definitions.
#
# The audacity_library() macro in au3defs.cmake automatically applies wxBase
# restrictions (wxUSE_GUI=0, etc.) as PRIVATE compile definitions to any library
# that links to wxBase. This ensures:
# - Libraries using wxBase get the restrictions applied to their own compilation
# - The restrictions do NOT propagate to consumers (PRIVATE, not INTERFACE)
# - Libraries can choose between wxBase (no GUI) or wxwidgets::wxwidgets (full GUI)
if(NOT TARGET wxBase)
    add_library(wxBase INTERFACE)
    target_link_libraries(wxBase INTERFACE wxwidgets::wxwidgets)
endif()

# The list of modules is ordered so that each library occurs after any others
# that it depends on
set( LIBRARIES
   au3-string-utils
   au3-strings
   au3-utility
   au3-uuid
   au3-components
   au3-basic-ui
   au3-exceptions
#   au3-export-ui # not yet used in AU4
   au3-preferences
   au3-math
   au3-files
   au3-import-export
   au3-ipc
   au3-registries
   au3-xml
   au3-audio-devices
   au3-project
   au3-screen-geometry
   au3-project-rate
   au3-file-formats
#   au3-theme # not yet used in AU4
#   au3-theme-resources # not yet used in AU4
   au3-track
   au3-transactions
   au3-sample-track
   au3-module-manager
   au3-project-history
   au3-graphics
   au3-audio-graph
   au3-tags
   au3-realtime-effects
   au3-audio-io
   au3-wave-track
#   au3-wave-track-settings # not yet used in AU4
   au3-wave-track-paint
   au3-track-selection
   au3-project-file-io
   au3-command-parameters
   au3-numeric-formats
   au3-effects
   au3-time-frequency-selection
#   au3-wx-wrappers # not yet used in AU4
#   au3-shuttlegui # not yet used in AU4
#   au3-wx-init # not yet used in AU4
#   au3-time-track # not yet used in AU4
   au3-snapping
#   au3-vst # not yet used in AU4
   au3-playable-track
   au3-time-and-pitch
   au3-mixer
   au3-channel
   au3-stretching-sequence
   au3-menus
#   au3-note-track # not yet used in AU4
   au3-viewport
#   au3-music-information-retrieval # not yet used in AU4
#   au3-crypto # not yet used in AU4
   au3-fft
#   au3-concurrency # not yet used in AU4
   au3-sqlite-helpers
#   au3-preference-pages # not yet used in AU4
   au3-dynamic-range-processor
   au3-audacity-application-logic
   au3-builtin-effects
   au3-label-track
   au3-wave-track-fft
)

# Conditional libraries based on AU4 build options
# Note: AU4 uses AU_MODULE_* variables instead of AU3's ${_OPT}* variables

if (AU_MODULE_EFFECTS_VST)
   list( APPEND LIBRARIES au3-vst3)
endif()

if (AU_MODULE_EFFECTS_LV2)
   list( APPEND LIBRARIES au3-lv2)
endif()

if (AU_MODULE_EFFECTS_AUDIO_UNIT)
   list( APPEND LIBRARIES au3-audio-unit)
endif()

# AU3-specific conditional libraries (not used in AU4 yet)
# These use ${_OPT} variables from AU3's build system
if ( DEFINED ${_OPT}has_networking AND ${_OPT}has_networking )
   list( APPEND LIBRARIES au3-network-manager)
endif()

if ( DEFINED ${_OPT}has_url_schemes_support AND ${_OPT}has_url_schemes_support )
   list( APPEND LIBRARIES au3-url-schemes)
endif()

if ( DEFINED ${_OPT}has_audiocom_upload AND ${_OPT}has_audiocom_upload)
   list( APPEND LIBRARIES au3-cloud-audiocom)
endif()

if( DEFINED ${_OPT}has_crashreports AND ${_OPT}has_crashreports )
   if( ${_OPT}crashreport_backend STREQUAL "breakpad" )
      list( APPEND LIBRARIES au3-breakpad-configurer )
   elseif( ${_OPT}crashreport_backend STREQUAL "crashpad" )
      list( APPEND LIBRARIES au3-crashpad-configurer )
   endif()
endif()

if ( DEFINED ${_OPT}use_nyquist AND ${_OPT}use_nyquist )
   list( APPEND LIBRARIES au3-nyquist-effects )
endif()

if ( DEFINED ${_OPT}use_ladspa AND ${_OPT}use_ladspa )
   list( APPEND LIBRARIES au3-ladspa )
endif()

# This library depends on au3-network-manager
# If Sentry reporting is disabled, an INTERFACE library
# will be defined
#Disabling for now as not used yet in AU4 : list( APPEND LIBRARIES au3-sentry-reporting)

# Set a CACHE variable with all AU3 library names so that au3defs.cmake can use it
# to generate API macros for all libraries
set(AU3_ALL_LIBRARIES ${LIBRARIES} CACHE INTERNAL "List of all AU3 libraries")

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_LIST_DIR}/../au3defs.cmake)

# Add subdirectories for all libraries
foreach( LIBRARY ${LIBRARIES} )
   add_subdirectory( "${LIBRARY}" )
endforeach()

set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )

# Export the list of libraries to parent scope for linking
set(AU3_LIBRARIES_LIST ${LIBRARIES} PARENT_SCOPE)
