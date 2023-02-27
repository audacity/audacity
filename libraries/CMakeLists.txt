# Include the libraries that we'll build

# The list of modules is ordered so that each library occurs after any others
# that it depends on
set( LIBRARIES
   lib-string-utils
   lib-strings
   lib-utility
   lib-uuid
   lib-components
   lib-basic-ui
   lib-exceptions
   lib-export-ui
   lib-preferences
   lib-math
   lib-files
   lib-import-export
   lib-ipc
   lib-registries
   lib-xml
   lib-audio-devices
   lib-project
   lib-screen-geometry
   lib-project-rate
   lib-file-formats
   lib-theme
   image-compiler # not a library, but has a place in their dependency graph
   lib-theme-resources
   lib-track
   lib-transactions
   lib-sample-track
   lib-module-manager
   lib-project-history
   lib-graphics
   lib-audio-graph
   lib-tags
   lib-realtime-effects
   lib-audio-io
   lib-wave-track
   lib-track-selection
   lib-project-file-io
   lib-command-parameters
   lib-numeric-formats
   lib-effects
   lib-time-frequency-selection
   lib-wx-wrappers
   lib-shuttlegui
   lib-wx-init
   lib-time-track
   lib-vst3
   lib-snapping
   lib-vst
   lib-lv2
   lib-ladspa
   lib-audio-unit
   lib-playable-track
   lib-time-and-pitch
   lib-mixer
   lib-channel
   lib-stretching-sequence
   lib-label-track
)

if ( ${_OPT}has_networking )
   list( APPEND LIBRARIES lib-network-manager)
endif()

if ( ${_OPT}has_url_schemes_support )
   list( APPEND LIBRARIES lib-url-schemes)
endif()

if ( ${_OPT}has_audiocom_upload)
   list( APPEND LIBRARIES
      lib-cloud-audiocom
   )
endif()

if( ${_OPT}has_crashreports )
   if( ${_OPT}crashreport_backend STREQUAL "breakpad" )
      list( APPEND LIBRARIES lib-breakpad-configurer )
   elseif( ${_OPT}crashreport_backend STREQUAL "crashpad" )
      list( APPEND LIBRARIES lib-crashpad-configurer )
   endif()
endif()

# This library depends on lib-network-manager
# If Sentry reporting is disabled, an INTERFACE library
# will be defined
list( APPEND LIBRARIES lib-sentry-reporting)

foreach( LIBRARY ${LIBRARIES} )
   add_subdirectory( "${LIBRARY}" )
endforeach()

set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
