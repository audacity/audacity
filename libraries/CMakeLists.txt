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
   lib-preferences
   lib-math
   lib-files
   lib-ipc
   lib-registries
   lib-xml
   lib-audio-devices
   lib-project
   lib-screen-geometry
   lib-project-rate
   lib-ffmpeg-support
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
   lib-command-parameters
   lib-numeric-formats
   lib-wx-wrappers
   lib-shuttlegui
)

if ( ${_OPT}has_networking )
   list( APPEND LIBRARIES lib-network-manager)
endif()

if ( ${_OPT}has_url_schemes_support )
   list( APPEND LIBRARIES lib-url-schemes)
endif()

if ( ${_OPT}has_audiocom_upload)
   list( APPEND LIBRARIES
      lib-cloud-upload
      lib-cloud-audiocom
   )
endif()

# This library depends on lib-network-manager
# If Sentry reporting is disabled, an INTERFACE library
# will be defined
list( APPEND LIBRARIES lib-sentry-reporting)

foreach( LIBRARY ${LIBRARIES} )
   add_subdirectory( "${LIBRARY}" )
endforeach()

set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
