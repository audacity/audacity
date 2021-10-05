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
   lib-registries
   lib-xml
   lib-audio-devices
   lib-project
   lib-screen-geometry
   lib-project-rate
   lib-ffmpeg-support
)

if ( ${_OPT}has_networking )
   list( APPEND LIBRARIES lib-network-manager)
endif()

# This library depends on lib-network-manager
# If Sentry reporting is disabled, an INTERFACE library
# will be defined
list( APPEND LIBRARIES lib-sentry-reporting)

foreach( LIBRARY ${LIBRARIES} )
   add_subdirectory( "${LIBRARY}" )
endforeach()

set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
set( AUDACITY_LIBRARIES "${AUDACITY_LIBRARIES}" PARENT_SCOPE )
