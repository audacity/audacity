# Include the libraries that we'll build

# The list of modules is ordered so that each library occurs after any others
# that it depends on
set( LIBRARIES
   lib-string-utils
   lib-strings
   lib-utility
   lib-uuid
)

foreach( LIBRARY ${LIBRARIES} )
   add_subdirectory( "${LIBRARY}" )
endforeach()

set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
set( AUDACITY_LIBRARIES "${AUDACITY_LIBRARIES}" PARENT_SCOPE )
