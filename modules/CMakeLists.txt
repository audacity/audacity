# Include the modules that we'll build

# The list of module sub-folders is ordered so that each folder occurs after any
# others that it depends on
set( FOLDERS
   etc
   import-export
   track-ui
   scripting
   nyquist
   sharing
)

foreach( FOLDER ${FOLDERS} )
   add_subdirectory("${FOLDER}")
endforeach()

#propagate collected edges and subgraphs up to root CMakeLists.txt
set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
set( GRAPH_SUBGRAPHS "${GRAPH_SUBGRAPHS}" PARENT_SCOPE )
