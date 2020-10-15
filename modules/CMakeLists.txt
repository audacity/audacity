# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
   mod-screenshot
   mod-timer-record
   mod-frequency-plot
   mod-contrast
   mod-undo-history
   mod-lyrics
   mod-mixer-board
   mod-script-pipe
)
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows" )
   list( APPEND MODULES
      mod-null
      mod-nyq-bench
   )
endif()

foreach( MODULE ${MODULES} )
   add_subdirectory("${MODULE}")
endforeach()

#propagate collected edges up to root CMakeLists.txt
set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
