# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
   mod-script-pipe
   mod-mp3
   mod-pcm
   mod-cl
   mod-lof
   mod-aup
)
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows" )
   list( APPEND MODULES
      mod-null
      mod-nyq-bench
   )
endif()

if ( USE_LIBOGG AND USE_LIBVORBIS )
   list( APPEND MODULES mod-ogg )
endif()

if ( USE_LIBFLAC )
   list( APPEND MODULES mod-flac )
endif()

if ( USE_LIBTWOLAME )
   list ( APPEND MODULES mod-mp2)
endif()

if ( USE_WAVPACK )
   list ( APPEND MODULES mod-wavpack )
endif()

if ( USE_LIBMPG123 )
   list ( APPEND MODULES mod-mpg123 )
endif()

if ( USE_FFMPEG )
   list ( APPEND MODULES mod-ffmpeg )
endif()

foreach( MODULE ${MODULES} )
   add_subdirectory("${MODULE}")
endforeach()

#propagate collected edges up to root CMakeLists.txt
set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
