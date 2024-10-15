#[[
A subdirectory containing module targets
]]

# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
   mod-mp3
   mod-pcm
   mod-cl
   mod-lof
   mod-aup
)

if ( USE_LIBOGG AND USE_LIBVORBIS )
   list( APPEND MODULES mod-ogg )
endif()

if ( USE_LIBFLAC )
   list( APPEND MODULES mod-flac )
endif()

if ( USE_LIBTWOLAME )
   list ( APPEND MODULES mod-mp2 )
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

if ( USE_LIBOPUS AND USE_OPUSFILE AND USE_LIBOGG )
   list ( APPEND MODULES mod-opus )
endif()

audacity_module_subdirectory("${MODULES}")
