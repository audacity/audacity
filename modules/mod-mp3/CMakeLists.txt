#[[
Adds MP3 import and export
]]

set( SOURCES
   ExportMP3.cpp
   ExportMP3.h
   ImportMP3_MAD.cpp
   ImportMP3_MPG123.cpp
   MP3Prefs.cpp
)

set( LIBRARIES
   Audacity
   libmp3lame::libmp3lame
   $<$<BOOL:${USE_LIBMPG123}>:mpg123::libmpg123>
)

# Do not define these for Windows or Mac until further testing
# can be done against ExportMP3.cpp.  If either are defined, the
# build will fail.
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
   if( "${${_OPT}use_lame}" STREQUAL "local" )
      set( DISABLE_DYNAMIC_LOADING_LAME YES )
   endif()
endif()

if ( USE_LIBMAD )

   list( APPEND SOURCES
      ImportMP3_MAD.cpp
   )

   list( APPEND LIBRARIES
      libmad::libmad
   )

   if ( USE_LIBID3TAG )
      list( APPEND LIBRARIES
         libid3tag::libid3tag
      )
   endif()

endif()

if (USE_LIBMAD OR USE_LIBMPG123)
   if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
      list( APPEND MIMETYPES
         audio/mpeg
      )
      set( MIMETYPES "${MIMETYPES}" PARENT_SCOPE )
   endif()
endif()

audacity_module( mod-mp3 "${SOURCES}" "${LIBRARIES}"
   "" "" )
