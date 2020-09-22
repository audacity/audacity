#[[
Adds FLAC import and export
]]

if ( USE_LIBFLAC )

   set( SOURCES
      ExportFLAC.cpp
      ImportFLAC.cpp
   )

   set( LIBRARIES
      Audacity
      FLAC::FLAC
      FLAC::FLAC++
   )

   if ( USE_LIBID3TAG )
      list( APPEND LIBRARIES
         libid3tag::libid3tag
      )
   endif()

   audacity_module( mod-flac "${SOURCES}" "${LIBRARIES}"
      "" "" )

   if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
      list( APPEND MIMETYPES
         audio/flac
         audio/x-flac
      )
      set( MIMETYPES "${MIMETYPES}" PARENT_SCOPE )
   endif()

endif()
