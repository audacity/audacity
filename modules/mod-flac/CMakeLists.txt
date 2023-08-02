set( TARGET mod-flac )

set( SOURCES
      ImportFLAC.cpp
      ExportFLAC.cpp
      FLAC.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      FLAC::FLAC
      FLAC::FLAC++
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
