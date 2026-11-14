message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( FILES "${_SRCDIR}/audacity.appdata.xml"
               DESTINATION "${_DATADIR}/metainfo" )
   endif()
endif()
