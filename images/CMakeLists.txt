
set( TARGET images )
set( TARGET_ROOT ${topdir}/images )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( FILES "${_SRCDIR}/audacity.svg"
               DESTINATION "${_DATADIR}/icons/hicolor/scalable/apps" )
      install( DIRECTORY "${_SRCDIR}/icons/"
               DESTINATION "${_DATADIR}/icons/hicolor"
               FILES_MATCHING PATTERN "*.png" )
   endif()
endif()

