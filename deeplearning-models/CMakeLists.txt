set( TARGET deeplearning-models )
set( TARGET_ROOT ${topdir}/deeplearning-models )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

set( SOURCES
   resampler.pt
   modelcard-schema.json
)

set ( OUTPUTS )
foreach( source ${SOURCES} )
   set( src "${_SRCDIR}/${source}" )
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows" )
      set( dst "${_DEST}/${TARGET}/${source}" )
   else()
      set( dst "${CMAKE_BINARY_DIR}/lib/${TARGET}/${source}" )
   endif()

   add_custom_command(
      DEPENDS
         "${src}"
      COMMAND
         ${CMAKE_COMMAND} -E make_directory "${_DEST}/${TARGET}"
      COMMAND
         ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
      OUTPUT
         "${dst}"
   )

   list( APPEND OUTPUTS "${dst}" )
endforeach()

add_custom_target( ${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES} )

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY "${_DEST}/${TARGET}"
               DESTINATION "${_PKGDATA}" )
   endif()
endif()
