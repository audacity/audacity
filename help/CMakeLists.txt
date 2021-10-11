
set( TARGET manual )
set( TARGET_ROOT ${topdir}/manual )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

if( NOT DEFINED PYTHON )
   message( WARNING "Python not found...unable to produce manual." )
   return()
endif()

set( dst "${_DEST}/help/manual" )
set( out "${CMAKE_BINARY_DIR}/help/manual/index.html" )

set(manual_version "${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}")

add_custom_command(
   COMMENT
      "Downloading manual..."
   COMMAND
      ${CMAKE_COMMAND}
         -D VERSION=${manual_version}
         -D DESTINATION=${CMAKE_BINARY_DIR}
         -P "${CMAKE_SOURCE_DIR}/help/DownloadManual.cmake"
   COMMAND
      ${CMAKE_COMMAND}
         -E tar "xf" "audacity-manual-${manual_version}.tar.gz"
   COMMAND
      ${CMAKE_COMMAND} -E copy_directory "help/manual" "${dst}"

   WORKING_DIRECTORY
      ${CMAKE_BINARY_DIR}
   OUTPUT
      "${out}"
)

add_custom_target( ${TARGET} DEPENDS "${out}" )

if( ${_OPT}package_manual )
   install(CODE "                                     \
      execute_process(                                \
         COMMAND                                      \
            ${CMAKE_COMMAND}                          \
               --build ${CMAKE_BINARY_DIR}            \
               --config \${CMAKE_INSTALL_CONFIG_NAME} \
               --target manual                        \
      )                                               \
   ")

   if( "${CMAKE_GENERATOR}" MATCHES "Xcode" )
      install( DIRECTORY "${CMAKE_BINARY_DIR}/help/manual" DESTINATION "${_APPDIR}/help/manual" )
   elseif( "${CMAKE_GENERATOR}" MATCHES "Visual Studio*" )
      install( DIRECTORY "${CMAKE_BINARY_DIR}/help/manual" DESTINATION "help/manual" )
   endif()
endif()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY "${dst}" OPTIONAL
               DESTINATION "${_DATADIR}/audacity/help" )
      install( FILES "${_SRCDIR}/audacity.1"
               DESTINATION "${_MANDIR}/man1" )
      install( FILES "${_SRCDIR}/audacity.appdata.xml"
               DESTINATION "${_DATADIR}/metainfo" )
   endif()
endif()
