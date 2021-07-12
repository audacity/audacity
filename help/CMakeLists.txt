
set( TARGET manual )
set( TARGET_ROOT ${topdir}/manual )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

if( NOT DEFINED PYTHON )
   message( WARNING "Python not found...unable to produce manual." )
   return()
endif()

set( host "alphamanual.audacityteam.org" )
set( src "https://${host}/man" )
set( dst "${_DEST}/help/manual" )
set( script_dir "${topdir}/scripts/mw2html_audacity" )
set( script "mw2html.py" )
set( out_dir "${_INTDIR}" )
set( out "${out_dir}/${host}/index.html" )

add_custom_command(
   COMMENT
      "Downloading manual from: ${src}"
   COMMAND
      "${PYTHON}" "${script_dir}/${script}" -s "${src}" "${out_dir}"
   COMMAND
      ${CMAKE_COMMAND} -E copy_directory "${out_dir}/${host}" "${dst}"
   WORKING_DIRECTORY
      "${script_dir}"
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
      install( DIRECTORY "${out_dir}/${host}/" DESTINATION "${_APPDIR}/help/manual" )
   elseif( "${CMAKE_GENERATOR}" MATCHES "Visual Studio*" )
      install( DIRECTORY "${out_dir}/${host}/" DESTINATION "help/manual" )
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
