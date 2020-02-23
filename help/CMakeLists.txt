
set( TARGET manual )
set( TARGET_ROOT ${topdir}/manual )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

set( host "alphamanual.audacityteam.org" )
set( src "https://${host}/man" )
set( dst "${_DEST}/help/manual" )
set( script_dir "${topdir}/scripts/mw2html_audacity" )
set( script "mw2html.py" )
set( out_dir "${_INTDIR}" )
set( out "${out_dir}/${host}/index.html" )

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   nuget_package( pkgdir "python2" "2.7.17" )
   file( TO_NATIVE_PATH "${pkgdir}/tools/python.exe" python )
else()
   find_package( Python2 )
   if( Python2_FOUND )
      set( python "${Python2_EXECUTABLE}" )
   endif()
endif()

if( NOT DEFINED python )
   message( WARNING "Python not found...unable to produce manual." )
   return()
endif()

add_custom_command(
   COMMENT
      "Downloading manual from: ${src}"
   COMMAND
      "${python}" "${script_dir}/${script}" -s "${src}" "${out_dir}"
   COMMAND
      ${CMAKE_COMMAND} -E copy_directory "${out_dir}/${host}" "${dst}"
   WORKING_DIRECTORY
      "${script_dir}"
   OUTPUT
      "${out}"
)

add_custom_target( ${TARGET} DEPENDS "${out}" )

if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*" )
   install( DIRECTORY "${dst}" OPTIONAL
            DESTINATION "${_DATADIR}/audacity/help" )
   install( FILES "${_SRCDIR}/audacity.1"
            DESTINATION "${_DATADIR}/man/man.1" )
   install( FILES "${_SRCDIR}/audacity.appdata.xml"
            DESTINATION "${_DATADIR}/appdata" )
endif()

