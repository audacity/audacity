
set( TARGET manual )
set( TARGET_ROOT ${topdir}/help )

message( STATUS "========== Configuring ${TARGET} ==========" )

set( host "alphamanual.audacityteam.org" )
set( src "https://${host}/man" )
set( dst "$<TARGET_FILE_DIR:Audacity>/help/manual" )
set( script_dir "${top_dir}/scripts/mw2html_audacity" )
set( script "mw2html.py" )
set( out_dir "${CMAKE_CURRENT_BINARY_DIR}" )
set( out "${out_dir}/${host}/index.html" )

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   file( TO_NATIVE_PATH
      "${CMAKE_BINARY_DIR}/packages/${PYTHON_NAME}.${PYTHON_VERSION}/tools/python.exe"
      python
   )
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
add_dependencies( ${TARGET} Audacity )
