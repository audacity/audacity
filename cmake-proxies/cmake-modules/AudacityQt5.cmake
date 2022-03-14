#[[
   Qt5 helper functions for QAudacity.

   The functions provided with Qt CMake modules do no set up PATH properly.
]]

if( ${_OPT}conan_enabled )
   include(conan_qt_executables_variables)
endif()

set(_QT5_INTERNAL_SCOPE Yes)

get_target_property(rcc_path Qt5::rcc IMPORTED_LOCATION)
get_target_property(moc_path Qt5::moc IMPORTED_LOCATION)
get_target_property(uic_path Qt5::uic IMPORTED_LOCATION)

if (WIN32)
   string(REPLACE ";" "\\;" escaped_path "$ENV{PATH}")
   set( QTUTILS_PATH "$<SHELL_PATH:${_SHARED_PROXY_BASE_PATH}/$<CONFIG>>" )
   set( cmd_prefix ${CMAKE_COMMAND} -E env PATH=${QTUTILS_PATH} )
elseif(APPLE)
   set( cmd_prefix ${CMAKE_COMMAND} -E env DYLD_FALLBACK_LIBRARY_PATH=$<SHELL_PATH:${_SHARED_PROXY_BASE_PATH}/$<CONFIG>> )
endif()

function( audacity_qt_add_resource outfiles resource )
   get_filename_component(outfilename ${resource} NAME_WE)
   get_filename_component(infile ${resource} ABSOLUTE)

   file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/qrc)
   set(outfile ${CMAKE_BINARY_DIR}/qrc/${outfilename}.cpp)

   get_filename_component(outfile ${outfile} ABSOLUTE)

   _qt5_parse_qrc_file(${infile} _out_depends _rc_depends)

   add_custom_command(OUTPUT ${outfile}
      COMMAND
            ${cmd_prefix}
            ${rcc_path}
               --name ${outfilename}
               --output ${outfile} ${infile}
               MAIN_DEPENDENCY ${infile}
               DEPENDS ${_rc_depends} "${_out_depends}"
               VERBATIM
   )

   set_source_files_properties(${infile} PROPERTIES SKIP_AUTORCC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTORCC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOMOC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOUIC ON)

   list(APPEND ${outfiles} ${outfile})
   set(${outfiles} ${${outfiles}} PARENT_SCOPE)
endfunction()

function( audacity_qt_wrap_ui outfiles resource )
   get_filename_component(outfilename ${resource} NAME_WE)
   get_filename_component(infile ${resource} ABSOLUTE)

   set(outfile ${CMAKE_BINARY_DIR}/uic/ui_${outfilename}.h)
   get_filename_component(outfile ${outfile} ABSOLUTE)


   add_custom_command(OUTPUT ${outfile}
      COMMAND
            ${cmd_prefix}
            ${uic_path}
               --name ${outfilename}
               --output ${outfile} ${infile}
               MAIN_DEPENDENCY ${infile}
               #VERBATIM
   )

   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTORCC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOMOC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOUIC ON)
   set_source_files_properties(${infile} PROPERTIES SKIP_AUTOUIC ON)

   list(APPEND ${outfiles} ${outfile})
   set(${outfiles} ${${outfiles}} PARENT_SCOPE)
endfunction()

function(audacity_qt_create_moc_command infile outfile moc_flags)
   _qt5_warn_deprecated("qt5_create_moc_command")

   get_filename_component(_moc_outfile_name "${outfile}" NAME)
   get_filename_component(_moc_outfile_dir "${outfile}" PATH)

   if(_moc_outfile_dir)
      set(_moc_working_dir WORKING_DIRECTORY ${_moc_outfile_dir})
   endif()

   set (_moc_parameters_file ${outfile}_parameters)
   set (_moc_parameters ${moc_flags} ${moc_options} -o "${outfile}" "${infile}")
   string (REPLACE ";" "\n" _moc_parameters "${_moc_parameters}")

   file(WRITE ${_moc_parameters_file} "${_moc_parameters}\n")

   set(_moc_extra_parameters_file @${_moc_parameters_file})

   add_custom_command(OUTPUT ${outfile}
                     COMMAND ${Qt5Core_MOC_EXECUTABLE} ${_moc_extra_parameters_file}
                     DEPENDS ${infile}
                     ${_moc_working_dir}
                     VERBATIM)

                     set_source_files_properties(${infile} PROPERTIES SKIP_AUTOMOC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOMOC ON)
   set_source_files_properties(${outfile} PROPERTIES SKIP_AUTOUIC ON)
endfunction()

function(audacity_qt_wrap_cpp outfiles infile)
   qt5_get_moc_flags(moc_flags)

   get_filename_component(infile ${infile} ABSOLUTE)

   qt5_make_output_file(${infile} moc_ cpp outfile)
   audacity_qt_create_moc_command(${infile} ${outfile} "${moc_flags}" "${moc_options}")

   list(APPEND ${outfiles} ${outfile})

   set(${outfiles} ${${outfiles}} PARENT_SCOPE)
endfunction()
