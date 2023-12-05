#[[
A module to look for Opus
]]

if( NOT opusfile_FOUND )
   include( FindPackageHandleStandardArgs)

   find_path( opusfile_INCLUDE_DIR opus/opusfile.h
      HINTS
         ${OPUSFILE_ROOT}
   )

   find_library( opusfile_LIBRARIES
      NAMES
         opusfile
      HINTS
         ${OPUSFILE_ROOT}
   )

   find_package_handle_standard_args( opusfile DEFAULT_MSG opusfile_INCLUDE_DIR opusfile_LIBRARIES )

   if( opusfile_FOUND )
      if( NOT TARGET opusfile::opusfile )
         add_library( opusfile::opusfile INTERFACE IMPORTED GLOBAL )

         target_include_directories( opusfile::opusfile INTERFACE ${opusfile_INCLUDE_DIR} "${opusfile_INCLUDE_DIR}/opus" )
         target_link_libraries( opusfile::opusfile INTERFACE ${opusfile_LIBRARIES} )
      endif()

      mark_as_advanced(
         opusfile_FOUND
         opusfile_INCLUDE_DIR
         opusfile_LIBRARIES
      )
   endif()
endif()
