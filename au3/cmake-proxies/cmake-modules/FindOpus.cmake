#[[
A module to look for Opus
]]

if( NOT Opus_FOUND )
   include( FindPackageHandleStandardArgs)

   find_package( Ogg QUIET )

   find_package( PkgConfig QUIET )
   pkg_check_modules( PC_Opus QUIET opus >= 1.3 )

   find_path( Opus_INCLUDE_DIR opus/opus.h
      HINTS
		   ${PC_Opus_INCLUDE_DIRS}
         ${OPUS_ROOT}
   )

   find_library( Opus_LIBRARIES
      NAMES
         opus
         opus_static
      HINTS
         ${PC_Opus_LIBRARY_DIRS}
         ${OPUS_ROOT}
   )

   find_package_handle_standard_args( Opus DEFAULT_MSG Ogg_FOUND Opus_INCLUDE_DIR Opus_LIBRARIES )

   if( Opus_FOUND )
      if( NOT TARGET Opus::opus )
         add_library( Opus::opus INTERFACE IMPORTED GLOBAL )

         target_include_directories( Opus::opus INTERFACE ${Opus_INCLUDE_DIR} )
         target_link_libraries( Opus::opus INTERFACE ${Opus_LIBRARIES} Ogg::ogg )
      endif()

      mark_as_advanced(
         Opus_FOUND
         Opus_INCLUDE_DIR
         Opus_LIBRARIES
      )
   endif()
endif()
