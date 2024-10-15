#[[
A module to look for Ogg
]]

if( NOT Ogg_FOUND )
   include( FindPackageHandleStandardArgs )

   find_package( PkgConfig QUIET)
   pkg_check_modules( PC_Ogg QUIET ogg >= 1.3.1 )

   find_path( Ogg_INCLUDE_DIR ogg/ogg.h
      HINTS
		   ${PC_Ogg_INCLUDE_DIRS}
         ${OGG_ROOT}
   )

   find_library( Ogg_LIBRARIES
      NAMES
         ogg
         ogg_static
      HINTS
         ${PC_Ogg_LIBRARY_DIRS}
         ${OGG_ROOT}
   )

   find_package_handle_standard_args( Ogg DEFAULT_MSG Ogg_INCLUDE_DIR Ogg_LIBRARIES )

   if( Ogg_FOUND )
      if( NOT TARGET Ogg::ogg )
         add_library( Ogg::ogg INTERFACE IMPORTED GLOBAL )

         target_include_directories( Ogg::ogg INTERFACE ${Ogg_INCLUDE_DIR} )
         target_link_libraries( Ogg::ogg INTERFACE ${Ogg_LIBRARIES} )
      endif()

      mark_as_advanced(
         Ogg_FOUND
         Ogg_INCLUDE_DIR
         Ogg_LIBRARIES
      )
   endif()
endif()
