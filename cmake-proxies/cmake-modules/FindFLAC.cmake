#[[
A module to look for FLAC
]]

if( NOT FLAC_FOUND )
   include( FindPackageHandleStandardArgs )

   find_package( Ogg QUIET )

   find_package( PkgConfig QUIET )
   pkg_check_modules( PC_FLAC QUIET flac >= 1.3.1 )

   find_path( FLAC_INCLUDE_DIR FLAC/stream_decoder.h
      HINTS
		   ${PC_FLAC_INCLUDE_DIRS}
         ${FLAC_ROOT}
   )

   find_library( FLAC_LIBRARIES
      NAMES
         FLAC
         FLAC_static
      HINTS
         ${PC_FLAC_LIBRARY_DIRS}
         ${FLAC_ROOT}
   )

   find_path( FLACPP_INCLUDE_DIR FLAC++/decoder.h
      HINTS
		   ${PC_FLAC_INCLUDE_DIRS}
         ${FLAC_ROOT}
   )

   find_library( FLACPP_LIBRARIES
      NAMES
         FLAC++
         FLAC++_static
      HINTS
         ${PC_FLAC_LIBRARY_DIRS}
         ${FLAC_ROOT}
   )

   find_package_handle_standard_args( FLAC DEFAULT_MSG Ogg_FOUND FLAC_INCLUDE_DIR FLAC_LIBRARIES )

   if( FLAC_FOUND )
      if( NOT TARGET FLAC::FLAC )
         add_library( FLAC::FLAC INTERFACE IMPORTED GLOBAL )

         target_include_directories( FLAC::FLAC INTERFACE ${FLAC_INCLUDE_DIR} )
         target_link_libraries( FLAC::FLAC INTERFACE ${FLAC_LIBRARIES} Ogg::ogg )
      endif()

      if( NOT TARGET FLAC::FLAC++ )
         add_library( FLAC::FLAC++ INTERFACE IMPORTED GLOBAL )

         target_include_directories( FLAC::FLAC++ INTERFACE ${FLACPP_INCLUDE_DIR} )
         target_link_libraries( FLAC::FLAC++ INTERFACE ${FLACPP_LIBRARIES} FLAC::FLAC)
      endif()

      mark_as_advanced(
         FLAC_FOUND
         FLAC_INCLUDE_DIR
         FLAC_LIBRARIES
         FLACPP_INCLUDE_DIR
         FLACPP_LIBRARIES
      )
   endif()
endif()
