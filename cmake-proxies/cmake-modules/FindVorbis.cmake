#[[
A module to look for Vorbis
]]

if( NOT Vorbis_FOUND )
   include( FindPackageHandleStandardArgs )

   find_package( Ogg QUIET )

   find_package( PkgConfig QUIET )

   pkg_check_modules( PC_Vorbis_Vorbis     QUIET vorbis     >= 1.3 )
   pkg_check_modules( PC_Vorbis_VorbisEnc  QUIET vorbisenc  >= 1.3 )
   pkg_check_modules( PC_Vorbis_VorbisFile QUIET vorbisfile >= 1.3 )

   find_path( Vorbis_Vorbis_INCLUDE_DIR vorbis/codec.h
      HINTS
		   ${PC_Vorbis_Vorbis_INCLUDE_DIRS}
         ${VORBIS_ROOT}
   )

   find_library( Vorbis_Vorbis_LIBRARIES
      NAMES
         vorbis
         vorbis_static
      HINTS
         ${PC_Vorbis_Vorbis_LIBRARY_DIRS}
         ${VORBIS_ROOT}
   )

   find_path( Vorbis_VorbisEnc_INCLUDE_DIR vorbis/vorbisenc.h
      HINTS
		   ${PC_Vorbis_VorbisEnc_INCLUDE_DIRS}
         ${VORBIS_ROOT}
   )

   find_library( Vorbis_VorbisEnc_LIBRARIES
      NAMES
         vorbisenc
         vorbisenc_static
      HINTS
         ${PC_Vorbis_VorbisEnc_LIBRARY_DIRS}
         ${VORBIS_ROOT}
   )

   find_path( Vorbis_VorbisFile_INCLUDE_DIR vorbis/vorbisfile.h
      HINTS
		   ${PC_Vorbis_VorbisFile_INCLUDE_DIRS}
         ${VORBIS_ROOT}
   )

   find_library( Vorbis_VorbisFile_LIBRARIES
      NAMES
         vorbisfile
         vorbisfile_static
      HINTS
         ${PC_Vorbis_VorbisFile_LIBRARY_DIRS}
         ${VORBIS_ROOT}
   )

   find_package_handle_standard_args(
      Vorbis
      DEFAULT_MSG
      Ogg_FOUND
      Vorbis_Vorbis_INCLUDE_DIR
      Vorbis_Vorbis_LIBRARIES
      Vorbis_VorbisEnc_INCLUDE_DIR
      Vorbis_VorbisEnc_LIBRARIES
      Vorbis_VorbisFile_INCLUDE_DIR
      Vorbis_VorbisFile_LIBRARIES
   )

   if( Vorbis_FOUND )
      if( NOT TARGET Vorbis::vorbis )
         add_library( Vorbis::vorbis INTERFACE IMPORTED GLOBAL )

         target_include_directories( Vorbis::vorbis INTERFACE ${Vorbis_Vorbis_INCLUDE_DIR} )
         target_link_libraries( Vorbis::vorbis INTERFACE ${Vorbis_Vorbis_LIBRARIES} Ogg::ogg )
      endif()

      if( NOT TARGET Vorbis::vorbisenc )
         add_library( Vorbis::vorbisenc INTERFACE IMPORTED GLOBAL )

         target_include_directories( Vorbis::vorbisenc INTERFACE ${Vorbis_VorbisEnc_INCLUDE_DIR} )
         target_link_libraries( Vorbis::vorbisenc INTERFACE ${Vorbis_VorbisEnc_LIBRARIES} Vorbis::vorbis )
      endif()

      if( NOT TARGET Vorbis::vorbisfile )
         add_library( Vorbis::vorbisfile INTERFACE IMPORTED GLOBAL )

         target_include_directories( Vorbis::vorbisfile INTERFACE ${Vorbis_VorbisFile_INCLUDE_DIR} )
         target_link_libraries( Vorbis::vorbisfile INTERFACE ${Vorbis_VorbisFile_LIBRARIES} Vorbis::vorbis )
      endif()

      mark_as_advanced(
         Vorbis_FOUND
         Vorbis_Vorbis_INCLUDE_DIR
         Vorbis_Vorbis_LIBRARIES
         Vorbis_VorbisEnc_INCLUDE_DIR
         Vorbis_VorbisEnc_LIBRARIES
         Vorbis_VorbisFile_INCLUDE_DIR
         Vorbis_VorbisFile_LIBRARIES
      )
   endif()
endif()
