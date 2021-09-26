#[[
A module to look for SndFile
]]

if( NOT SndFile_FOUND )
   include( FindPackageHandleStandardArgs )

   find_package( PkgConfig QUIET)
   pkg_check_modules( PC_SndFile QUIET sndfile )

   find_path( SndFile_INCLUDE_DIR sndfile.h
      HINTS
		   ${PC_SndFile_INCLUDE_DIRS}
         ${OGG_ROOT}
   )

   find_library( SndFile_LIBRARIES
      NAMES
         sndfile
         sndfile_static
      HINTS
         ${PC_SndFile_LIBRARY_DIRS}
         ${OGG_ROOT}
   )

   find_package_handle_standard_args( SndFile DEFAULT_MSG SndFile_INCLUDE_DIR SndFile_LIBRARIES )

   if( SndFile_FOUND )
      if( NOT TARGET SndFile::sndfile )
         add_library( SndFile::sndfile INTERFACE IMPORTED GLOBAL )

         target_include_directories( SndFile::sndfile INTERFACE ${SndFile_INCLUDE_DIR} )
         target_link_libraries( SndFile::sndfile INTERFACE ${SndFile_LIBRARIES} )
      endif()

      mark_as_advanced(
         SndFile_FOUND
         SndFile_INCLUDE_DIR
         SndFile_LIBRARIES
      )
   endif()
endif()
