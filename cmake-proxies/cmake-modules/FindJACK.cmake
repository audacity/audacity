#[[
A module to look for JACK
]]

if( NOT JACK_FOUND )
   include( FindPackageHandleStandardArgs )

   find_package( PkgConfig QUIET)
   pkg_check_modules( PC_JACK QUIET jack )

   find_path( JACK_INCLUDE_DIR jack/jack.h
      HINTS
        ${PC_JACK_INCLUDE_DIRS}
        ${JACK_ROOT}
   )

   find_library( JACK_LIBRARIES
      NAMES
        jack
      HINTS
        ${PC_JACK_LIBRARY_DIRS}
        ${JACK_ROOT}
   )

   find_package_handle_standard_args( JACK DEFAULT_MSG JACK_INCLUDE_DIR JACK_LIBRARIES )

   if( JACK_FOUND )
      if( NOT TARGET JACK::jack )
        add_library( JACK::jack INTERFACE IMPORTED GLOBAL )

        target_include_directories( JACK::jack INTERFACE ${JACK_INCLUDE_DIR} )
        target_link_libraries( JACK::jack INTERFACE ${JACK_LIBRARIES} )
      endif()

      mark_as_advanced(
        JACK_FOUND
        JACK_INCLUDE_DIR
        JACK_LIBRARIES
      )
   endif()
endif()
