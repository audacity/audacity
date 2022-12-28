#[[
A module to look for libuuid
]]

if( NOT libuuid_FOUND )
   find_path( libuuid_INCLUDE_DIR uuid/uuid.h )
   find_library( libuuid_LIBRARIES NAMES uuid )

   if( libuuid_INCLUDE_DIR AND libuuid_LIBRARIES )
      set( libuuid_FOUND Yes )
   endif()

   if( libuuid_FOUND )
      if( NOT libuuid_FIND_QUIETLY )
         message( STATUS "Found libuuid: \n\tlibuuid_INCLUDE_DIR: ${libuuid_INCLUDE_DIR}\n\tlibuuid_LIBRARIES: ${libuuid_LIBRARIES}" )
      endif()

      if( NOT TARGET libuuid::libuuid )
         add_library( libuuid::libuuid INTERFACE IMPORTED GLOBAL)

         target_include_directories( libuuid::libuuid INTERFACE ${libuuid_INCLUDE_DIR} )
         target_link_libraries( libuuid::libuuid INTERFACE ${libuuid_LIBRARIES} )
      endif()
   else()
      if( libuuid_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find libuuid")
      endif()
   endif()

   mark_as_advanced(
      libuuid_FOUND
      libuuid_INCLUDE_DIR
      libuuid_LIBRARIES
   )
endif()
