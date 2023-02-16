#[[
A module to look for mpg123
]]

if( NOT mpg123_FOUND )
   find_path( mpg123_INCLUDE_DIR mpg123.h )
   find_library( mpg123_LIBRARIES NAMES mpg123 )

   if( mpg123_INCLUDE_DIR AND mpg123_LIBRARIES )
      set( mpg123_FOUND Yes )
   endif()

   if( mpg123_FOUND )
      if( NOT mpg123_FIND_QUIETLY )
         message( STATUS "Found mpg123: \n\tmpg123_INCLUDE_DIR: ${mpg123_INCLUDE_DIR}\n\tmpg123_LIBRARIES: ${mpg123_LIBRARIES}" )
      endif()

      if( NOT TARGET mpg123::libmpg123 )
         add_library( mpg123::libmpg123 INTERFACE IMPORTED GLOBAL)

         target_include_directories( mpg123::libmpg123 INTERFACE ${mpg123_INCLUDE_DIR} )
         target_link_libraries( mpg123::libmpg123 INTERFACE ${mpg123_LIBRARIES} )
      endif()
   else()
      if( mpg123_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find mpg123")
      endif()
   endif()

   mark_as_advanced(
      mpg123_FOUND
      mpg123_INCLUDE_DIR
      mpg123_LIBRARIES
   )
endif()
