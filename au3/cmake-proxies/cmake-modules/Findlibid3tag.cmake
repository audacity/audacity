#[[
A module to look for libid3tag
]]

if( NOT libid3tag_FOUND )
   find_path( libid3tag_INCLUDE_DIR id3tag.h )
   find_library( libid3tag_LIBRARIES NAMES id3tag )

   if( libid3tag_INCLUDE_DIR AND libid3tag_LIBRARIES )
      set( libid3tag_FOUND Yes )
   endif()

   if( libid3tag_FOUND )
      if( NOT libid3tag_FIND_QUIETLY )
         message( STATUS "Found libid3tag: \n\tlibid3tag_INCLUDE_DIR: ${libid3tag_INCLUDE_DIR}\n\tlibid3tag_LIBRARIES: ${libid3tag_LIBRARIES}" )
      endif()

      if( NOT TARGET libid3tag::libid3tag )
         add_library( libid3tag::libid3tag INTERFACE IMPORTED GLOBAL)

         target_include_directories( libid3tag::libid3tag INTERFACE ${libid3tag_INCLUDE_DIR} )
         target_link_libraries( libid3tag::libid3tag INTERFACE ${libid3tag_LIBRARIES} )
      endif()
   else()
      if( libid3tag_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find libid3tag")
      endif()
   endif()

   mark_as_advanced(
      libid3tag_FOUND
      libid3tag_INCLUDE_DIR
      libid3tag_LIBRARIES
   )
endif()
