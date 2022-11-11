#[[
A module to look for wavpack
]]

if( NOT wavpack_FOUND )
   find_path( wavpack_INCLUDE_DIR wavpack/wavpack.h )
   find_library( wavpack_LIBRARIES NAMES wavpack )

   if( wavpack_INCLUDE_DIR AND wavpack_LIBRARIES )
      set( wavpack_FOUND Yes )
   endif()

   if( wavpack_FOUND )
      if( NOT wavpack_FIND_QUIETLY )
         message( STATUS "Found wavpack: \n\twavpack_INCLUDE_DIR: ${wavpack_INCLUDE_DIR}\n\twavpack_LIBRARIES: ${wavpack_LIBRARIES}" )
      endif()

      if( NOT TARGET wavpack::wavpack )
         add_library( wavpack::wavpack INTERFACE IMPORTED GLOBAL)

         target_include_directories( wavpack::wavpack INTERFACE ${wavpack_INCLUDE_DIR} )
         target_link_libraries( wavpack::wavpack INTERFACE ${wavpack_LIBRARIES} )
      endif()
   else()
      if( wavpack_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find wavpack")
      endif()
   endif()

   mark_as_advanced(
      wavpack_FOUND
      wavpack_INCLUDE_DIR
      wavpack_LIBRARIES
   )
endif()
