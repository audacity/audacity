#[[
A module to look for libmad
]]

if( NOT libmad_FOUND )
   find_path( libmad_INCLUDE_DIR mad.h )
   find_library( libmad_LIBRARIES NAMES mad )

   if( libmad_INCLUDE_DIR AND libmad_LIBRARIES )
      set( libmad_FOUND Yes )
   endif()

   if( libmad_FOUND )
      if( NOT libmad_FIND_QUIETLY )
         message( STATUS "Found mad: \n\tlibmad_INCLUDE_DIR: ${libmad_INCLUDE_DIR}\n\tlibmad_LIBRARIES: ${libmad_LIBRARIES}" )
      endif()

      if( NOT TARGET libmad::libmad )
         add_library( libmad::libmad INTERFACE IMPORTED GLOBAL)

         target_include_directories( libmad::libmad INTERFACE ${libmad_INCLUDE_DIR} )
         target_link_libraries( libmad::libmad INTERFACE ${libmad_LIBRARIES} )
      endif()
   else()
      if( libmad_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find libmad")
      endif()
   endif()

   mark_as_advanced(
      libmad_FOUND
      libmad_INCLUDE_DIR
      libmad_LIBRARIES
   )
endif()
