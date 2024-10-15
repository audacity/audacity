#[[
A module to look for libmp3lame
]]

if( NOT libmp3lame_FOUND )
   find_path( libmp3lame_INCLUDE_DIR lame/lame.h )
   find_library( libmp3lame_LIBRARIES NAMES mp3lame )

   if( libmp3lame_INCLUDE_DIR AND libmp3lame_LIBRARIES )
      set( libmp3lame_FOUND Yes )
   endif()

   if( libmp3lame_FOUND )
      if( NOT libmp3lame_FIND_QUIETLY )
         message( STATUS "Found lame: \n\tlibmp3lame_INCLUDE_DIR: ${libmp3lame_INCLUDE_DIR}\n\tlibmp3lame_LIBRARIES: ${libmp3lame_LIBRARIES}" )
      endif()

      if( NOT TARGET libmp3lame::libmp3lame )
         add_library( libmp3lame::libmp3lame INTERFACE IMPORTED GLOBAL)

         target_include_directories( libmp3lame::libmp3lame INTERFACE ${libmp3lame_INCLUDE_DIR} )
         target_link_libraries( libmp3lame::libmp3lame INTERFACE ${libmp3lame_LIBRARIES} )
      endif()
   else()
      if( libmp3lame_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find libmp3lame")
      endif()
   endif()

   mark_as_advanced(
      libmp3lame_FOUND
      libmp3lame_INCLUDE_DIR
      libmp3lame_LIBRARIES
   )
endif()
