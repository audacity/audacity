#[[
A module to look for PortAudio
]]

if( NOT PortAudio_FOUND )
   find_path( PortAudio_INCLUDE_DIR portaudio.h )
   find_library( PortAudio_LIBRARIES NAMES portaudio )

   if( PortAudio_INCLUDE_DIR AND PortAudio_LIBRARIES )
      set( PortAudio_FOUND Yes )
   endif()

   if( PortAudio_FOUND )
      if( NOT PortAudio_FIND_QUIETLY )
         message( STATUS "Found PortAudio: \n\tPortAudio_INCLUDE_DIR: ${PortAudio_INCLUDE_DIR}\n\tPortAudio_LIBRARIES: ${PortAudio_LIBRARIES}" )
      endif()

      if( NOT TARGET PortAudio::PortAudio )
         add_library( PortAudio::PortAudio INTERFACE IMPORTED GLOBAL)

         target_include_directories( PortAudio::PortAudio INTERFACE ${PortAudio_INCLUDE_DIR} )
         target_link_libraries( PortAudio::PortAudio INTERFACE ${PortAudio_LIBRARIES} )

         add_library(portaudio ALIAS PortAudio::PortAudio)
      endif()
   else()
      if( PortAudio_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find PortAudio")
      endif()
   endif()

   mark_as_advanced(
      PortAudio_FOUND
      PortAudio_INCLUDE_DIR
      PortAudio_LIBRARIES
   )
endif()
