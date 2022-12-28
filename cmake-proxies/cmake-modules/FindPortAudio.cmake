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

      if( NOT TARGET portaudio::portaudio )
         add_library( portaudio::portaudio INTERFACE IMPORTED GLOBAL)

         target_include_directories( portaudio::portaudio INTERFACE ${PortAudio_INCLUDE_DIR} )
         target_link_libraries( portaudio::portaudio INTERFACE ${PortAudio_LIBRARIES} )

         add_library(portaudio ALIAS portaudio::portaudio)
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
