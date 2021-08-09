#[[
A module to look for PortMidi
]]

if( NOT PortMidi_FOUND )
   find_path( PortMidi_INCLUDE_DIR portmidi.h )
   find_library( PortMidi_LIBRARIES NAMES portmidi )

   if( PortMidi_INCLUDE_DIR AND PortMidi_LIBRARIES )
      set( PortMidi_FOUND Yes )
   endif()

   if( PortMidi_FOUND )
      if( NOT PortMidi_FIND_QUIETLY )
         message( STATUS "Found PortMidi: \n\tPortMidi_INCLUDE_DIR: ${PortMidi_INCLUDE_DIR}\n\tPortMidi_LIBRARIES: ${PortMidi_LIBRARIES}" )
      endif()

      if( NOT TARGET PortMidi::PortMidi )
         add_library( PortMidi::PortMidi INTERFACE IMPORTED GLOBAL)

         target_include_directories( PortMidi::PortMidi INTERFACE ${PortMidi_INCLUDE_DIR} )
         target_link_libraries( PortMidi::PortMidi INTERFACE ${PortMidi_LIBRARIES} )

         add_library(portmidi ALIAS PortMidi::PortMidi)
      endif()
   else()
      if( PortMidi_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find PortMidi")
      endif()
   endif()

   mark_as_advanced(
      PortMidi_FOUND
      PortMidi_INCLUDE_DIR
      PortMidi_LIBRARIES
   )
endif()
