#[[
A module to look for portmidi
]]

if( NOT portmidi_FOUND )
   find_path( portmidi_INCLUDE_DIR portmidi.h )
   find_library( portmidi_LIBRARIES NAMES portmidi )

   if( portmidi_INCLUDE_DIR AND portmidi_LIBRARIES )
      set( portmidi_FOUND Yes )
   endif()

   if( portmidi_FOUND )
      if( NOT portmidi_FIND_QUIETLY )
         message( STATUS "Found portmidi: \n\tportmidi_INCLUDE_DIR: ${portmidi_INCLUDE_DIR}\n\tportmidi_LIBRARIES: ${portmidi_LIBRARIES}" )
      endif()

      if( NOT TARGET portmidi::portmidi )
         add_library( portmidi::portmidi INTERFACE IMPORTED GLOBAL)

         target_include_directories( portmidi::portmidi INTERFACE ${portmidi_INCLUDE_DIR} )
         target_link_libraries( portmidi::portmidi INTERFACE ${portmidi_LIBRARIES} )

         add_library(portmidi ALIAS portmidi::portmidi)
      endif()
   else()
      if( portmidi_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find portmidi")
      endif()
   endif()

   mark_as_advanced(
      portmidi_FOUND
      portmidi_INCLUDE_DIR
      portmidi_LIBRARIES
   )
endif()
