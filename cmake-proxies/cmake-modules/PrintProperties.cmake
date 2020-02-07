#
# From https://stackoverflow.com/a/51987470 but modified
#
function(print_properties typ tgt)
   message("Properties for ${typ} ${tgt}:")

   # Get all propreties that cmake supports
   execute_process(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)

   # Convert command output into a CMake list
   STRING(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
   STRING(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

   # Fix https://stackoverflow.com/questions/32197663/how-can-i-remove-the-the-location-property-may-not-be-read-from-target-error-i
   list(FILTER CMAKE_PROPERTY_LIST EXCLUDE REGEX "^LOCATION$|^LOCATION_|_LOCATION$")

   # For some reason, "TYPE" shows up twice - others might too?
   list(REMOVE_DUPLICATES CMAKE_PROPERTY_LIST)

   # build whitelist by filtering down from CMAKE_PROPERTY_LIST in case cmake is
   # a different version, and one of our hardcoded whitelisted properties
   # doesn't exist!
   unset(CMAKE_WHITELISTED_PROPERTY_LIST)
   foreach(prop ${CMAKE_PROPERTY_LIST})
      if(prop MATCHES "^(INTERFACE|[_a-z]|IMPORTED_LIBNAME_|MAP_IMPORTED_CONFIG_)|^(COMPATIBLE_INTERFACE_(BOOL|NUMBER_MAX|NUMBER_MIN|STRING)|EXPORT_NAME|IMPORTED(_GLOBAL|_CONFIGURATIONS|_LIBNAME)?|NAME|TYPE|NO_SYSTEM_FROM_IMPORTED)$")
         list(APPEND CMAKE_WHITELISTED_PROPERTY_LIST ${prop})
      endif()
   endforeach()

   set(PROP_LIST ${CMAKE_PROPERTY_LIST})
   if( typ MATCHES "TARGET" )
      get_target_property(target_type ${tgt} TYPE)
      if(target_type STREQUAL "INTERFACE_LIBRARY")
         set(PROP_LIST ${CMAKE_WHITELISTED_PROPERTY_LIST})
      endif()
   endif()

   foreach (prop ${PROP_LIST})
      string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
      get_property(propval ${typ} ${tgt} PROPERTY ${prop} SET)
      if (propval)
         get_property(propval ${typ} ${tgt} PROPERTY ${prop})
         if (NOT propval STREQUAL "propval-NOTFOUND")
            message ("  ${prop} = ${propval}")
         endif()
      endif()
   endforeach()
endfunction()

