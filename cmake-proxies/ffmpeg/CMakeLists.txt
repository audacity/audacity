
# Add our target and all of it's aliases
add_library( ${TARGET} INTERFACE )
add_library( ${symbol} ALIAS ${TARGET} )
add_library( libavcodec ALIAS ${TARGET} )
add_library( libavformat ALIAS ${TARGET} )
add_library( libavutil ALIAS ${TARGET} )

# Pull in standard variables
def_vars()

message( STATUS "========== Configuring ${name} ==========" )

# Let the Audacity target know we're using ffmpeg
set( USE_${symbol} ON CACHE INTERNAL USE_${symbol} )

# Add the system/local option
option( use_system_${name} "Use ${name} system library if available" ${prefer_system_libs} )

# Look up the system packages if the user wants them
if( use_system_${name} )
   # Provide an option that determines if the libraries are loaded
   # dynamically at run time or statically linked at build time
   option( disable_dynamic_${name} "Disable dynamic loading of ${name} libraries" NO)

   # Look them up
   pkg_check_modules( ${TARGET} ${packages} )
else()
   # Make sure to reset in case user reconfigures between local/system
   set( disable_dynamic_${name} NO CACHE INTERNAL "" )
endif()

# If the system packages were found
if( ${TARGET}_FOUND )
   message( STATUS "Using '${name}' system library" )

   # Pull in the package settings
   get_package_interface( ${TARGET} )
else()
   message( STATUS "Using '${name}' local library" )

   # Otherwise define the local settings
   list( APPEND INCLUDES
      INTERFACE
         ${TARGET_ROOT}
   )
endif()

# And add the settings to the target
target_include_directories( ${TARGET} INTERFACE ${INCLUDES} )
target_compile_options( ${TARGET} INTERFACE ${COPTS} )
target_link_directories( ${TARGET} INTERFACE ${LINKDIRS} )
target_link_options( ${TARGET} INTERFACE ${LOPTS} )
target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )

