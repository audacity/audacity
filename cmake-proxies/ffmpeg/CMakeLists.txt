
# Add our target and all of it's aliases
add_library( ${TARGET} INTERFACE )
add_library( ${symbol} ALIAS ${TARGET} )
add_library( libavcodec ALIAS ${TARGET} )
add_library( libavformat ALIAS ${TARGET} )
add_library( libavutil ALIAS ${TARGET} )

# Pull in standard variables
def_vars()

message( STATUS "========== Configuring ${name} ==========" )

# We can only (possibly) link to it if we have pkg-config
if( PkgConfig_FOUND )
   set( opt "linked" )
   set( desc "linked, " )
endif()

# FFmpeg is either dynamically loaded, linked to, or off
cmd_option( ${_OPT}use_ffmpeg
            "Use ffmpeg library [loaded, ${desc}off]"
            "loaded"
            STRINGS "loaded" ${opt} "off"
)

# Determine if it will be turned off, linked to, or loaded
if( ${_OPT}use_ffmpeg STREQUAL "off" )
   message( STATUS "Disabling '${name}' library" )
else()
   # Let the Audacity target know that this library will be used.
   set( USE_${symbol} ON CACHE INTERNAL "" )

   # Check for system package
   pkg_check_modules( ${TARGET} ${packages} )

   # Default to dynamically loaded
   set( isdyn YES )

   # Does the user want to link to it instead?
   if( ${_OPT}use_ffmpeg STREQUAL "linked" )
      # Set up for link if it was found
      if( ${TARGET}_FOUND )
         message( STATUS "Linking '${name}' library during build" )

         # Pull in the package settings
         get_package_interface( ${TARGET} )

         # Not dynamic
         set( isdyn NO )
      endif()
   endif()

   # Pull in the local includes if we're dynamically loading
   if( isdyn )
      message( STATUS "Will dynamically load '${name}' library at runtime" )

      # Use the system includes if they are available
      if( ${TARGET}_FOUND )
         message( STATUS "Using '${name}' system includes" )

         # Only pull in the includes, don't need the libraries
         list( APPEND INCLUDES
            INTERFACE
               ${${TARGET}_INCLUDE_DIRS}
         )
      else()
         message( STATUS "Using '${name}' local includes" )

         # Use the local ffmpeg includes
         list( APPEND INCLUDES
            INTERFACE
               ${TARGET_ROOT}
         )
      endif()
   endif()
endif()

# And add the settings to the target
target_include_directories( ${TARGET} INTERFACE ${INCLUDES} )
target_compile_options( ${TARGET} INTERFACE ${COPTS} )
target_link_directories( ${TARGET} INTERFACE ${LINKDIRS} )
target_link_options( ${TARGET} INTERFACE ${LOPTS} )
target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )

