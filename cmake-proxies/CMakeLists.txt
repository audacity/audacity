
# Allow user to globally set the system library preference
option( prefer_system_libs "Use system libraries if available" YES )

#
# Add individual library targets
#
# Parms:
#     dir      directory name within the cmake-proxies directory.
#              (Doesn't HAVE to match the same directory in lib-src,
#              but it usually does.)
#
#     name     suffix for the cmake user options
#
#     symbol   suffix for the "USE_<symbol>" variable that the Audacity
#              target uses to include/exclude functionality.
#
#     requried Determines if the library is required or not.  If it is,
#              the user is not given the option of enabling/disabling it.
#
#     check    Determines if local/system checks should be performed here
#              or in the subdirectory config.
#
#     packages A list of packages required for this target in pkg-config
#              format.
function( addlib dir name symbol required check ) #packages )
   # Extract the list of packages from the function args
   list( SUBLIST ARGV 5 -1 packages )

   # Define target's name and it's source directory
   set( TARGET ${dir} )
   set( TARGET_ROOT ${libsrc}/${dir} )

   # If the target is required, then it's always enabled.  Otherwise,
   # give the user the option to enable/disable it.
   set( enable enable_${name} ) 
   if( required )
      set( ${enable} YES )
   else()
      option( ${enable} "Enable ${name} library" ON )
   endif()

   # Bail if the target isn't enabled.
   if( NOT ${enable} )
      message( STATUS "========== ${name} disabled ==========" )
      return()
   endif()

   # If we're not checking for system or local here, then let the
   # target config handle the rest.
   if( NOT check )
      add_subdirectory( ${dir} EXCLUDE_FROM_ALL )
      return()
   endif()

   # Only present the system library option if pkg-config was found and
   # a package has been specified.  Otherwise, the local library will
   # be used unconditionally.
   set( system use_system_${name} ) 
   if( PkgConfig_FOUND AND packages )
      option( ${system} "Use ${name} system library if available" ${prefer_system_libs} )
   else()
      set( ${system} NO )
   endif()

   message( STATUS "========== Configuring ${name} ==========" )

   # Let the Audacity target know that this library will be used.
   set( USE_${symbol} ON CACHE INTERNAL USE_${symbol} )

   # Check for the system package if the user prefers it.
   if( ${system} )
      # Look them up
      pkg_check_modules( ${symbol} ${packages} )
      if( ${symbol}_FOUND )
         message( STATUS "Using '${name}' system library" )

         # Use the symbol name for the target to prevent a collision when
         # the alias libraries are defined for the listed packages.
         set( TARGET ${symbol} )

         # Create the target interface library
         add_library( ${TARGET} INTERFACE IMPORTED GLOBAL )

         # Retrieve the package information
         get_package_interface( ${symbol} )

         # And add it to our target
         target_include_directories( ${TARGET} INTERFACE ${INCLUDES} )
         target_compile_options( ${TARGET} INTERFACE ${CFLAGS} )
         target_link_directories( ${TARGET} INTERFACE ${LINKDIRS} )
         target_link_options( ${TARGET} INTERFACE ${LDFLAGS} )
         target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )

         # Define a library alias for each of the packages
         foreach( package ${packages} )
            # Convert the package spec to a list
            string( REPLACE " " ";" package "${package}" )

            # And extract just the package name
            list( GET package 0 package )

            # Create the alias
            add_library( "lib${package}" ALIAS ${TARGET} )
         endforeach()

         # We are done...
         return()
      endif()
   endif()

   message( STATUS "Using '${name}' local library" )

   # Pull in the target config
   add_subdirectory( ${dir} EXCLUDE_FROM_ALL )

   # Get the list of targets defined by that config
   get_property( targets DIRECTORY "${dir}" PROPERTY BUILDSYSTEM_TARGETS )

   # Set the folder (for the IDEs) for each one
   foreach( target ${targets} )
      # Add "global" defines
      set( DEFINES
         NDEBUG
      )
      target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )

      # Skip interface libraries since they don't have any source to
      # present in the IDEs   
      get_target_property( type "${target}" TYPE )
      if( NOT "${type}" STREQUAL "INTERFACE_LIBRARY" )
         set_target_properties( ${target} PROPERTIES FOLDER "lib-src" )
      endif()
   endforeach()
endfunction()

# Required libraries
#
#       directory          option      symbol      req   chk   version
addlib( wxwidgets          wxwidgets   WXWIDGETS   YES   NO    "" ) # must be first
addlib( FileDialog         FileDialog  FILEDIALOG  YES   YES   "" )
addlib( expat              expat       EXPAT       YES   YES   "" )
addlib( lame               lame        LAME        YES   YES   "lame >= 3.100" )
addlib( lib-widget-extra   libextra    WIDGET      YES   YES   "" )
addlib( libsndfile         sndfile     SNDFILE     YES   YES   "sndfile >= 1.0.24" )
addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )
addlib( portaudio-v19      portaudio   PORTAUDIO   YES   YES   "" )

# Optional libraries
#
#       directory          option      symbol      req   chk   version
addlib( ffmpeg             ffmpeg      FFMPEG      NO    NO    "libavcodec >= 51.53" "libavformat >= 52.12" "libavutil >= 52.66" )
addlib( libid3tag          id3tag      LIBID3TAG   NO    YES   "id3tag >= 0.15.1b" )
addlib( libmad             mad         LIBMAD      NO    YES   "mad >= 2.3" )
addlib( libnyquist         nyquist     NYQUIST     NO    YES   "" )
addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )
addlib( libogg             ogg         LIBOGG      NO    YES   "ogg >= 1.3.1" )
addlib( libvorbis          vorbis      LIBVORBIS   NO    YES   "vorbis >= 1.3.3" "vorbisenc >= 1.3.3" "vorbisfile >= 1.3.3" )
addlib( libflac            flac        LIBFLAC     NO    YES   "flac >= 1.3.1" "flac++ >= 1.3.1" )
addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" )
addlib( portmidi           midi        PORTMIDI    NO    YES   "portmidi >= 0.1" )
addlib( portmixer          portmixer   PORTMIXER   NO    YES   "" )
addlib( portsmf            portsmf     PORTSMF     NO    YES   "portsmf >= 0.1" )
addlib( sbsms              sbsms       SBSMS       NO    YES   "sbsms >= 2.0.2" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    YES   "soundtouch >= 1.7.1" )
addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )

