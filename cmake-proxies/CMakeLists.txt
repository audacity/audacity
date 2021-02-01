# Allow user to globally set the library preference
cmd_option( ${_OPT}lib_preference
            "Library preference [system (if available), local]"
            "system"
            STRINGS "system" "local"
)

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
#     required Determines if the library is required or not.  If it is,
#              the user is not given the option of enabling/disabling it.
#
#     check    Determines if local/system checks should be performed here
#              or in the subdirectory config.
#
#     packages A list of packages required for this target in pkg-config
#              format.
function( addlib dir name symbol required check )
   # Extract the list of packages from the function args
   list( SUBLIST ARGV 5 -1 packages )

   # Define target's name and it's source directory
   set( TARGET ${dir} )
   set( TARGET_ROOT ${libsrc}/${dir} )

   # Define the option name
   set( use ${_OPT}use_${name} ) 

   # If we're not checking for system or local here, then let the
   # target config handle the rest.
   if( NOT check )
      add_subdirectory( ${dir} EXCLUDE_FROM_ALL )
      return()
   endif()

   # If the target isn't required, allow the user to select which one
   # to use or disable it entirely
   set( desc "local" )
   if( packages )
      set( sysopt "system" )
      string( PREPEND desc "system (if available), " )
      set( default "${${_OPT}lib_preference}" )
   else()
      set( default "local" )
   endif()

   if( NOT required )
      set( reqopt "off" )
      string( APPEND desc ", off" )
   endif()

   cmd_option( ${use}
               "Use ${name} library [${desc}]"
               "${default}"
               STRINGS ${sysopt} "local" ${reqopt}
   )

   # Bail if the target will not be used
   if( ${use} STREQUAL "off" )
      message( STATUS "========== ${name} disabled ==========" )

      set( USE_${symbol} OFF CACHE INTERNAL "" FORCE )

      return()
   endif()

   # Let the Audacity target know that this library will be used
   set( USE_${symbol} ON CACHE INTERNAL "" FORCE )

   message( STATUS "========== Configuring ${name} ==========" )

   # Check for the system package(s) if the user prefers it
   if( ${use} STREQUAL "system" )
      # Look them up
      pkg_check_modules( PKG_${TARGET} ${packages} )
      if( PKG_${TARGET}_FOUND )
         message( STATUS "Using '${name}' system library" )

         # Create the target interface library
         add_library( ${TARGET} INTERFACE IMPORTED GLOBAL )

         # Retrieve the package information
         get_package_interface( PKG_${TARGET} )

         # And add it to our target
         target_include_directories( ${TARGET} INTERFACE ${INCLUDES} )
         target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )
      else()
         set( ${use} "local" )
         set_property( CACHE ${use} PROPERTY VALUE "local" )
      endif()
   endif()

   # User wants the local package or the system one wasn't found
   if( ${use} STREQUAL "local" )
      message( STATUS "Using '${name}' local library" )

      # Pull in the target config
      add_subdirectory( ${dir} EXCLUDE_FROM_ALL )

      # Get the list of targets defined by that config
      get_property( targets DIRECTORY "${dir}" PROPERTY BUILDSYSTEM_TARGETS )

      # Set the folder (for the IDEs) for each one
      foreach( target ${targets} )
         # Skip interface libraries since they don't have any source to
         # present in the IDEs   
         get_target_property( type "${target}" TYPE )
         if( NOT "${type}" STREQUAL "INTERFACE_LIBRARY" )
            set_target_properties( ${target} PROPERTIES FOLDER "lib-src" )
         endif()
      endforeach()
   endif()
endfunction()

# Required libraries
#
#       directory          option      symbol      req   chk   version
addlib( wxWidgets          wxwidgets   WX          YES   NO    "" ) # must be first
addlib( expat              expat       EXPAT       YES   YES   "expat >= 2.1.0" )
addlib( lame               lame        LAME        YES   YES   "lame >= 3.100" )
addlib( libsndfile         sndfile     SNDFILE     YES   YES   "sndfile >= 1.0.28" )
addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )
addlib( portaudio-v19      portaudio   PORTAUDIO   YES   YES   "" )
addlib( sqlite             sqlite      SQLITE      YES   YES   "sqlite3 >= 3.32.0" )

# Optional libraries
#
#       directory          option      symbol      req   chk   version
addlib( ffmpeg             ffmpeg      FFMPEG      NO    NO    "libavcodec >= 51.53" "libavformat >= 52.12" "libavutil >= 52.66" )
addlib( libid3tag          id3tag      LIBID3TAG   NO    YES   "id3tag >= 0.15.1b" )
addlib( libmad             mad         LIBMAD      NO    YES   "mad >= 0.15.1b" )
addlib( libnyquist         nyquist     NYQUIST     NO    YES   "" )
addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )
addlib( libogg             ogg         LIBOGG      NO    YES   "ogg >= 1.3.1" )
addlib( libvorbis          vorbis      LIBVORBIS   NO    YES   "vorbis >= 1.3.3" "vorbisenc >= 1.3.3" "vorbisfile >= 1.3.3" )
addlib( libflac            flac        LIBFLAC     NO    YES   "flac >= 1.3.1" "flac++ >= 1.3.1" )
addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" "suil-0 >= 0.10.6" )
addlib( portmidi           midi        MIDI        NO    YES   "portmidi >= 0.1" )
addlib( portmixer          portmixer   PORTMIXER   NO    YES   "" )
addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
addlib( libsbsms           sbsms       SBSMS       NO    YES   "sbsms >= 2.2.0" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    YES   "soundtouch >= 1.7.1" )
addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )

