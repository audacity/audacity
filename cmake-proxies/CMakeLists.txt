
#These are done in their actual directories, no need for a proxy.

#Same idea, but not yet done/needed
#add_subdirectory( "mod-null" )
#add_subdirectory( "mod-nyq-bench" )
#add_subdirectory( "mod-track-panel" )

#These are all headers, nothing to build.
#add_subdirectory( "ffmpeg" )

function( addlib dir name symbol required version )
   message( STATUS "========== Configuring ${name} ==========" )

   set( TARGET ${dir} )

   set( enable enable_${name} ) 
   if( NOT ${required} )
      option( ${enable} "Enable ${name} library" ON )
   else()
      set( ${enable} ON )
   endif()

   set( use_system use_system_${name} ) 
   if( PkgConfig_FOUND AND version )
      option( ${use_system} "Prefer ${name} system library if available" OFF )
   else()
      set( ${use_system} OFF )
   endif()

   if( NOT ${${enable}} )
      return()
   endif()

   set( USE_${symbol} ON CACHE INTERNAL USE_${symbol} )

   if( ${${use_system}} )
      pkg_check_modules( ${name} ${version} )
      if( ${${name}_FOUND} )
         message( STATUS "Using SYSTEM '${name}' package" )

         add_library( ${TARGET} INTERFACE IMPORTED GLOBAL )

         target_compile_options( ${TARGET} INTERFACE ${${name}_CFLAGS_OTHER} )
         target_include_directories( ${TARGET} INTERFACE ${${name}_INCLUDE_DIRS} )
         target_link_libraries( ${TARGET} INTERFACE ${${name}_LIBRARIES} )

         return()
      endif()
   endif()

   message( STATUS "Using LOCAL '${name}' package" )
   set( TARGET_ROOT ${libsrc}/${dir} )
   add_subdirectory( ${dir} EXCLUDE_FROM_ALL )

   get_property( targets DIRECTORY "${dir}" PROPERTY BUILDSYSTEM_TARGETS )
   foreach( target ${targets} )
      get_target_property( type "${target}" TYPE )
      if( NOT "${type}" STREQUAL "INTERFACE_LIBRARY" )
         # Add "global" defines
         set( DEFINES
            NDEBUG
         )
         target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )

         set_target_properties( ${target} PROPERTIES FOLDER "lib-src" )
      endif()
   endforeach()
endfunction()

# Required libraries
#
#       directory          option      symbol      req   version
addlib( wxwidgets          wxWidgets   WXWIDGETS   YES   "" )
addlib( FileDialog         FileDialog  FILEDIALOG  YES   "" )
addlib( expat              expat       EXPAT       YES   "" )
addlib( lame               lame        LAME        YES   "lame >= 3.100" )
addlib( lib-widget-extra   libextra    WIDGET      YES   "" )
addlib( libsndfile         sndfile     SNDFILE     YES   "sndfile >= 1.0.24" )
addlib( libsoxr            soxr        SOXR        YES   "soxr >= 0.1.1" )
addlib( portaudio-v19      portaudio   PORTAUDIO   YES   "" )

# Optional libraries
#
#       directory          option      symbol      req   version
addlib( lv2                lv2         LV2         NO    "lilv-0 >= 0.24.6 lv2 >= 1.16.0 serd-0 >= 0.30.2 sord-0 >= 0.16.4 sratom-0 >= 0.6.4" )
addlib( libid3tag          id3tag      LIBID3TAG   NO    "id3tag >= 0.15.1b" )
addlib( libmad             mad         LIBMAD      NO    "mad >= 2.3" )
addlib( libnyquist         nyquist     NYQUIST     NO    "" )
addlib( libvamp            vamp        VAMP        NO    "vamp >= 2.5" )
addlib( libogg             ogg         LIBOGG      NO    "ogg >= 1.3.1" )
addlib( libvorbis          vorbis      LIBVORBIS   NO    "vorbis >= 1.3.3" )
addlib( libflac            flac        LIBFLAC     NO    "flac >= 1.3.1" )
addlib( portmidi           midi        PORTMIDI    NO    "portmidi >= 0.1" )
addlib( portmixer          portmixer   PORTMIXER   NO    "" )
addlib( portsmf            portsmf     PORTSMF     NO    "portsmf >= 0.1" )
addlib( sbsms              sbsms       SBSMS       NO    "sbsms >= 2.0.2" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    "soundtouch >= 1.7.1" )
addlib( twolame            twolame     LIBTWOLAME  NO    "twolame >= 0.3.13" )

