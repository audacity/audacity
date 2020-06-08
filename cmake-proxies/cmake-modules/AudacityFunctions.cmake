#
# A collection of functions and macros
#

# Defines several useful directory paths for the active context.
macro( def_vars )
   set( _SRCDIR "${CMAKE_CURRENT_SOURCE_DIR}" )
   set( _INTDIR "${CMAKE_CURRENT_BINARY_DIR}" )
   set( _PRVDIR "${CMAKE_CURRENT_BINARY_DIR}/private" )
   set( _PUBDIR "${CMAKE_CURRENT_BINARY_DIR}/public" )
endmacro()

# Helper to organize sources into folders for the IDEs
macro( organize_source root prefix sources )
   set( cleaned )
   foreach( source ${sources} )
      # Remove generator expressions
      string( REGEX REPLACE ".*>:(.*)>*" "\\1" source "${source}" )
      string( REPLACE ">" "" source "${source}" )

      # Remove keywords
      string( REGEX REPLACE "^[A-Z]+$" "" source "${source}" )

      # Add to cleaned
      list( APPEND cleaned "${source}" )
   endforeach()

   # Define the source groups
   if( "${prefix}" STREQUAL "" )
      source_group( TREE "${root}" FILES ${cleaned} )
   else()
      source_group( TREE "${root}" PREFIX ${prefix} FILES ${cleaned} )
   endif()
endmacro()

# Given a directory, recurse to all defined subdirectories and assign
# the given folder name to all of the targets found.
function( set_dir_folder dir folder)
   get_property( subdirs DIRECTORY "${dir}" PROPERTY SUBDIRECTORIES )
   foreach( sub ${subdirs} )
      set_dir_folder( "${sub}" "${folder}" )
   endforeach()

   get_property( targets DIRECTORY "${dir}" PROPERTY BUILDSYSTEM_TARGETS )
   foreach( target ${targets} )
      get_target_property( type "${target}" TYPE )
      if( NOT "${type}" STREQUAL "INTERFACE_LIBRARY" )
         set_target_properties( ${target} PROPERTIES FOLDER ${folder} )
      endif()
   endforeach()
endfunction()

# Helper to retrieve the settings returned from pkg_check_modules()
macro( get_package_interface package )
   set( INCLUDES
      ${${package}_INCLUDE_DIRS}
   )

   set( LINKDIRS
      ${${package}_LIBDIR}
   )

   # We resolve the full path of each library to ensure the
   # correct one is referenced while linking
   foreach( lib ${${package}_LIBRARIES} )
      find_library( LIB_${lib} ${lib} HINTS ${LINKDIRS} )
      list( APPEND LIBRARIES ${LIB_${lib}} )
   endforeach()
endmacro()

# Set the cache and context value
macro( set_cache_value var value )
   set( ${var} "${value}" )
   set_property( CACHE ${var} PROPERTY VALUE "${value}" )
endmacro()

# Set the given property and its config specific brethren to the same value
function( set_target_property_all target property value )
   set_target_properties( "${target}" PROPERTIES "${property}" "${value}" )
   foreach( type ${CMAKE_CONFIGURATION_TYPES} )
      string( TOUPPER "${property}_${type}" prop )
      set_target_properties( "${target}" PROPERTIES "${prop}" "${value}" )
   endforeach()
endfunction()

# Taken from wxWidgets and modified for Audacity
#
# cmd_option(<name> <desc> [default] [STRINGS strings])
# The default is ON if third parameter isn't specified
function( cmd_option name desc )
   cmake_parse_arguments( OPTION "" "" "STRINGS" ${ARGN} )

   if( ARGC EQUAL 2 )
      if( OPTION_STRINGS )
         list( GET OPTION_STRINGS 1 default )
      else()
         set( default ON )
      endif()
   else()
      set( default ${OPTION_UNPARSED_ARGUMENTS} )
   endif()

   if( OPTION_STRINGS )
      set( cache_type STRING )
   else()
      set( cache_type BOOL )
   endif()

   set( ${name} "${default}" CACHE ${cache_type} "${desc}" )
   if( OPTION_STRINGS )
      set_property( CACHE ${name} PROPERTY STRINGS ${OPTION_STRINGS} )

      # Check valid value
      set( value_is_valid FALSE )
      set( avail_values )
      foreach( opt ${OPTION_STRINGS} )
         if( ${name} STREQUAL opt )
            set( value_is_valid TRUE )
            break()
         endif()
         string( APPEND avail_values " ${opt}" )
      endforeach()
      if( NOT value_is_valid )
         message( FATAL_ERROR "Invalid value \"${${name}}\" for option ${name}. Valid values are: ${avail_values}" )
      endif()
   endif()

   set( ${name} "${${name}}" PARENT_SCOPE )
endfunction()

# Downloads NuGet packages
#
# Why this is needed...
#
# To get NuGet to work, you have to add the VS_PACKAGE_REFERENCES
# property to a target. This target must NOT be a UTILITY target,
# which is what we use to compile the message catalogs and assemble
# the manual. We could add that property to the Audacity target and
# CMake would add the required nodes to the VS project. And when the
# Audacity target is built, the NuGet packages would get automatically
# downloaded. This also means that the locale and manual targets
# must be dependent on the Audacity target so the packages would get
# downloaded before they execute. This would be handled by the CMake
# provided ALL_BUILD target which is, by default, set as the startup
# project in Visual Studio. Sweet right? Well, not quite...
#
# We want the Audacity target to be the startup project to provide
# eaiser debugging. But, if we do that, the ALL_BUILD target is no
# longer "in control" and any dependents of the Audacity target would
# not get built. So, targets like "nyquist" and "plug-ins" would have
# to be manually built. This is not what we want since Nyquist would
# not be available during Audacity debugging because the Nyquist runtime
# would not be copied into the destination folder alonside the Audacity
# executable.
#
# To remedy this conundrum, we simply download the NuGet packages
# ourselves and make the Audacity target dependent on the targets
# mentioned above. This ensures that the dest folder is populated
# and laid out like Audacity expects.
#
function( nuget_package dir name version )
   # Generate the full package directory name
   set( pkgdir "${CMAKE_BINARY_DIR}/packages/${name}/${version}" )

   # Don't download it again if the package directory already exists
   if( NOT EXISTS "${pkgdir}" )
      set( pkgurl "https://www.nuget.org/api/v2/package/${name}/${version}" )

      # Create the package directory
      file( MAKE_DIRECTORY "${pkgdir}" )

      # And download the package into the package directory
      file( DOWNLOAD "${pkgurl}" "${pkgdir}/package.zip" )

      # Extract the contents of the package into the package directory
      execute_process(
         COMMAND
            ${CMAKE_COMMAND} -E tar x "${pkgdir}/package.zip"
         WORKING_DIRECTORY
            ${pkgdir}
      )
   endif()

   # Return the package directory name to the caller
   set( ${dir} "${pkgdir}" PARENT_SCOPE )
endfunction()

# Determines if the linker supports the "-platform_version" argument
# on macOS.
macro( check_for_platform_version )
   if( NOT DEFINED LINKER_SUPPORTS_PLATFORM_VERSION )
      execute_process(
         COMMAND
            ld -platform_version macos 1.1 1.1
         ERROR_VARIABLE
            error
      )

      if( error MATCHES ".*unknown option.*" )
         set( PLATFORM_VERSION_SUPPORTED no CACHE INTERNAL "" )
      else()
         set( PLATFORM_VERSION_SUPPORTED yes CACHE INTERNAL "" )
      endif()
   endif()
endmacro()

# Set compilation options common to library targets that make up the
# executable.
function( audacity_target_options target private_libraries public_libraries )
   #
   #
   #
   set( DEFINES
      PRIVATE
         BUILDING_AUDACITY
         WXINTL_NO_GETTEXT_MACRO
         WXUSINGDLL
         CMAKE
         $<$<BOOL:${HAVE_LRINT}>:
            HAVE_LRINT
         >
         $<$<BOOL:${HAVE_LRINTF}>:
            HAVE_LRINTF
         >
         $<$<BOOL:${HAVE_MLOCK}>:
            HAVE_MLOCK
         >
         $<$<PLATFORM_ID:Windows>:
            _CRT_SECURE_NO_WARNINGS
            __STDC_CONSTANT_MACROS
            STRICT
         >
   )
   target_compile_definitions( ${target} PRIVATE ${DEFINES} )

   set( OPTIONS
      PRIVATE
         $<$<CXX_COMPILER_ID:MSVC>:/permissive->
         $<$<CXX_COMPILER_ID:AppleClang,Clang>:-Wno-underaligned-exception-object>
#      $<$<CXX_COMPILER_ID:GNU>:-Wl,-rpath -Wl,${_RPATH}>
   )
   target_compile_options( ${target} PRIVATE ${OPTIONS} )

   #
   #
   #
   # some always included directories
   set( INCLUDES
      PUBLIC
         # for the generated config header:
         "${CMAKE_BINARY_DIR}/src/private"
         # for other generated headers:
         "${CMAKE_CURRENT_BINARY_DIR}/private"

         "${CMAKE_SOURCE_DIR}/include"
         "${CMAKE_SOURCE_DIR}/src"
   )
   target_include_directories( ${target} PRIVATE ${INCLUDES} )

   set( LDFLAGS
      PRIVATE
         $<$<CXX_COMPILER_ID:MSVC>:/MANIFEST:NO>
   )
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      # Bug 2400 workaround
      #
      # Replaces the SDK version in the built executable with 10.13 to
      # prevent high CPU usage and slow drawing on Mojave or newer
      check_for_platform_version()
      if( PLATFORM_VERSION_SUPPORTED )
         list( APPEND LDFLAGS
            PRIVATE
               -Wl,-platform_version,macos,10.7,10.13
         )
      else()
         list( APPEND LDFLAGS
            PRIVATE
               -Wl,-sdk_version,10.13
         )
      endif()
   endif()
   target_link_options( ${target} PRIVATE ${LDFLAGS} )

   # so-called "link" libraries affect compilation too (include paths)
   list( APPEND private_libraries
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GLIB>
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:z>
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:pthread>
   )
   target_link_libraries( ${target} PRIVATE ${private_libraries} )
   if( public_libraries )
      target_link_libraries( ${target} PUBLIC ${public_libraries} )
   endif()

   if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" AND NOT CCACHE_PROGRAM AND ${_OPT}use_pch )
      target_precompile_headers( ${target} PRIVATE "${CMAKE_SOURCE_DIR}/src/AudacityHeaders.h" )
   endif()
endfunction()
