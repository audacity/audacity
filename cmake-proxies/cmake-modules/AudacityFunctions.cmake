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

function( audacity_library_name var value )
   file( RELATIVE_PATH temp "${CMAKE_SOURCE_DIR}/src" "${value}" )
   string( REGEX REPLACE "[/\\]" "_" temp "${temp}" )
   set( ${var} "${temp}" PARENT_SCOPE )
endfunction()

function( audacity_library_names var )
   foreach( library ${ARGN} )
      if( EXISTS "${CMAKE_SOURCE_DIR}/${library}" )
         audacity_library_name( library "${CMAKE_SOURCE_DIR}/${library}" )
      endif()
      list( APPEND all_libraries "${library}" )
   endforeach()
   set( ${var} ${all_libraries} PARENT_SCOPE )
endfunction()

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
         ${EXPERIMENTAL_OPTIONS_LIST}
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
         "${CMAKE_SOURCE_DIR}"
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
endfunction()

# Macro makes a library target (computing its name from the current source path)
# unless given no sources; then visits further subdirectories, and passes
# collected names of targets to the calling scope.
#
# Nesting of subdirectories does not yet have any influence on their target
# properties, but it is intended that transitive dependencies between two
# subdirectories where neither contains the other should be non-cyclic, and the
# script that generates a dependency graph will assign similar colors to sister
# subdirectories and put them in a cluster.
#
# The dependencies among subdirectories defined by the union of INCLUDE_PRIVATE
# and INCLUDE_PUBLIC relations may have cycles. Those defined by PRIVATE and
# PUBLIC must not (configuration will fail when not all the subdirectories in a
# cycle have TYPE STATIC).  It is intended that the uses of INCLUDE_PRIVATE and
# INCLUDE_PUBLIC be phased out as subdirectories are reorganized to eliminate
# dependency cycles.
#
# TYPE chooses type of library in add_library, defaults to OBJECT; if SOURCES
#    and OBJCPP_SOURCES are empty, it must be undefined.
#    (Note that with STATIC archives, objects will link into the build only as
#    needed to satisfy references; therefore some other library type is needed
#    if the code relies on registrations at file-scope static initialization
#    time that avoid static linkage depdendencies from without.)
# INCLUDE_PRIVATE names other subdirectories relative to project root; the
#    header files in them, and in the subdirectories they name in
#    INCLUDE_PUBLIC, are available to the compiler
# INCLUDE_PUBLIC affects this subdirectory's compilation as does INCLUDE_PRIVATE
#    and also adds them, with the subdirectory itself, to the interface include
#    directories property; use it when headers in this subdirectory must include
#    those from elsewhere, such as to inherit a base class, so that dependents'
#    INCLUDE_PRIVATE lists may be shortened
# PRIVATE names other subdirectories relative to project root or third-party
#    libraries.  All of their interface properties affect compilation --
#    includes and command-line preprocessor definitions, also the interface
#    properties of other subdirectories transitively reachable via
#    PUBLIC
# PUBLIC affects compilation as does PRIVATE, and also populates the interface
#    properties of this library.  All interface properties of other
#    subdirectories in PUBLIC are included in the interface too, and so on
#    transitively, and also the INCLUDE_PUBLIC of all of those, but then not
#    transitively
# PRECOMPILE are names of accessible header files to include in this directory's
#    precompiled header; unquoted file names from the project's own tree, or
#    system header names in <...>
# OBJCPP_SOURCES are relative to the current directory; these will compile on
#    Mac as Objective-C++, and will be added only to the Mac build, unless also
#    listed in SOURCES
# SOURCES are source file paths relative to the current directory
# SUBDIRECTORIES relative to current may also define libraries, whose names are
#    collected, and with this directory's library name (if any), are passed to
#    the calling scope in COLLECTED_OBJECTS (for object libraries),
#    COLLECTED_MODULES (for module libraries),
#    and COLLECTED_LIBRARIES (for other libraries, including third-party), so
#    that the top level can build and link them correctly
#
macro( do_directory )
   message( STATUS "========== Configuring ${CMAKE_CURRENT_SOURCE_DIR} ==========" )

   # parse arguments
   cmake_parse_arguments( DO_DIRECTORY
      ""
      "TYPE"
      "INCLUDE_PRIVATE;INCLUDE_PUBLIC;PRIVATE;PUBLIC;PRECOMPILE;OBJCPP_SOURCES;SOURCES;SUBDIRECTORIES"
      ${ARGN}
   )

   if( NOT DO_DIRECTORY_SOURCES AND NOT DO_DIRECTORY_OBJCPP_SOURCES )
      if( DO_DIRECTORY_TYPE )
         message( FATAL_ERROR "TYPE not allowed without sources" )
      endif()
      unset( DO_DIRECTORY_NAME )
   else()
      # determine target name and library type
      audacity_library_name( DO_DIRECTORY_NAME "${CMAKE_CURRENT_SOURCE_DIR}" )
      if( NOT DO_DIRECTORY_TYPE )
         set( DO_DIRECTORY_TYPE OBJECT )
      endif()

      # add the library
      add_library( "${DO_DIRECTORY_NAME}" "${DO_DIRECTORY_TYPE}" )

      # allow library's own headers to be found in compilation, and also usable
      # by dependents
      target_include_directories(
         "${DO_DIRECTORY_NAME}" PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}" )

      # populate the INTERFACE includes -- not as PUBLIC, see below instead
      # for population of PRIVATE include directories.
      set( DO_DIRECTORY_INCLUDES ${DO_DIRECTORY_INCLUDE_PUBLIC} )
      list( TRANSFORM DO_DIRECTORY_INCLUDES PREPEND "${CMAKE_SOURCE_DIR}/" )
      target_include_directories(
         ${DO_DIRECTORY_NAME} INTERFACE ${DO_DIRECTORY_INCLUDES} )

      # convert supplied lists to target names
      foreach( DO_DIRECTORY_LIST INCLUDE_PRIVATE INCLUDE_PUBLIC PRIVATE PUBLIC )
         audacity_library_names(
            DO_DIRECTORY_${DO_DIRECTORY_LIST}
            ${DO_DIRECTORY_${DO_DIRECTORY_LIST}} )
      endforeach()

      # populate our own private include directories with others' interfaces
      foreach( DO_DIRECTORY_LIBRARY
         ${DO_DIRECTORY_INCLUDE_PRIVATE}
         ${DO_DIRECTORY_INCLUDE_PUBLIC}
      )
         # use a generator expression to get whatever the above lines put into
         # interfaces.  (We can't use the generator in the above lines because
         # we are allowing cycles and that might cause nonterminating generator
         # expression expansion.  So we get only one level of recursive access
         # to public includes of dependencies, not fully transitive.)
         target_include_directories(
            "${DO_DIRECTORY_NAME}" PRIVATE
            "$<TARGET_PROPERTY:${DO_DIRECTORY_LIBRARY},INTERFACE_INCLUDE_DIRECTORIES>" )
      endforeach()

      # set up other compile and link options, and transitive availability
      # of public library dependencies.  We don't use any interface-only (as
      # distinct from private) link libraries.
      audacity_target_options(
         "${DO_DIRECTORY_NAME}"
         "${DO_DIRECTORY_PRIVATE}"
         "${DO_DIRECTORY_PUBLIC}"
      )

      # Set special options for any objective-c++ files on Mac
      if( DO_DIRECTORY_OBJCPP_SOURCES AND CMAKE_SYSTEM_NAME MATCHES "Darwin" )
         list( APPEND DO_DIRECTORY_SOURCES ${DO_DIRECTORY_OBJCPP_SOURCES} )
         set_source_files_properties( ${DO_DIRECTORY_OBJCPP_SOURCES}
            PROPERTIES COMPILE_FLAGS "-x objective-c++"
            SKIP_PRECOMPILE_HEADERS YES
         )
      endif()

      # Make the sources part of the build
      # Eliminate duplicates first, in case some file needed mention also
      # in OBJCPP_SOURCES
      list( SORT DO_DIRECTORY_SOURCES )
      list( REMOVE_DUPLICATES DO_DIRECTORY_SOURCES )
      target_sources( "${DO_DIRECTORY_NAME}" PRIVATE ${DO_DIRECTORY_SOURCES} )

      if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" AND NOT CCACHE_PROGRAM AND ${_OPT}use_pch )
         # Generate the precompiled header for this directory
         set( DO_DIRECTORY_PCH "${CMAKE_CURRENT_BINARY_DIR}/private/AudacityHeaders.h" )
         file( MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/private" )
         list( TRANSFORM DO_DIRECTORY_PRECOMPILE REPLACE "^([^<].*)" "\"\\1\"" )
         list( TRANSFORM DO_DIRECTORY_PRECOMPILE PREPEND "#include " )
         string( JOIN "\n" DO_DIRECTORY_PRECOMPILES ${DO_DIRECTORY_PRECOMPILE} )
         configure_file(
            "${CMAKE_SOURCE_DIR}/src/AudacityHeaders.h.in"
            "${DO_DIRECTORY_PCH}"
         )
         target_precompile_headers(
            "${DO_DIRECTORY_NAME}" PRIVATE "${DO_DIRECTORY_PCH}" )
      endif()
   endif()

   # visit further sub-directories
   foreach( DO_DIRECTORY_SUBDIRECTORY ${DO_DIRECTORY_SUBDIRECTORIES} )
      add_subdirectory( "${DO_DIRECTORY_SUBDIRECTORY}" )
   endforeach()

   # collect library and object names
   if( DO_DIRECTORY_NAME )
      if( DO_DIRECTORY_TYPE STREQUAL "OBJECT" )
         # All constituent objects of the archive will be linked directly into
         # the program
         list( APPEND COLLECTED_OBJECTS "$<TARGET_OBJECTS:${DO_DIRECTORY_NAME}>" )
         # libraries used by an OBJECT library are not linked when the library
         # builds, so they must be collected and linked into the program
         list( APPEND COLLECTED_LIBRARIES ${DO_DIRECTORY_PRIVATE} ${DO_DIRECTORY_PUBLIC} )
      elseif( DO_DIRECTORY_TYPE STREQUAL "MODULE" )
         # modules are not linked into the main program, but should be counted as
         # dependencies
         list( APPEND COLLECTED_MODULES "${DO_DIRECTORY_NAME}" )
      else()
         # STATIC and SHARED libraries will be linked into the program
         list( APPEND COLLECTED_LIBRARIES "${DO_DIRECTORY_NAME}" )
      endif()
   endif()

   # These lines require a macro, not function scope to propagate values up
   # to the CMakeLists.txt invoking the one containing the macro call
   foreach( DO_DIRECTORY_VAR OBJECTS MODULES LIBRARIES )
      set( COLLECTED_${DO_DIRECTORY_VAR} ${COLLECTED_${DO_DIRECTORY_VAR}} PARENT_SCOPE)
   endforeach()
endmacro()

