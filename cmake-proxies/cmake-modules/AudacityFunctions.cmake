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
function( get_package_interface package target )
   set( package_includes
      ${${package}_INCLUDE_DIRS}
   )

   set( package_linkdirs
      ${${package}_LIBDIR}
   )

   # We resolve the full path of each library to ensure the
   # correct one is referenced while linking
   set( package_libraries )
   foreach( lib ${${package}_LIBRARIES} )
      find_library( LIB_${lib} ${lib} HINTS ${package_linkdirs} )
      list( APPEND package_libraries ${LIB_${lib}} )
   endforeach()

   # And add it to our target
   target_include_directories( ${target} INTERFACE ${package_includes} )
   target_link_libraries( ${target} INTERFACE ${package_libraries} )

   message(STATUS "Interface ${target}:\n\tinclude: ${includes}\n\tLibraries: ${LIBRARIES}")
endfunction()

# Set the cache and context value
macro( set_cache_value var value )
   set( ${var} "${value}" )
   set_property( CACHE ${var} PROPERTY VALUE "${value}" )
endmacro()

# Set a CMake variable to the value of the corresponding environment variable
# if the CMake variable is not already defined. Any addition arguments after
# the variable name are passed through to set().
macro( set_from_env var )
   if( NOT DEFINED ${var} AND NOT "$ENV{${var}}" STREQUAL "" )
      set( ${var} "$ENV{${var}}" ${ARGN} ) # pass additional args (e.g. CACHE)
   endif()
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
# easier debugging. But, if we do that, the ALL_BUILD target is no
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

# To be used to compile all C++ in the application and modules
function( audacity_append_common_compiler_options var use_pch )
   if( NOT use_pch )
      list( APPEND ${var}
         PRIVATE
            # include the correct config file; give absolute path to it, so
            # that this works whether in src, modules, libraries
            $<$<PLATFORM_ID:Windows>:/FI${CMAKE_BINARY_DIR}/src/private/configwin.h>
            $<$<PLATFORM_ID:Darwin>:-include ${CMAKE_BINARY_DIR}/src/private/configmac.h>
            $<$<NOT:$<PLATFORM_ID:Windows,Darwin>>:-include ${CMAKE_BINARY_DIR}/src/private/configunix.h>
      )
   endif()
   list( APPEND ${var}
         -DAUDACITY_VERSION=${AUDACITY_VERSION}
         -DAUDACITY_RELEASE=${AUDACITY_RELEASE}
         -DAUDACITY_REVISION=${AUDACITY_REVISION}
         -DAUDACITY_MODLEVEL=${AUDACITY_MODLEVEL}

         # Version string for visual display
         -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

         # This value is used in the resource compiler for Windows
         -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"

         # This renames a good use of this C++ keyword that we don't need
	      # to review when hunting for leaks because of naked new and delete.
	 -DPROHIBITED==delete

         # Reviewed, certified, non-leaky uses of NEW that immediately entrust
	      # their results to RAII objects.
         # You may use it in NEW code when constructing a wxWindow subclass
	      # with non-NULL parent window.
         # You may use it in NEW code when the NEW expression is the
	      # constructor argument for a standard smart
         # pointer like std::unique_ptr or std::shared_ptr.
         -Dsafenew=new

         $<$<CXX_COMPILER_ID:MSVC>:/permissive->
         $<$<CXX_COMPILER_ID:AppleClang,Clang>:-Wno-underaligned-exception-object>
         $<$<CXX_COMPILER_ID:AppleClang,Clang>:-Werror=return-type>
         $<$<CXX_COMPILER_ID:AppleClang,Clang>:-Werror=dangling-else>
         $<$<CXX_COMPILER_ID:AppleClang,Clang>:-Werror=return-stack-address>
	      # Yes, CMake will change -D to /D as needed for Windows:
         -DWXINTL_NO_GETTEXT_MACRO
         $<$<CXX_COMPILER_ID:MSVC>:-D_USE_MATH_DEFINES>
         $<$<CXX_COMPILER_ID:MSVC>:-DNOMINMAX>

         # Define/undefine _DEBUG
	 # Yes, -U to /U too as needed for Windows:
	 $<IF:$<CONFIG:Debug>,-D_DEBUG=1,-U_DEBUG>
   )
   # Definitions controlled by the AUDACITY_BUILD_LEVEL switch
   if( AUDACITY_BUILD_LEVEL EQUAL 0 )
      list( APPEND ${var} -DIS_ALPHA -DUSE_ALPHA_MANUAL )
   elseif( AUDACITY_BUILD_LEVEL EQUAL 1 )
      list( APPEND ${var} -DIS_BETA -DUSE_ALPHA_MANUAL )
   else()
      list( APPEND ${var} -DIS_RELEASE )
   endif()

   set( ${var} "${${var}}" PARENT_SCOPE )
endfunction()

function( import_export_symbol var module_name )
   # compute, e.g. "TRACK_UI_API" from module name "mod-track-ui"
   string( REGEX REPLACE "^mod-" "" symbol "${module_name}" )
   string( REGEX REPLACE "^lib-" "" symbol "${symbol}" )
   string( TOUPPER "${symbol}" symbol )
   string( REPLACE "-" "_" symbol "${symbol}" )
   string( APPEND symbol "_API" )
   set( "${var}" "${symbol}" PARENT_SCOPE )
endfunction()

function( import_symbol_define var module_name )
   import_export_symbol( symbol "${module_name}" )
   if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      set( value "_declspec(dllimport)" )
   elseif( HAVE_VISIBILITY )
      set( value "__attribute__((visibility(\"default\")))" )
   else()
      set( value "" )
   endif()
   set( "${var}" "${symbol}=${value}" PARENT_SCOPE )
endfunction()

function( export_symbol_define var module_name )
   import_export_symbol( symbol "${module_name}" )
   if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      set( value "_declspec(dllexport)" )
   elseif( HAVE_VISIBILITY )
      set( value "__attribute__((visibility(\"default\")))" )
   else()
      set( value "" )
   endif()
   set( "${var}" "${symbol}=${value}" PARENT_SCOPE )
endfunction()

# shorten a target name for purposes of generating a dependency graph picture
function( canonicalize_node_name var node )
   # strip generator expressions
   string( REGEX REPLACE ".*>.*:(.*)>" "\\1" node "${node}" )
   # omit the "-interface" for alias targets to modules
   string( REGEX REPLACE "-interface\$" "" node "${node}"  )
   # shorten names of standard libraries or Apple frameworks
   string( REGEX REPLACE "^-(l|framework )" "" node "${node}" )
   # shorten paths
   get_filename_component( node "${node}" NAME_WE )
   set( "${var}" "${node}" PARENT_SCOPE )
endfunction()

function( audacity_module_fn NAME SOURCES IMPORT_TARGETS
   ADDITIONAL_DEFINES ADDITIONAL_LIBRARIES LIBTYPE )

   set( TARGET ${NAME} )
   set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

   message( STATUS "========== Configuring ${TARGET} ==========" )

   def_vars()

   if (LIBTYPE STREQUAL "MODULE" AND CMAKE_SYSTEM_NAME MATCHES "Windows")
      set( REAL_LIBTYPE SHARED )
   else()
      set( REAL_LIBTYPE "${LIBTYPE}" )
   endif()
   add_library( ${TARGET} ${REAL_LIBTYPE} )

   # Manual propagation seems to be necessary from
   # interface libraries -- just doing target_link_libraries naming them
   # doesn't work as promised

   # compute INCLUDES
   set( INCLUDES )
   list( APPEND INCLUDES PUBLIC ${TARGET_ROOT} )

   # compute DEFINES
   set( DEFINES )
   list( APPEND DEFINES ${ADDITIONAL_DEFINES} )

   # send the file to the proper place in the build tree, by setting the
   # appropriate property for the platform
   if (CMAKE_SYSTEM_NAME MATCHES "Windows")
      set( DIRECTORY_PROPERTY RUNTIME_OUTPUT_DIRECTORY )
   else ()
      set( DIRECTORY_PROPERTY LIBRARY_OUTPUT_DIRECTORY )
   endif ()

   if (LIBTYPE STREQUAL "MODULE")
      set( ATTRIBUTES "shape=box" )
      set_target_property_all( ${TARGET} ${DIRECTORY_PROPERTY} "${_MODDIR}" )
      set_target_properties( ${TARGET}
         PROPERTIES
            PREFIX ""
            FOLDER "modules" # for IDE organization
      )
      if( CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" )
         add_custom_command(
	    TARGET ${TARGET}
            COMMAND ${CMAKE_COMMAND}
	       -D SRC="${_MODDIR}/${TARGET}.so"
               -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>"
               -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
            POST_BUILD )
      endif()
   else()
      set( ATTRIBUTES "shape=octagon" )
      set_target_property_all( ${TARGET} ${DIRECTORY_PROPERTY} "${_SHARED_PROXY_PATH}" )
      set_target_properties( ${TARGET}
         PROPERTIES
            PREFIX ""
            FOLDER "libraries" # for IDE organization
            INSTALL_NAME_DIR ""
            BUILD_WITH_INSTALL_NAME_DIR YES
      )
   endif()

   if( "wxBase" IN_LIST IMPORT_TARGETS OR "wxwidgets::base" IN_LIST IMPORT_TARGETS )
      string( APPEND ATTRIBUTES " style=filled" )
   endif()

   export_symbol_define( export_symbol "${TARGET}" )
   import_symbol_define( import_symbol "${TARGET}" )

   list( APPEND DEFINES
      PRIVATE "${export_symbol}"
      INTERFACE "${import_symbol}"
   )

   set( LOPTS
      PRIVATE
         $<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>
   )

   # compute LIBRARIES
   set( LIBRARIES )

   foreach( IMPORT ${IMPORT_TARGETS} )
      list( APPEND LIBRARIES "${IMPORT}" )
   endforeach()

   list( APPEND LIBRARIES ${ADDITIONAL_LIBRARIES} )

#   list( TRANSFORM SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/" )

   # Compute compilation options.
   # Perhaps a another function argument in future to customize this too.
   set( OPTIONS )
   audacity_append_common_compiler_options( OPTIONS NO )

   organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
   target_sources( ${TARGET} PRIVATE ${SOURCES} )
   target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
   target_compile_options( ${TARGET} ${OPTIONS} )
   target_include_directories( ${TARGET} PUBLIC ${TARGET_ROOT} )

   target_link_options( ${TARGET} PRIVATE ${LOPTS} )
   target_link_libraries( ${TARGET} PUBLIC ${LIBRARIES} )

   if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows" )
      add_custom_command(
         TARGET "${TARGET}"
         POST_BUILD
         COMMAND $<IF:$<CONFIG:Debug>,echo,strip> -x $<TARGET_FILE:${TARGET}>
      )
   endif()

   if( NOT REAL_LIBTYPE STREQUAL "MODULE" )
      if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
         set( REQUIRED_LOCATION "${_EXEDIR}" )
      elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin")
         set( REQUIRED_LOCATION "${_PKGLIB}" )
      else()
         set( REQUIRED_LOCATION "${_DEST}/${_PKGLIB}" )
      endif()

      add_custom_command(TARGET ${TARGET} POST_BUILD
         COMMAND ${CMAKE_COMMAND} -E copy
            "$<TARGET_FILE:${TARGET}>"
            "${REQUIRED_LOCATION}/$<TARGET_FILE_NAME:${TARGET}>"
      )
   endif()

   # define an additional interface library target
   set(INTERFACE_TARGET "${TARGET}-interface")
   if (NOT REAL_LIBTYPE STREQUAL "MODULE")
      add_library("${INTERFACE_TARGET}" ALIAS "${TARGET}")
   else()
      add_library("${INTERFACE_TARGET}" INTERFACE)
      foreach(PROP
         INTERFACE_INCLUDE_DIRECTORIES
         INTERFACE_COMPILE_DEFINITIONS
         INTERFACE_LINK_LIBRARIES
      )
         get_target_property( PROPS "${TARGET}" "${PROP}" )
         if (PROPS)
            set_target_properties(
               "${INTERFACE_TARGET}"
               PROPERTIES "${PROP}" "${PROPS}" )
         endif()
      endforeach()
   endif()

   # collect dependency information
   list( APPEND GRAPH_EDGES "\"${TARGET}\" [${ATTRIBUTES}]" )
   if (NOT LIBTYPE STREQUAL "MODULE")
      list( APPEND GRAPH_EDGES "\"Audacity\" -> \"${TARGET}\"" )
   endif ()
   set(ACCESS PUBLIC PRIVATE INTERFACE)
   foreach( IMPORT ${IMPORT_TARGETS} )
      if(IMPORT IN_LIST ACCESS)
         continue()
      endif()
      canonicalize_node_name(IMPORT "${IMPORT}")
      list( APPEND GRAPH_EDGES "\"${TARGET}\" -> \"${IMPORT}\"" )
   endforeach()
   set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
endfunction()

# Set up for defining a module target.
# All modules depend on the application.
# Pass a name and sources, and a list of other targets.
# Use the interface compile definitions and include directories of the
# other targets, and link to them.
# More defines, and more target libraries (maybe generator expressions)
# may be given too.
macro( audacity_module NAME SOURCES IMPORT_TARGETS
   ADDITIONAL_DEFINES ADDITIONAL_LIBRARIES )
   # The extra indirection of a function call from this macro, and
   # re-assignment of GRAPH_EDGES, is so that a module definition may
   # call this macro, and it will (correctly) collect edges for the
   # CMakeLists.txt in the directory above it; but otherwise we take
   # advantage of function scoping of variables.
   audacity_module_fn(
      "${NAME}"
      "${SOURCES}"
      "${IMPORT_TARGETS}"
      "${ADDITIONAL_DEFINES}"
      "${ADDITIONAL_LIBRARIES}"
      "MODULE"
   )
   set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
endmacro()

# Set up for defining a library target.
# The application depends on all libraries.
# Pass a name and sources, and a list of other targets.
# Use the interface compile definitions and include directories of the
# other targets, and link to them.
# More defines, and more target libraries (maybe generator expressions)
# may be given too.
macro( audacity_library NAME SOURCES IMPORT_TARGETS
   ADDITIONAL_DEFINES ADDITIONAL_LIBRARIES )
   # ditto comment in the previous macro
   audacity_module_fn(
      "${NAME}"
      "${SOURCES}"
      "${IMPORT_TARGETS}"
      "${ADDITIONAL_DEFINES}"
      "${ADDITIONAL_LIBRARIES}"
      "SHARED"
   )
   set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
   # Collect list of libraries for the executable to declare dependency on
   list( APPEND AUDACITY_LIBRARIES "${NAME}" )
   set( AUDACITY_LIBRARIES "${AUDACITY_LIBRARIES}" PARENT_SCOPE )
endmacro()

# A special macro for header only libraries

macro( audacity_header_only_library NAME SOURCES IMPORT_TARGETS
   ADDITIONAL_DEFINES )
   # ditto comment in the previous macro
   add_library( ${NAME} INTERFACE )

   target_include_directories ( ${NAME} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR} )
   target_sources( ${NAME} INTERFACE ${SOURCES})
   target_link_libraries( ${NAME} INTERFACE ${IMPORT_TARGETS} )
   target_compile_definitions( ${NAME} INTERFACE ${ADDITIONAL_DEFINES} )

   list( APPEND AUDACITY_LIBRARIES "${NAME}" )
   set( AUDACITY_LIBRARIES "${AUDACITY_LIBRARIES}" PARENT_SCOPE )
endmacro()

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
   set( subdir "${CMAKE_SOURCE_DIR}/cmake-proxies/${dir}" )
   set( bindir "${CMAKE_BINARY_DIR}/cmake-proxies/${dir}" )

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
      add_subdirectory( ${subdir} ${bindir} EXCLUDE_FROM_ALL )
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

   if ( TARGET "${TARGET}" )
      return()
   endif()

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
         get_package_interface( PKG_${TARGET} ${TARGET} )
      else()
         find_package( ${packages} QUIET )

         if( TARGET ${TARGET} )
            set( PKG_${TARGET}_FOUND Yes )
         endif()
      endif()

      if( NOT PKG_${TARGET}_FOUND )
         if( ${_OPT}obey_system_dependencies )
            message( FATAL_ERROR "Failed to find the system package ${name}" )
         else()
            set( ${use} "local" )
            set_property( CACHE ${use} PROPERTY VALUE "local" )
         endif()
      endif()
   endif()

   # User wants the local package or the system one wasn't found
   if( ${use} STREQUAL "local" )
      message( STATUS "Using '${name}' local library" )

      # Pull in the target config
      add_subdirectory( ${subdir} ${bindir} EXCLUDE_FROM_ALL )

      # Get the list of targets defined by that config
      get_property( targets DIRECTORY "${subdir}" PROPERTY BUILDSYSTEM_TARGETS )

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

