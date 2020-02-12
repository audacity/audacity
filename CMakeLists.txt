# If you want built-in precompiled header support
# then make sure you have cmake 3.16 or higher.
cmake_minimum_required( VERSION 3.14 )

# Don't allow in-source builds...no real reason, just
# keeping those source trees nice and tidy.  :-)
# (This can be removed if it becomes an issue.)
if( EXISTS "lib-src" )
   message( FATAL_ERROR
      "In-source builds not allowed.\n"
      "Create a new directory and run cmake from there, i.e.:\n"
      "  mkdir build\n"
      "  cd build\n"
      "  cmake ..\n"
      "You will need to delete CMakeCache.txt and CMakeFiles from this directory to clean up."
   )
endif()

# Ignore COMPILE_DEFINITIONS_<Config> properties
cmake_policy( SET CMP0043 NEW )

# ``INTERPROCEDURAL_OPTIMIZATION`` is enforced when enabled.
cmake_policy( SET CMP0069 NEW )

# ``FindOpenGL`` prefers GLVND by default when available.
cmake_policy( SET CMP0072 NEW )

# Include file check macros honor ``CMAKE_REQUIRED_LIBRARIES``.
cmake_policy( SET CMP0075 NEW )

if( WIN32 )
   # The NuGet packages that the Windows build requires
   # (Only here for visibility)
   set( GETTEXT_NAME "Gettext.Tools" )
   set( GETTEXT_VERSION "0.20.1.1" )
   set( PYTHON_NAME "python2" )
   set( PYTHON_VERSION "2.7.17" )
elseif( APPLE )
   # Define the OSX compatibility parameters
   set( CMAKE_OSX_ARCHITECTURES x86_64 CACHE INTERNAL "" )
   set( CMAKE_OSX_DEPLOYMENT_TARGET 10.7 CACHE INTERNAL "" )
   set( CMAKE_OSX_SYSROOT macosx CACHE INTERNAL "" )

   # Shouldn't cmake do this???
   string( APPEND CMAKE_CXX_FLAGS " -stdlib=libc++" )
endif()

# Define option() prefix
set( _OPT "audacity_" )

# Add our module path
set( CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake-proxies/cmake-modules)

# This "is a good thing" but greatly increases link time on Linux
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION ON )
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON )
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION_DEBUG OFF )

# Set the required C++ standard
set( CMAKE_CXX_STANDARD 14 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )

# Use ccache if available
find_program( CCACHE_PROGRAM ccache )
mark_as_advanced( FORCE CCACHE_PROGRAM )

if( CCACHE_PROGRAM )
   set_property( GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}" )
endif()

# Our very own project
project( Audacity )

# Pull all the modules we'll need
include( CheckCXXCompilerFlag )
include( CheckIncludeFile )
include( CheckIncludeFiles )
include( CheckLibraryExists )
include( CheckSymbolExists )
include( CheckTypeSize )
include( CMakeDependentOption )
include( CMakeDetermineASM_NASMCompiler )
include( CMakePushCheckState )
include( GNUInstallDirs )
include( TestBigEndian )

# Organize subdirectories/targets into folders for the IDEs
set_property( GLOBAL PROPERTY USE_FOLDERS ON )

if( CMAKE_GENERATOR MATCHES "Visual Studio" )
   # Make sure Audacity is the startup project
   set_directory_properties(
      PROPERTIES
         VS_STARTUP_PROJECT "${CMAKE_PROJECT_NAME}"
   )

   # Build using multiple processors
   foreach( config ${CMAKE_CONFIGURATION_TYPES} )
      string( TOUPPER "${config}" config )
      string( APPEND CMAKE_C_FLAGS_${config} " /MP" )
      string( APPEND CMAKE_CXX_FLAGS_${config} " /MP" )
   endforeach()

   # Define system library information, but we'll do the install
   set( CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP YES )
   set( CMAKE_INSTALL_UCRT_LIBRARIES NO )
   set( CMAKE_INSTALL_MFC_LIBRARIES NO )
   set( CMAKE_INSTALL_OPENMP_LIBRARIES NO )
   include( InstallRequiredSystemLibraries )
endif()

# Where the final product is stored
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/audacity )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/audacity )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )

# Set up RPATH handling
set( CMAKE_SKIP_BUILD_RPATH FALSE )
set( CMAKE_BUILD_WITH_INSTALL_RPATH FALSE )
set( CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}/audacity" )
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )
set( CMAKE_MACOSX_RPATH FALSE )

# the RPATH to be used when installing, but only if it's not a system directory
#list( FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_FULL_LIBDIR}" isSystemDir)
#IF("${isSystemDir}" STREQUAL "-1")
#   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
#ENDIF("${isSystemDir}" STREQUAL "-1")

# Just a couple of convenience variables
set( topdir "${CMAKE_SOURCE_DIR}" )
set( libsrc "${topdir}/lib-src" )

# Add the math library (if found) to the list of required libraries
check_library_exists( m pow "" HAVE_LIBM )
if( HAVE_LIBM )
   list( APPEND CMAKE_REQUIRED_LIBRARIES -lm )
endif()

# Add the dynamic linker library (if needed) to the list of required libraries
list( APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_DL_LIBS} )

# Make sure they're used during the link steps
set( CMAKE_LINK_INTERFACE_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} )

# May be a misconfiguration in my system, but CMake doesn't want to look
# in /usr/local/lib by default...so force the issue
if( CMAKE_SYSTEM_NAME MATCHES "FreeBSD" )
   list( APPEND CMAKE_EXE_LINKER_FLAGS -L/usr/local/lib )
   list( APPEND CMAKE_MODULE_LINKER_FLAGS -L/usr/local/lib )
   list( APPEND CMAKE_SHARED_LINKER_FLAGS -L/usr/local/lib )
endif()

# Various common checks whose results are used by the different targets
test_big_endian( WORDS_BIGENDIAN )

# Check for compiler flags
if( CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang|GNU" )
   check_cxx_compiler_flag( "-mmmx" HAVE_MMX )
   if( HAVE_MMX )
      set( MMX_FLAG "-mmmx" CACHE INTERNAL "" )
   endif()

   check_cxx_compiler_flag( "-msse" HAVE_SSE )
   if( HAVE_SSE )
      set( SSE_FLAG "-msse" CACHE INTERNAL "" )
   endif()

   check_cxx_compiler_flag( "-msse2" HAVE_SSE2 )
   if( HAVE_SSE2 )
      set( SSE_FLAG "-msse2" CACHE INTERNAL "" )
   endif()
elseif( CMAKE_CXX_COMPILER_ID MATCHES "MSVC" )
   set( HAVE_MMX ON )
   set( MMX_FLAG "" )
   set( HAVE_SSE ON )
   set( SSE_FLAG "/arch:SSE" )
   set( HAVE_SSE2 ON )
   set( SSE2_FLAG "/arch:SSE2" )
endif()

check_include_files( "float.h;stdarg.h;stdlib.h;string.h" STDC_HEADERS )

check_include_file( "assert.h" HAVE_ASSERT_H )
check_include_file( "byteswap.h" HAVE_BYTESWAP_H )
check_include_file( "errno.h" HAVE_ERRNO_H )
check_include_file( "fcntl.h" HAVE_FCNTL_H )
check_include_file( "fenv.h" HAVE_FENV_H )
check_include_file( "inttypes.h" HAVE_INTTYPES_H )
check_include_file( "limits.h" HAVE_LIMITS_H )
check_include_file( "malloc.h" HAVE_MALLOC_H )
check_include_file( "memory.h" HAVE_MEMORY_H )
check_include_file( "stdbool.h" HAVE_STDBOOL_H )
check_include_file( "stdint.h" HAVE_STDINT_H )
check_include_file( "stdlib.h" HAVE_STDLIB_H )
check_include_file( "string.h" HAVE_STRING_H )
check_include_file( "strings.h" HAVE_STRINGS_H )
check_include_file( "unistd.h" HAVE_UNISTD_H )
check_include_file( "xmmintrin.h" HAVE_XMMINTRIN_H )
check_include_file( "sys/param.h" HAVE_SYS_PARAM_H )
check_include_file( "sys/stat.h" HAVE_SYS_STAT_H )
check_include_file( "sys/types.h" HAVE_SYS_TYPES_H )
check_include_file( "sys/wait.h" HAVE_SYS_WAIT_H )

check_symbol_exists( bcopy "strings.h" HAVE_BCOPY )
check_symbol_exists( fileno "stdio.h" HAVE_FILENO )
check_symbol_exists( flock "sys/file.h" HAVE_FLOCK )
check_symbol_exists( fork "unistd.h" HAVE_FORK )
check_symbol_exists( fsync "unistd.h" HAVE_FSYNC )
check_symbol_exists( ftruncate "unistd.h" HAVE_FTRUNCATE )
check_symbol_exists( getpagesize "unistd.h" HAVE_GETPAGESIZE )
check_symbol_exists( gettimeofday "sys/time.h" HAVE_GETTIMEOFDAY )
check_symbol_exists( gmtime "time.h" HAVE_GMTIME )
check_symbol_exists( gmtime_r "time.h" HAVE_GMTIME_R )
check_symbol_exists( lrint "math.h" HAVE_LRINT )
check_symbol_exists( lrintf "math.h" HAVE_LRINTF )
check_symbol_exists( lround "math.h" HAVE_LROUND )
check_symbol_exists( lstat "sys/stat.h" HAVE_LSTAT )
check_symbol_exists( memcpy "string.h" HAVE_MEMCPY )
check_symbol_exists( memmove "string.h" HAVE_MEMMOVE )
check_symbol_exists( mlock "sys/mman.h" HAVE_MLOCK )
check_symbol_exists( pipe "unistd.h" HAVE_PIPE )
check_symbol_exists( posix_fadvise "fcntl.h" HAVE_POSIX_FADVISE )
check_symbol_exists( posix_memalign "stdlib.h" HAVE_POSIX_MEMALIGN )
check_symbol_exists( strchr "string.h" HAVE_STRCHR )
check_symbol_exists( waitpid "sys/wait.h" HAVE_WAITPID )

check_type_size( "int8_t" SIZEOF_INT8 LANGUAGE C )
check_type_size( "int16_t" SIZEOF_INT16 LANGUAGE C )
check_type_size( "uint16_t" SIZEOF_UINT16 LANGUAGE C )
check_type_size( "u_int16_t" SIZEOF_U_INT16 LANGUAGE C )
check_type_size( "int32_t" SIZEOF_INT32 LANGUAGE C )
check_type_size( "uint32_t" SIZEOF_UINT32 LANGUAGE C )
check_type_size( "u_int32_t" SIZEOF_U_INT32 LANGUAGE C )
check_type_size( "int64_t" SIZEOF_INT64 LANGUAGE C )
check_type_size( "short" SIZEOF_SHORT LANGUAGE C )
check_type_size( "unsigned short" SIZEOF_UNSIGNED_SHORT LANGUAGE C )
check_type_size( "int" SIZEOF_INT LANGUAGE C )
check_type_size( "unsigned int" SIZEOF_UNSIGNED_INT LANGUAGE C )
check_type_size( "long" SIZEOF_LONG LANGUAGE C )
check_type_size( "unsigned long" SIZEOF_UNSIGNED_LONG LANGUAGE C )
check_type_size( "long long" SIZEOF_LONG_LONG LANGUAGE C )
check_type_size( "unsigned long long" SIZEOF_UNSIGNED_LONG_LONG LANGUAGE C )
check_type_size( "float" SIZEOF_FLOAT LANGUAGE C )
check_type_size( "double" SIZEOF_DOUBLE LANGUAGE C )
check_type_size( "long double" SIZEOF_LONG_DOUBLE LANGUAGE C )
check_type_size( "loff_t" SIZEOF_LOFF LANGUAGE C )
check_type_size( "off_t" SIZEOF_OFF LANGUAGE C )
check_type_size( "off64_t" SIZEOF_OFF64 LANGUAGE C )
check_type_size( "size_t" SIZEOF_SIZE LANGUAGE C )
check_type_size( "wchar_t" SIZEOF_WCHAR LANGUAGE C )
check_type_size( "void*" SIZEOF_POINTER LANGUAGE C )

# Determine 32-bit or 64-bit target
if( CMAKE_C_COMPILER_ID MATCHES "MSVC" AND CMAKE_VS_PLATFORM_NAME MATCHES "Win64|x64" )
   set( IS_64BIT ON )
elseif( NOT CMAKE_SIZEOF_VOID_P STREQUAL "4" )
   set( IS_64BIT ON )
endif()

if( IS_64BIT )
   message( STATUS "Building for 64-bit target" )
else()
   message( STATUS "Building for 32-bit target" )
endif()

# We'll be using it if it's available
find_package( PkgConfig QUIET )

# Mostly just to make the CMP0072 policy happy
find_package( OpenGL QUIET )

# When called will define several useful directory paths for the 
# current context.
macro( def_vars )
   set( _SRCDIR "${CMAKE_CURRENT_SOURCE_DIR}" )
   set( _INTDIR "${CMAKE_CURRENT_BINARY_DIR}" )
   set( _PRVDIR "${CMAKE_CURRENT_BINARY_DIR}/private" )
   set( _PUBDIR "${CMAKE_CURRENT_BINARY_DIR}/public" )
endmacro()

# Define the non-install and executable destinations
#
# If this is a multi-config build system (VS, Xcode), CMAKE_CFG_INTDIR
# will (eventually) resolve to the build type, i.e., Debug, Release, etc.
# and CMAKE_BUILD_TYPE will be empty.
#
# For single-config build systems, CMAKE_CFG_INTDIR will be "." and
# CMAKE_BUILD_TYPE will be something like Debug.
#
# So, in either case we end up with what we want:
#     .../bin/Debug//
# or:
#     .../bin//Debug
set( _DEST "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CMAKE_CFG_INTDIR}/${CMAKE_BUILD_TYPE}" )
set( _EXEDIR "${_DEST}" )
string( REGEX REPLACE "/+$" "" _EXEDIR "${_EXEDIR}" )

# Adjust them for the Mac
if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( _DEST "${_DEST}/Audacity.app/Contents" )
   set( _EXEDIR "${_DEST}/Macos" )
endif()

set( _PREFIX "${CMAKE_INSTALL_PREFIX}" )
set( _LIBDIR "${CMAKE_INSTALL_LIBDIR}/audacity" )
set( _RPATH "\$ORIGIN/../${_LIBDIR}" )
set( _DATADIR "${CMAKE_INSTALL_DATADIR}" )
set( _PKGDATA "${_DATADIR}/audacity/" )

# Precreate the lib and lib64 directories so we can make then the same
if( NOT EXISTS "${CMAKE_BINARY_DIR}/lib" )
   file( MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/lib" )
endif()

# Only create on systems that need it, effectively excluding Windows where links
# may not work due to insufficient privileges
if( NOT CMAKE_INSTALL_LIBDIR STREQUAL "lib" AND NOT EXISTS "${CMAKE_BINARY_DIR}/lib64" )
   file( CREATE_LINK "${CMAKE_BINARY_DIR}/lib" "${CMAKE_BINARY_DIR}/lib64" SYMBOLIC )
endif()

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

   set( COPTS
      ${${package}_CFLAGS}
      ${${package}_CFLAGS_OTHER}
   )

   set( LOPTS
      ${${package}_LDFLAGS}
      ${${package}_LDFLAGS_OTHER}
   )

   set( LINKDIRS
      ${${package}_LIBRARY_DIRS}
   )

   set( LIBRARIES
      ${${package}_LIBRARIES}
   )
endmacro()

# Set the given property and its config specific brethren to the same value
function( set_target_property_all target property value )
   set_target_properties( "${target}" PROPERTIES "${property}" "${value}" )
   foreach( type ${CMAKE_CONFIGURATION_TYPES} )
      string( TOUPPER "${property}_${type}" prop )
      set_target_properties( "${target}" PROPERTIES "${prop}" "${value}" )
   endforeach()
endfunction()

# Add our children
add_subdirectory( "cmake-proxies" )
add_subdirectory( "help" )
add_subdirectory( "images" )
add_subdirectory( "locale" )
add_subdirectory( "nyquist" )
add_subdirectory( "plug-ins" )
add_subdirectory( "src" )
add_subdirectory( "cmake-proxies/mod-nyq-bench" )
add_subdirectory( "cmake-proxies/mod-script-pipe" )

# Uncomment what follows for symbol values.
#[[
   get_cmake_property(_variableNames VARIABLES)
   foreach (_variableName ${_variableNames})
      message(STATUS "${_variableName}=${${_variableName}}")
   endforeach()
#]]
#[[
   include(PrintProperties)
   print_properties(TARGET "wxWidgets")
#]]

