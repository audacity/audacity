# If you want built-in precompiled header support
# then make sure you have cmake 3.16 or higher.
#
# Minimum required is 3.15 due to use of multiple values in
# generator expressions.
cmake_minimum_required( VERSION 3.15 )

# If building with GNU compiler, then must be 4.9 or later.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.9")
   message(FATAL_ERROR "Audacity requires at least GCC 4.9")
endif ()

if( EXISTS "${CMAKE_SOURCE_DIR}/cmake-proxies/cmake-modules/Variables.cmake" )
   include( "${CMAKE_SOURCE_DIR}/cmake-proxies/cmake-modules/Variables.cmake" )
endif()

# We only do alpha builds, beta builds, and release versions.
# Most of the time we're in development, so AUDACITY_BUILD_LEVEL should be
# defined to 0.
# Its value may be more than 0 for pre-release "Beta" builds that differ only
# in the welcome screen, and hiding of some development menu commands, but
# still link to the alpha manual online.

# Set this value to 0 for alpha, 1 for beta, 2 for release builds
if( NOT DEFINED AUDACITY_BUILD_LEVEL )
   set( AUDACITY_BUILD_LEVEL 0 CACHE STRING "0 for alpha, 1 for beta, 2 for release builds" )
endif()

# The Audacity version
# Increment as appropriate after release of a new version, and set back
# AUDACITY_BUILD_LEVEL to 0
set( AUDACITY_VERSION 3 )
set( AUDACITY_RELEASE 1 )
set( AUDACITY_REVISION 0 )
set( AUDACITY_MODLEVEL 0 )

string( TIMESTAMP __TDATE__ "%Y%m%d" )
if( AUDACITY_BUILD_LEVEL EQUAL 0 )
   set( AUDACITY_SUFFIX "-alpha-${__TDATE__}" )
elseif(  AUDACITY_BUILD_LEVEL EQUAL 1 )
   set( AUDACITY_SUFFIX "-beta-${__TDATE__}" )
else()
   set( AUDACITY_SUFFIX "" )
endif()

# Don't allow in-source builds...no real reason, just
# keeping those source trees nice and tidy.  :-)
# (This can be removed if it becomes an issue.)
if( "${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}" )
   message( FATAL_ERROR
      "In-source builds not allowed.\n"
      "Create a new directory and run cmake from there, i.e.:\n"
      "  mkdir build\n"
      "  cd build\n"
      "  cmake ..\n"
      "You will need to delete CMakeCache.txt and CMakeFiles from this directory to clean up."
   )
endif()

# Just a couple of convenience variables
set( topdir "${CMAKE_SOURCE_DIR}" )
set( libsrc "${topdir}/lib-src" )

# Default build type is Debug
if( NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES )
   set( CMAKE_BUILD_TYPE "Debug" )
endif()

# Ignore COMPILE_DEFINITIONS_<Config> properties
cmake_policy( SET CMP0043 NEW )

# Link libraries by full path even in implicit directories.
cmake_policy( SET CMP0060 NEW )

# ``INTERPROCEDURAL_OPTIMIZATION`` is enforced when enabled.
cmake_policy( SET CMP0069 NEW )

# ``FindOpenGL`` prefers GLVND by default when available.
cmake_policy( SET CMP0072 NEW )

# Include file check macros honor ``CMAKE_REQUIRED_LIBRARIES``.
cmake_policy( SET CMP0075 NEW )

# Definitions that must happen before the project() command
if( APPLE )
   set( MIN_MACOS_VERSION 10.9 )
   set( TARGET_MACOS_VERSION 10.13 )
   # Generate schema files
   set( CMAKE_XCODE_GENERATE_SCHEME ON )

   # Define the OSX compatibility parameters
   set( CMAKE_OSX_ARCHITECTURES x86_64 CACHE INTERNAL "" )
   set( CMAKE_OSX_DEPLOYMENT_TARGET ${MIN_MACOS_VERSION} CACHE INTERNAL "" )
   set( CMAKE_OSX_SYSROOT macosx CACHE INTERNAL "" )
   set( CMAKE_XCODE_ATTRIBUTE_CODE_SIGN_IDENTITY "" CACHE INTERNAL "" )

   # This prevents a link error when building with the 10.9 or older SDKs
   set( CMAKE_XCODE_ATTRIBUTE_CLANG_LINK_OBJC_RUNTIME OFF )

   # Shouldn't cmake do this???
   string( APPEND CMAKE_CXX_FLAGS " -stdlib=libc++" )
endif()

# Add our module path
# CMAKE_BINARY_DIR is required for Conan to work
set( AUDACITY_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake-proxies/cmake-modules")
set( CMAKE_MODULE_PATH
   ${AUDACITY_MODULE_PATH}
   ${CMAKE_BINARY_DIR}
   ${CMAKE_MODULE_PATH}
)

set( CMAKE_PREFIX_PATH
   ${CMAKE_BINARY_DIR}
   ${CMAKE_PREFIX_PATH}
)

# This "is a good thing" but greatly increases link time on Linux
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION ON )
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON )
#set( CMAKE_INTERPROCEDURAL_OPTIMIZATION_DEBUG OFF )

# Set the required C++ standard
set( CMAKE_CXX_STANDARD 17 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )

# Use ccache if available
find_program( CCACHE_PROGRAM ccache )
mark_as_advanced( FORCE CCACHE_PROGRAM )

if( CCACHE_PROGRAM )
   set_property( GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}" )
endif()

# Define option() prefix
set( _OPT "audacity_" )

# Our very own project
project( Audacity )

# Load our functions/macros
include( AudacityFunctions )

set_from_env(AUDACITY_ARCH_LABEL) # e.g. x86_64

# Allow user to globally set the library preference
cmd_option( ${_OPT}conan_enabled
            "Use Conan package manager for 3d party dependencies"
            On
)

cmd_option( ${_OPT}conan_allow_prebuilt_binaries
            "Allow using the compiled dependencies from the cache or remotes"
            On
)

# Allow user to globally set the library preference
cmd_option( ${_OPT}lib_preference
            "Library preference [system (if available), local]"
            "local"
            STRINGS "system" "local"
)

# Special mode, that will force dependencies to the packages provided by system unless they were set to local explicitly.
cmd_option( ${_OPT}obey_system_dependencies
            "Use system packages to satisfy dependencies"
            Off
)

cmd_option( ${_OPT}has_networking
   "Build networking features into Audacity"
   Off)

include( AudacityDependencies )

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

set_from_env(SENTRY_DSN_KEY)
set_from_env(SENTRY_HOST)
set_from_env(SENTRY_PROJECT)
set_from_env(CRASH_REPORT_URL)

cmake_dependent_option(
   ${_OPT}has_sentry_reporting
   "Build support for sending errors to Sentry"
   On
   "${_OPT}has_networking;DEFINED SENTRY_DSN_KEY;DEFINED SENTRY_HOST;DEFINED SENTRY_PROJECT"
   Off
)

cmake_dependent_option(
   ${_OPT}has_crashreports
   "Enables crash reporting for Audacity"
   On
   "${_OPT}has_networking;DEFINED CRASH_REPORT_URL"
   Off
)

cmake_dependent_option(
   ${_OPT}has_updates_check
   "Build updates checking features into Audacity"
   On
   "${_OPT}has_networking"
   Off
)

# Determine 32-bit or 64-bit target
if( CMAKE_C_COMPILER_ID MATCHES "MSVC" AND CMAKE_VS_PLATFORM_NAME MATCHES "Win64|x64" )
   set( IS_64BIT ON )
elseif( NOT CMAKE_SIZEOF_VOID_P STREQUAL "4" )
   set( IS_64BIT ON )
endif()

message( STATUS "Build Info:" )
message( STATUS "  Host System: ${CMAKE_HOST_SYSTEM}" )
message( STATUS "  Host System Name: ${CMAKE_HOST_SYSTEM_NAME}" )
message( STATUS "  Host System Processor: ${CMAKE_HOST_SYSTEM_PROCESSOR}" )
message( STATUS "  Host System Version: ${CMAKE_HOST_SYSTEM_VERSION}" )

if( IS_64BIT )
   message( STATUS "  Host System Architecture: 64-bit" )
else()
   message( STATUS "  Host System Architecture: 32-bit" )
endif()

message( STATUS )
message( STATUS "  Compiler: ${CMAKE_CXX_COMPILER}" )
message( STATUS "  Compiler Version: ${CMAKE_CXX_COMPILER_VERSION}" )
message( STATUS "  Compiler Standard: ${CMAKE_CXX_STANDARD}" )
message( STATUS "  Compiler Standard Required: ${CMAKE_CXX_STANDARD_REQUIRED}" )
message( STATUS "  Compiler Extensions: ${CMAKE_CXX_EXTENSIONS}" )
message( STATUS )

if( CMAKE_GENERATOR MATCHES "Visual Studio" )
   message( STATUS "  MSVC Version: ${MSVC_VERSION}" )
   message( STATUS "  MSVC Toolset: ${MSVC_TOOLSET_VERSION}" )
   message( STATUS )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( CMAKE_GENERATOR MATCHES "Xcode" )
      message( STATUS "  Xcode Version: ${XCODE_VERSION}" )
   endif()
   message( STATUS "  MacOS SDK: ${CMAKE_OSX_SYSROOT}" )
   message( STATUS )

   if(${_OPT}has_crashreports)
      set(CMAKE_XCODE_ATTRIBUTE_DEBUG_INFORMATION_FORMAT "dwarf-with-dsym")
   endif()

endif()

# Try to get the current commit information
set( GIT_COMMIT_SHORT "unknown" )
set( GIT_COMMIT_LONG "unknown" )
set( GIT_COMMIT_TIME "unknown" )
find_package( Git QUIET )
if( GIT_FOUND )
   execute_process(
      COMMAND
         ${GIT_EXECUTABLE} show -s "--format=%h;%H;%cd"
      WORKING_DIRECTORY
         ${topdir}
      OUTPUT_VARIABLE
         git_output
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
   )

   if( git_output )
      list( GET git_output 0 GIT_COMMIT_SHORT )
      list( GET git_output 1 GIT_COMMIT_LONG )
      list( GET git_output 2 GIT_COMMIT_TIME )

      file(WRITE
         "${CMAKE_BINARY_DIR}/Variables.cmake"
         "set( AUDACITY_BUILD_LEVEL ${AUDACITY_BUILD_LEVEL} )\nset( AUDACITY_REV_LONG \"${GIT_COMMIT_LONG}\" )\nset( AUDACITY_REV_TIME \"${GIT_COMMIT_TIME}\" )\n"
      )
   endif()
endif()
message( STATUS "  Current Commit: ${GIT_COMMIT_SHORT}" )
message( STATUS )

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
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )

set ( _SHARED_PROXY_BASE "shared" )
set ( _SHARED_PROXY_BASE_PATH "${CMAKE_BINARY_DIR}/${_SHARED_PROXY_BASE}")

# Define the non-install and executable paths
if( CMAKE_CONFIGURATION_TYPES )
   set( _DESTDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CMAKE_CFG_INTDIR}" )
   set( _SHARED_PROXY_PATH "${_SHARED_PROXY_BASE_PATH}/${CMAKE_CFG_INTDIR}")
else()
   set( _DESTDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CMAKE_BUILD_TYPE}" )
   set( _SHARED_PROXY_PATH "${_SHARED_PROXY_BASE_PATH}/${CMAKE_BUILD_TYPE}")
endif()

set( _DEST "${_DESTDIR}" )
set( INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}" )
set( _LIBDIR "${CMAKE_INSTALL_LIBDIR}" )
set( _DATADIR "${CMAKE_INSTALL_DATADIR}" )
set( _PKGLIB "${_LIBDIR}/audacity" )
set( _PKGDATA "${_DATADIR}/audacity/" )
set( _MANDIR "${CMAKE_INSTALL_MANDIR}" )
set( _MODDIR "${_DEST}/modules" )
set( _EXEDIR "${_DEST}" )

# Setup RPATH handling
set( CMAKE_BUILD_RPATH "${_DEST}/${_PKGLIB}" )
set( CMAKE_BUILD_WITH_INSTALL_RPATH FALSE )
set( CMAKE_INSTALL_RPATH "$ORIGIN/../${_PKGLIB}" )
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE )

# Adjust them for the Mac
if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( _APPDIR "Audacity.app/Contents" )
   set( _DEST "${_DESTDIR}/${_APPDIR}" )
   set( _EXEDIR "${_DEST}/MacOS" )
   set( _MODDIR "${_DEST}/modules" )
   set( _PKGLIB "${_DEST}/Frameworks" )

   set( CMAKE_MACOSX_RPATH OFF )
endif()

# Add the math library (if found) to the list of required libraries
check_library_exists( m pow "" HAVE_LIBM )
if( HAVE_LIBM )
   list( APPEND CMAKE_REQUIRED_LIBRARIES -lm )
endif()

check_library_exists( atomic __atomic_fetch_add_4 "" HAVE_LIBATOMIC )
if( HAVE_LIBATOMIC )
  list( APPEND CMAKE_REQUIRED_LIBRARIES -latomic )
endif()

# Add the dynamic linker library (if needed) to the list of required libraries
list( APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_DL_LIBS} )

# Make sure they're used during the link steps
set( CMAKE_LINK_INTERFACE_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} )

# Various common checks whose results are used by the different targets
test_big_endian( WORDS_BIGENDIAN )

# Check for compiler flags
set( MMX_FLAG "" CACHE INTERNAL "" )
set( SSE_FLAG "" CACHE INTERNAL "" )
if( CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang|GNU" )
   check_cxx_compiler_flag( "-mmmx" HAVE_MMX )
   if( HAVE_MMX AND NOT IS_64BIT )
      set( MMX_FLAG "-mmmx" CACHE INTERNAL "" )
   endif()

   check_cxx_compiler_flag( "-msse" HAVE_SSE )
   if( HAVE_SSE AND NOT IS_64BIT )
      set( SSE_FLAG "-msse" CACHE INTERNAL "" )
   endif()

   check_cxx_compiler_flag( "-msse2" HAVE_SSE2 )
   if( HAVE_SSE2 AND NOT IS_64BIT )
      set( SSE_FLAG "-msse2" CACHE INTERNAL "" )
   endif()
elseif( CMAKE_CXX_COMPILER_ID MATCHES "MSVC" )
   set( HAVE_MMX ON )
   set( HAVE_SSE ON )
   set( HAVE_SSE2 ON )
   if( NOT IS_64BIT )
      set( SSE_FLAG "/arch:SSE2" )
   endif()
endif()

check_include_files( "float.h;stdarg.h;stdlib.h;string.h" STDC_HEADERS )

check_include_file( "assert.h" HAVE_ASSERT_H )
check_include_file( "errno.h" HAVE_ERRNO_H )
check_include_file( "fcntl.h" HAVE_FCNTL_H )
check_include_file( "fenv.h" HAVE_FENV_H )
check_include_file( "inttypes.h" HAVE_INTTYPES_H )
check_include_file( "limits.h" HAVE_LIMITS_H )
if( CMAKE_SYSTEM_NAME MATCHES "FreeBSD" )
  check_include_file( "stdlib.h" HAVE_MALLOC_H )
  check_include_file( "sys/endian.h" HAVE_ENDIAN_H )
else()
  check_include_file( "malloc.h" HAVE_MALLOC_H )
  check_include_file( "byteswap.h" HAVE_BYTESWAP_H )
endif()
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

# We'll be using it if it's available
find_package( PkgConfig QUIET )

# Mostly just to make the CMP0072 policy happy
find_package( OpenGL QUIET )

# Precreate the lib and lib64 directories so we can make then the same
if( NOT EXISTS "${CMAKE_BINARY_DIR}/lib" )
   file( MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/lib" )
endif()

# Only create on systems that need it, effectively excluding Windows where links
# may not work due to insufficient privileges
if( NOT CMAKE_INSTALL_LIBDIR STREQUAL "lib" AND NOT EXISTS "${CMAKE_BINARY_DIR}/lib64" )
   file( CREATE_LINK "${CMAKE_BINARY_DIR}/lib" "${CMAKE_BINARY_DIR}/lib64" SYMBOLIC )
endif()

# Define Audacity's name
if( CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows" )
   set( AUDACITY_NAME "Audacity" )
else()
   set( AUDACITY_NAME "audacity" )
endif()

# Create short and full version strings
set( AUDACITY_DIST_VERSION ${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION} )
set( AUDACITY_INFO_VERSION ${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}.${AUDACITY_MODLEVEL} )

# Python is used for the manual and (possibly) message catalogs
find_package( Python3 )
if( Python3_FOUND )
   set( PYTHON "${Python3_EXECUTABLE}" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   # This is an odd case now, as Conan requires Python as well
   nuget_package( pkgdir "python3" "3.7.7" )
   file( TO_NATIVE_PATH "${pkgdir}/tools/python.exe" PYTHON )
endif()

# define EXPERIMENTAL flags
# Do this before consistency checks for added third-party libraries
include( "src/Experimental.cmake" )

cmd_option( ${_OPT}use_ffmpeg
   "Use ffmpeg library [loaded, off]"
   "loaded"
   STRINGS "loaded" "off"
)

if( ${_OPT}use_ffmpeg )
   set(USE_FFMPEG Yes)
endif()

# Add our children
add_subdirectory( "cmake-proxies" )

# Conan uses find_package and does not set GLOBAL flag
resolve_conan_dependencies()

# This libraries have 3d party dependencies.
# Moreover, portmixer is only available as a part of
# Audacity source tree now.
cmd_option( ${_OPT}use_portmixer "Build PortMixer support into Audacity" On)
if( ${_OPT}use_portmixer )
   set(USE_PORTMIXER Yes)
   add_subdirectory( "lib-src/portmixer" )
endif()

cmd_option( ${_OPT}use_nyquist "Build Nyquist support into Audacity" On)
if( ${_OPT}use_nyquist )
   set(USE_NYQUIST Yes)
   add_subdirectory( "lib-src/libnyquist" )
endif()

add_subdirectory( "help" )

if(${_OPT}has_crashreports)
   add_subdirectory( "crashreports" )
endif()

add_subdirectory( "images" )
add_subdirectory( "libraries" )
add_subdirectory( "locale" )
add_subdirectory( "src" )
add_subdirectory( "modules" )
add_subdirectory( "nyquist" )
add_subdirectory( "plug-ins" )
add_subdirectory( "scripts" )

# Generate config file
if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   configure_file( src/audacity_config.h.in src/private/configwin.h )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( HAVE_VISIBILITY 1 )
   configure_file( src/audacity_config.h.in src/private/configmac.h )
else()
   set( HAVE_VISIBILITY 1 )
   configure_file( src/audacity_config.h.in src/private/configunix.h )
endif()

# Generate a picture of module dependencies
string( JOIN "\n" GRAPH_EDGES ${GRAPH_EDGES} )
# Choose edge attributes making it easy to hover at either end of edge
# and see a tooltip describing the edge, in svg image
file( WRITE "${CMAKE_CURRENT_BINARY_DIR}/modules.dot" "digraph {
   graph [rankdir=LR] edge [dir=both,arrowtail=inv] \n"
   "${GRAPH_EDGES}"
   "\n}\n"
)
execute_process( COMMAND
   dot -O -Tsvg "${CMAKE_CURRENT_BINARY_DIR}/modules.dot" )

#
# Code signing
#

cmd_option( ${_OPT}perform_codesign
            "Perform code signing during the install step. This only works on Windows and macOS."
            Off
)

cmake_dependent_option(
   ${_OPT}perform_notarization
   "Perform notarization during the install step. This only works on macOS."
   Off
   "${_OPT}perform_codesign;APPLE"
   Off
)

if( ${_OPT}perform_codesign )
   include( AudacityCodeSigning )
endif()

#
# Packaging
#

cmd_option( ${_OPT}package_manual
            "Package the manual along with the DMG and InnoSetup targets"
            Off
)

# Variables, that are common for all package targets

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   include( AudacityInnoSetup )
endif()

# Uncomment what follows for symbol values.
#[[
   get_cmake_property( _variableNames VARIABLES )
   foreach( _variableName ${_variableNames} )
      message( STATUS "${_variableName}=${${_variableName}}" )
   endforeach()
#]]
#[[
   include( PrintProperties )
   print_properties( TARGET "wxWidgets" )
#]]

include( Package ) # do this last
