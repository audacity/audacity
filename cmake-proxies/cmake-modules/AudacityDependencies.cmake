set(find_package_file "${CMAKE_BINARY_DIR}/find_package_include.cmake")
file(WRITE ${find_package_file} "# Generated dependecies list\n")

#[[
   `audacity_find_package` emulates the behavior of `find_package` but
   add some Audaicity-specific logic to it.

   Directly supported find_package options:

   * REQUIRED
   * QUIET

   Other options can be passed to the `find_package` call by passing
   FIND_PACKAGE_OPTIONS before them.

   Additionally, the following options are supported:

   * CONAN_PACKAGE_NAME - name of the package in Conan. If not specified,
     the name of the package is assumed to be the lowercase of the CMake package name.
   * OPTION_NAME - name of the option to enable/disable the package. If not specified,
     the name of the option is assumed to be the lowercase of the CMake package name.

   Call to `audacity_find_package` will create the `option` with the name
   audacity_use_<OPTION_NAME>. If REQUIRED is not specified, the option will allow
   to disable the package. If Conan is enabled, the option will allow `local` state.
   In all the cases the option allows `system` state. By default, the option is set
   to audacity_lib_preference value.

   If the option is set to `local`, the package will be enabled when running Conan.

   If the option is not set to `off`, the package will be searched using `find_package`.

   Usage:

   audacity_find_package(
      <package_name>
      [REQUIRED]
      [QUIET]
      [FIND_PACKAGE_OPTIONS <find_package_options>]
      [CONAN_PACKAGE_NAME <conan_package_name>]
      [OPTION_NAME <option_name>]
   )
]]
function(audacity_find_package package_name)
   set(options REQUIRED QUIET)
   set(one_value_args VERSION CONAN_PACKAGE_NAME OPTION_NAME)
   set(multi_value_args FIND_PACKAGE_OPTIONS)
   cmake_parse_arguments(audacity_find_package "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

   #message(FATAL_ERROR "${package_name} R ${audicity_find_package_REQUIRED} Q ${audicity_find_package_QUIET} V ${audicity_find_package_VERSION} C ${audicity_find_package_CONAN_PACKAGE_NAME} O ${audicity_find_package_OPTION} F ${audicity_find_package_FIND_PACKAGE_OPTIONS}")

   if(NOT audacity_find_package_OPTION_NAME)
      string(TOLOWER "${package_name}" audacity_find_package_OPTION_NAME)
   endif()

   if(NOT audacity_find_package_CONAN_PACKAGE_NAME)
      string(TOLOWER "${package_name}" audacity_find_package_CONAN_PACKAGE_NAME)
   endif()

   set( option_name ${_OPT}use_${audacity_find_package_OPTION_NAME} )

   set( option_desc "local" )

   set( sysopt "system" )
   string( PREPEND option_desc "system (if available), " )

   if( ${_OPT}conan_enabled )
      set( default "${${_OPT}lib_preference}" )
   else()
      set( default "system" )
   endif()

    if( ${_OPT}conan_enabled )
        set( localopt "local" )
    endif()

    if( NOT audacity_find_package_REQUIRED )
        set( reqopt "off" )
        string( APPEND option_desc ", off" )
    endif()

   cmd_option( ${option_name}
      "Use ${option_name_base} library [${option_desc}]"
      "${default}"
      STRINGS ${sysopt} ${localopt} ${reqopt}
   )

   string( TOUPPER "${audacity_find_package_OPTION_NAME}" symbol)

   if( ${option_name} STREQUAL "off" )
      message( STATUS "========== ${package_name} is disabled ==========" )
      set( USE_${symbol} OFF CACHE INTERNAL "" FORCE )
      list(APPEND conan_package_options "-o" "use_${audacity_find_package_CONAN_PACKAGE_NAME}=False")
      return()
   endif()

   set( USE_${symbol} ON CACHE INTERNAL "" FORCE )

   if( audacity_find_package_REQUIRED )
      set(audacity_find_package_REQUIRED "REQUIRED")
   else()
      set(audacity_find_package_REQUIRED "")
   endif()

   if( audacity_find_package_QUIET )
      set(audacity_find_package_QUIET "QUIET")
   else()
      set(audacity_find_package_QUIET "")
   endif()

   string(
      JOIN " " find_package_string "find_package(" ${package_name}
         ${audacity_find_package_VERSION}
         ${audacity_find_package_REQUIRED}
         ${audacity_find_package_QUIET}
         ${audacity_find_package_FIND_PACKAGE_OPTIONS}
      ")\n"
   )

   file(APPEND ${find_package_file} "${find_package_string}")

   if( ${option_name} STREQUAL "local" )
      message( STATUS "========== Using Conan version of ${package_name} ==========" )
      list(APPEND conan_package_options "use_${audacity_find_package_CONAN_PACKAGE_NAME}=True")
   else()
      message( STATUS "========== Using system version of ${package_name} ==========" )
   endif()

   if(conan_package_options)
      set(conan_package_options ${conan_package_options} PARENT_SCOPE)
      mark_as_advanced(${package_name}_DIR)
      mark_as_advanced(${audacity_find_package_OPTION_NAME}_DIR)
      mark_as_advanced(${audacity_find_package_CONAN_PACKAGE_NAME}_DIR)
   endif()
endfunction()

# Process the list of the 3d party libraries
include (DependenciesList)

# If conan is enabled, run conan_runner.py
if( ${_OPT}conan_enabled )
   # Deduce the build type
   get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
   if( is_multi_config )
      set(_build_types ${CMAKE_CONFIGURATION_TYPES})
   else()
      set(_build_types ${CMAKE_BUILD_TYPE})
   endif()

   # Force the packages rebuld, if needed
   if( ${_OPT}conan_force_build_dependencies )
      set( _force_build "--force-build")
   endif()

   if( NOT ${_OPT}conan_allow_prebuilt_binaries )
      set( _disallow_prebuilt "--disallow-prebuilt")
   endif()

   # Deduce the target architecture
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      set( _target_arch ${MACOS_ARCHITECTURE} )
   elseif( MSVC )
      set( _target_arch ${CMAKE_CXX_COMPILER_ARCHITECTURE_ID} )
   else()
      set( _target_arch "${CMAKE_SYSTEM_PROCESSOR}" )
   endif()

   # Enable Conan download cache, if needed
   if( ${_OPT}conan_download_cache )
      set( _download_cache "--download-cache" ${${_OPT}conan_download_cache} )
   endif()

   # Set the libraries installation directory (Linux only, ignored on other platforms)
   if( _PKGLIB )
      set( _libdir "--lib-dir" ${_PKGLIB} )
   endif()

   if( MIN_MACOS_VERSION )
      set( _min_macos_version "--min-os-version" ${MIN_MACOS_VERSION} )
   endif()

   execute_process(
      COMMAND ${PYTHON} "${CMAKE_SOURCE_DIR}/conan/conan_runner.py"
         --build-dir ${CMAKE_BINARY_DIR}
         --compiler ${CMAKE_CXX_COMPILER_ID}
         --compiler-version ${CMAKE_CXX_COMPILER_VERSION}
         --build-types ${_build_types}
         --target-arch ${_target_arch}
         --build-arch ${CMAKE_HOST_SYSTEM_PROCESSOR}
         ${_libdir}
         ${_force_build}
         ${_disallow_prebuilt}
         ${_download_cache}
         ${_min_macos_version}
         -o ${conan_package_options}

      RESULT_VARIABLE conan_result
      COMMAND_ECHO STDOUT
   )

   if( conan_result )
      message( FATAL_ERROR "Conan failed to install dependencies (${conan_result}) ${PYTHON}" )
   endif()

   set(CMAKE_FIND_PACKAGE_PREFER_CONFIG TRUE)
endif()

if( ${_OPT}conan_enabled )
   set( _file_name "${CMAKE_BINARY_DIR}/generators/pre-find-package.cmake" )

   if( EXISTS "${_file_name}" )
      message(STATUS "Including ${_file_name}")
      include( ${_file_name} )
   endif()
endif()
# Resolve the dependencies
include(${find_package_file})

# Monkey-patch some targets to make the names consistent
file(GLOB dependency_helpers "${AUDACITY_MODULE_PATH}/dependencies/*.cmake")

foreach(f ${dependency_helpers})
    include(${f})
endforeach()

if( ${_OPT}conan_enabled )
   set( _file_name "${CMAKE_BINARY_DIR}/generators/post-find-package.cmake" )
   if( EXISTS "${_file_name}" )
      message(STATUS "Including ${_file_name}")
      include( ${_file_name} )
   endif()
endif()
