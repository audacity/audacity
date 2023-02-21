#[[
A module to look for crashpad
]]

if( NOT Crashpad_FOUND )
   if( NOT DEFINED ENV{Crashpad_ROOT} )
      message( STATUS "Crashpad not found. Please set checkout crashpad sources and make sure Crashpad_ROOT environment variable exists." )
      return()
   endif()

   set( CRASHPAD_OUT_DIR ${CMAKE_BINARY_DIR}/crashpad/Debug)
   set( CRASHPAD_BUILD_DIR ${CRASHPAD_OUT_DIR}/build)

   set(GN_COMMAND)
   set(GN_ARGS)
   if(CMAKE_SYSTEM_NAME MATCHES "Windows")
      set(GN_COMMAND cmd /C gn gen )
      set(GN_ARGS "--args=is_debug=true extra_cflags=\"/MDd\"")
   else()
      set(GN_COMMAND gn gen)
      set(GN_ARGS "--args=is_debug=true")
   endif()
   
   execute_process(
      COMMAND ${GN_COMMAND} ${CRASHPAD_BUILD_DIR} ${GN_ARGS}
      WORKING_DIRECTORY $ENV{Crashpad_ROOT}/crashpad
      COMMAND_ECHO STDOUT
      RESULT_VARIABLE CRASHPAD_GN_RESULT
   )
   if( CRASHPAD_GN_RESULT)
      message( FATAL_ERROR "Failed to configure crashpad" )
   endif()

   execute_process(
      COMMAND ninja -C ${CRASHPAD_BUILD_DIR}
      RESULT_VARIABLE CRASHPAD_NINJA_RESULT
   )
   if(CRASHPAD_NINJA_RESULT)
      message( FATAL_ERROR "Failed to build ${CONFIG} crashpad" )
   endif()

   add_library(crashpad::util STATIC IMPORTED GLOBAL)
   add_library(crashpad::context STATIC IMPORTED GLOBAL)
   add_library(crashpad::mini_chromium_base STATIC IMPORTED GLOBAL)
   add_library(crashpad::client_common STATIC IMPORTED GLOBAL)
   add_library(crashpad::snapshot STATIC IMPORTED GLOBAL)
   add_library(crashpad::minidump STATIC IMPORTED GLOBAL)
   add_library(crashpad::client STATIC IMPORTED GLOBAL)
   add_executable(crashpad::handler IMPORTED GLOBAL)

   file(
      COPY 
         $ENV{Crashpad_ROOT}/crashpad/client
         $ENV{Crashpad_ROOT}/crashpad/minidump
         $ENV{Crashpad_ROOT}/crashpad/third_party/mini_chromium/mini_chromium/base
         $ENV{Crashpad_ROOT}/crashpad/third_party/mini_chromium/mini_chromium/build
         $ENV{Crashpad_ROOT}/crashpad/util
         ${CRASHPAD_BUILD_DIR}/gen/build
      DESTINATION ${CRASHPAD_OUT_DIR}/headers
      FILES_MATCHING PATTERN "*.h"
   )

   set_target_properties(crashpad::util PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/util/${CMAKE_STATIC_LIBRARY_PREFIX}util${CMAKE_STATIC_LIBRARY_SUFFIX}
   )
   set_target_properties(crashpad::context PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/snapshot/${CMAKE_STATIC_LIBRARY_PREFIX}context${CMAKE_STATIC_LIBRARY_SUFFIX}
   )
   set_target_properties(crashpad::mini_chromium_base PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/third_party/mini_chromium/mini_chromium/base/${CMAKE_STATIC_LIBRARY_PREFIX}base${CMAKE_STATIC_LIBRARY_SUFFIX}
   )
   set_target_properties(crashpad::client_common PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/client/${CMAKE_STATIC_LIBRARY_PREFIX}common${CMAKE_STATIC_LIBRARY_SUFFIX}
   )
   set_target_properties(crashpad::snapshot PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/snapshot/${CMAKE_STATIC_LIBRARY_PREFIX}snapshot${CMAKE_STATIC_LIBRARY_SUFFIX}
   )
   set_target_properties(crashpad::minidump PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/minidump/${CMAKE_STATIC_LIBRARY_PREFIX}minidump${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${CRASHPAD_OUT_DIR}/headers
   )
   set_target_properties(crashpad::client PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/obj/client/${CMAKE_STATIC_LIBRARY_PREFIX}client${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${CRASHPAD_OUT_DIR}/headers
   )
   set_target_properties(crashpad::handler PROPERTIES
      IMPORTED_LOCATION ${CRASHPAD_BUILD_DIR}/crashpad_handler${CMAKE_EXECUTABLE_SUFFIX}
   )
   
   set(CrashpadDebug_FOUND TRUE)
endif()
