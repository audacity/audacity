#[[
A module to look for vst3sdk
]]

if( NOT vst3sdk_FOUND )
   if( DEFINED ENV{VST3_SDK_DIR} )
      set( vst3sdk_DIR $ENV{VST3_SDK_DIR} )
   elseif( DEFINED ENV{VST3SDK_PATH} )
      set( vst3sdk_DIR $ENV{VST3SDK_PATH} )
   elseif( DEFINED ENV{VST3SDK} )
      set( vst3sdk_DIR $ENV{VST3SDK} )
   else()
      set( vst3sdk_DIR ${CMAKE_SOURCE_DIR}/vst3sdk )
   endif()

   if( NOT EXISTS ${vst3sdk_DIR} )
      message( STATUS "VST3SDK not found. Please set VST3_SDK_DIR to the path to the vst3sdk directory." )
      return()
   endif()

   set( vst3sdk_BUILD ${CMAKE_BINARY_DIR}/vst3sdk )

   message( STATUS "VST3SDK_DIR: ${vst3sdk_DIR}" )
   message( STATUS "Building SDK in ${vst3sdk_BUILD}" )

   execute_process(COMMAND
      ${CMAKE_COMMAND}
         -G ${CMAKE_GENERATOR}
         -S ${vst3sdk_DIR}
         -B ${vst3sdk_BUILD}
         -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
         -D SMTG_ENABLE_VST3_HOSTING_EXAMPLES=OFF
         -D SMTG_ENABLE_VST3_PLUGIN_EXAMPLES=OFF
         -D SMTG_ENABLE_VSTGUI_SUPPORT=OFF
         -D SMTG_CREATE_BUNDLE_FOR_WINDOWS=Off
         -D SMTG_MYPLUGINS_SRC_PATH=""
         -D SMTG_RUN_VST_VALIDATOR=Off

      RESULT_VARIABLE vst3sdk_configure_result
   )

   if( vst3sdk_configure_result )
      message( FATAL_ERROR "Failed to configure VST3SDK" )
   endif()

   execute_process(COMMAND
      ${CMAKE_COMMAND}
         --build ${vst3sdk_BUILD}
         --config ${CMAKE_BUILD_TYPE}
         --parallel

      RESULT_VARIABLE vst3sdk_build_result
   )

   if( vst3sdk_build_result )
      message( FATAL_ERROR "Failed to build VST3SDK" )
   endif()

   add_library(vst3sdk::base STATIC IMPORTED GLOBAL)
   add_library(vst3sdk::pluginterfaces STATIC IMPORTED GLOBAL)
   add_library(vst3sdk::sdk_hosting STATIC IMPORTED GLOBAL)
   add_library(vst3sdk::sdk_common STATIC IMPORTED GLOBAL)

   set_target_properties(vst3sdk::base PROPERTIES
      IMPORTED_LOCATION ${vst3sdk_BUILD}/lib/${CMAKE_BUILD_TYPE}/${CMAKE_STATIC_LIBRARY_PREFIX}base${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${vst3sdk_DIR}
   )

   set_target_properties(vst3sdk::pluginterfaces PROPERTIES
      IMPORTED_LOCATION ${vst3sdk_BUILD}/lib/${CMAKE_BUILD_TYPE}/${CMAKE_STATIC_LIBRARY_PREFIX}pluginterfaces${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${vst3sdk_DIR}
   )

   set_target_properties(vst3sdk::sdk_hosting PROPERTIES
      IMPORTED_LOCATION ${vst3sdk_BUILD}/lib/${CMAKE_BUILD_TYPE}/${CMAKE_STATIC_LIBRARY_PREFIX}sdk_hosting${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${vst3sdk_DIR}
   )

   set_target_properties(vst3sdk::sdk_common PROPERTIES
      IMPORTED_LOCATION ${vst3sdk_BUILD}/lib/${CMAKE_BUILD_TYPE}/${CMAKE_STATIC_LIBRARY_PREFIX}sdk_common${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${vst3sdk_DIR}
   )

   find_package(X11 REQUIRED)

   if( X11_FOUND )
      target_link_libraries(vst3sdk::base INTERFACE X11::X11)
   endif()

   set(vst3sdk_FOUND TRUE)
endif()
