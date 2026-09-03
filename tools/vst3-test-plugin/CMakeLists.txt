#
# Audacity: A Digital Audio Editor
#
# Developer tool: a VST3 effect whose module load is controlled by a validation gate file, for
# testing plugin validation and loading (see README.md), plus a small Qt controller
# app. Opt in with -DAU_BUILD_VST3_TEST_PLUGIN=ON.
#

option(AU_BUILD_VST3_TEST_PLUGIN
       "Build VST3 test plugin and its controller (developer tool, see tools/vst3-test-plugin)" OFF)

# The deploy target must always exist so a pre-launch task (.vscode/tasks.json) can
# invoke it without breaking F5 for anyone not building the plugin. The real deploy
# target further down replaces this no-op when the plugin is actually built.
function(au_test_vst3_noop_deploy_target reason)
    add_custom_target(deploy_test_vst3
        COMMAND ${CMAKE_COMMAND} -E echo "[vst3-test-plugin] ${reason}, nothing to deploy"
        VERBATIM
    )
endfunction()

if (NOT AU_BUILD_VST3_TEST_PLUGIN)
    au_test_vst3_noop_deploy_target("not built (AU_BUILD_VST3_TEST_PLUGIN=OFF)")
    return()
endif()

set(AU_VST3_TEST_PLUGIN_NAME AuVst3TestPlugin)
# VST3 bundle root: <Name>.vst3 (the per-platform binary lives under Contents/)
set(AU_VST3_TEST_PLUGIN_BUNDLE ${CMAKE_BINARY_DIR}/test-plugins/${AU_VST3_TEST_PLUGIN_NAME}.vst3)

# ---- Controller (portable, Qt Widgets): installs the plugin and drives its validation gate file ----
find_package(Qt6 REQUIRED COMPONENTS Widgets)
add_executable(au_vst3_test_plugin_validation_controller controller/main.cpp)
target_link_libraries(au_vst3_test_plugin_validation_controller PRIVATE Qt6::Widgets)
target_compile_definitions(au_vst3_test_plugin_validation_controller PRIVATE
    "AU_VST3_TEST_PLUGIN_BUNDLE=\"${AU_VST3_TEST_PLUGIN_BUNDLE}\""
    "AU_VST3_TEST_PLUGIN_NAME=\"${AU_VST3_TEST_PLUGIN_NAME}\""
)
set_target_properties(au_vst3_test_plugin_validation_controller PROPERTIES
    CXX_STANDARD 17
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test-plugins
)

# ---- The plugin itself (Linux only for now) ----
if (NOT CMAKE_SYSTEM_NAME STREQUAL "Linux")
    message(WARNING "[vst3-test-plugin] plugin only implemented for Linux, skipping it (controller still built)")
    au_test_vst3_noop_deploy_target("plugin not built on this platform")
    return()
endif()

get_property(VST3_SDK_PATH GLOBAL PROPERTY vst3sdk_SOURCE_DIR)
if (NOT VST3_SDK_PATH)
    message(FATAL_ERROR "[vst3-test-plugin] vst3sdk not populated - the dependency manifest must run first")
endif()

# The SDK sources are compiled into the module (the in-tree vst_sdk_3 static
# library isn't built as position-independent code).
set(SDK_PLUGIN_SRC
    ${VST3_SDK_PATH}/public.sdk/source/main/pluginfactory.cpp
    ${VST3_SDK_PATH}/public.sdk/source/main/linuxmain.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/commoniids.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/commonstringconvert.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/pluginview.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstaudioeffect.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstbus.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstcomponent.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstcomponentbase.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstinitiids.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstparameters.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstsinglecomponenteffect.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/stringconvert.cpp
    ${VST3_SDK_PATH}/base/source/baseiids.cpp
    ${VST3_SDK_PATH}/base/source/fbuffer.cpp
    ${VST3_SDK_PATH}/base/source/fdebug.cpp
    ${VST3_SDK_PATH}/base/source/fdynlib.cpp
    ${VST3_SDK_PATH}/base/source/fobject.cpp
    ${VST3_SDK_PATH}/base/source/fstreamer.cpp
    ${VST3_SDK_PATH}/base/source/fstring.cpp
    ${VST3_SDK_PATH}/base/source/updatehandler.cpp
    ${VST3_SDK_PATH}/base/source/timer.cpp
    ${VST3_SDK_PATH}/base/thread/source/fcondition.cpp
    ${VST3_SDK_PATH}/base/thread/source/flock.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/conststringtable.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/coreiids.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/funknown.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/ustring.cpp
)

add_library(au_vst3_test_plugin_validation_gate MODULE vst3testplugin.cpp ${SDK_PLUGIN_SRC})

target_include_directories(au_vst3_test_plugin_validation_gate PRIVATE ${VST3_SDK_PATH})
target_compile_definitions(au_vst3_test_plugin_validation_gate PRIVATE $<IF:$<CONFIG:Debug>,DEVELOPMENT=1,RELEASE=1>)
# the SDK sources aren't warning-clean under the project's flags
target_compile_options(au_vst3_test_plugin_validation_gate PRIVATE -w)

# VST3 bundle layout: <Name>.vst3/Contents/<arch>-linux/<Name>.so
set_target_properties(au_vst3_test_plugin_validation_gate PROPERTIES
    OUTPUT_NAME ${AU_VST3_TEST_PLUGIN_NAME}
    PREFIX ""
    SUFFIX ".so"
    CXX_STANDARD 17
    POSITION_INDEPENDENT_CODE ON
    LIBRARY_OUTPUT_DIRECTORY ${AU_VST3_TEST_PLUGIN_BUNDLE}/Contents/${CMAKE_SYSTEM_PROCESSOR}-linux
)

# Deploy into the user's VST3 folder as an explicit, opt-in target.
configure_file(deploy.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/deploy_test_vst3.cmake @ONLY)
add_custom_target(deploy_test_vst3
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/deploy_test_vst3.cmake
    COMMENT "[vst3-test-plugin] deploying the test VST3 to the user's VST3 folder"
    VERBATIM
)
add_dependencies(deploy_test_vst3 au_vst3_test_plugin_validation_gate)

message(STATUS "[vst3-test-plugin] bundle: ${AU_VST3_TEST_PLUGIN_BUNDLE} (deploy with the `deploy_test_vst3` target)")
