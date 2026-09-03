#
# Audacity: A Digital Audio Editor
#
# Developer tool: a VST3 effect whose module load is controlled by a gate file, for
# testing plugin validation and loading (see README.md), plus a small Qt controller
# app. Opt in with -DAU_BUILD_TEST_VST3_GATE_PLUGIN=ON.
#

set(AU_TEST_VST3_GATE_NAME AuTestGate)
# VST3 bundle root: <Name>.vst3 (the per-platform binary lives under Contents/)
set(AU_TEST_VST3_GATE_BUNDLE ${CMAKE_BINARY_DIR}/test-plugins/${AU_TEST_VST3_GATE_NAME}.vst3)

# ---- Controller (portable, Qt Widgets): installs the plugin and drives its gate file ----
find_package(Qt6 REQUIRED COMPONENTS Widgets)
add_executable(au_test_vst3_gate_controller controller/main.cpp)
target_link_libraries(au_test_vst3_gate_controller PRIVATE Qt6::Widgets)
target_compile_definitions(au_test_vst3_gate_controller PRIVATE
    "AU_TEST_VST3_GATE_BUNDLE=\"${AU_TEST_VST3_GATE_BUNDLE}\""
    "AU_TEST_VST3_GATE_NAME=\"${AU_TEST_VST3_GATE_NAME}\""
)
set_target_properties(au_test_vst3_gate_controller PROPERTIES
    CXX_STANDARD 17
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test-plugins
)

# ---- The plugin itself (Linux only for now) ----
if (NOT CMAKE_SYSTEM_NAME STREQUAL "Linux")
    message(WARNING "[test-vst3-gate] plugin only implemented for Linux, skipping it (controller still built)")
    return()
endif()

get_property(VST3_SDK_PATH GLOBAL PROPERTY vst3sdk_SOURCE_DIR)
if (NOT VST3_SDK_PATH)
    message(FATAL_ERROR "[test-vst3-gate] vst3sdk not populated - the dependency manifest must run first")
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

add_library(au_test_vst3_gate MODULE autestgate.cpp ${SDK_PLUGIN_SRC})

target_include_directories(au_test_vst3_gate PRIVATE ${VST3_SDK_PATH})
target_compile_definitions(au_test_vst3_gate PRIVATE $<IF:$<CONFIG:Debug>,DEVELOPMENT=1,RELEASE=1>)
# the SDK sources aren't warning-clean under the project's flags
target_compile_options(au_test_vst3_gate PRIVATE -w)

# VST3 bundle layout: <Name>.vst3/Contents/<arch>-linux/<Name>.so
set_target_properties(au_test_vst3_gate PROPERTIES
    OUTPUT_NAME ${AU_TEST_VST3_GATE_NAME}
    PREFIX ""
    SUFFIX ".so"
    CXX_STANDARD 17
    POSITION_INDEPENDENT_CODE ON
    LIBRARY_OUTPUT_DIRECTORY ${AU_TEST_VST3_GATE_BUNDLE}/Contents/${CMAKE_SYSTEM_PROCESSOR}-linux
)

message(STATUS "[test-vst3-gate] bundle: ${AU_TEST_VST3_GATE_BUNDLE} (symlink it into ~/.vst3)")
