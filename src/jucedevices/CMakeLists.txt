#
# Audacity: A Digital Audio Editor
#

declare_module(jucedevices)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/jucedevicesmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/jucedevicesmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ijucedeviceaccess.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/juceaudiodevices.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/juceaudiodevices.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/jucedevice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/jucedevice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/juceincludes.h
)
# JUCE headers clash with the `using namespace muse` of other unity-combined sources
set_source_files_properties(${MODULE_SRC} PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)

set(MODULE_LINK
    audio
    au3-audio-devices # DeviceManager's standard rates
    wxBase # DeviceManager.h
    juce
)

setup_module()

if (AU_BUILD_JUCEDEVICES_TESTS)
    add_subdirectory(tests)
endif()
