#
# Audacity: A Digital Audio Editor
#

# The audio engine of graphs of nodes, over any audio library
declare_module(au4audio)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au4audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au4audiomodule.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au4audioengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au4audioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectscratch.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectscratch.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsnode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsnode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/endoftracknode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/endoftracknode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/gainpannode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/gainpannode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/ieffectscontext.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/iendoftrackcontext.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/imutesolocontext.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/iocallback.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/iocallback.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/mutesolonode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/mutesolonode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playedrange.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playheadmapper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playheadmapper.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/sequentialmixnode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/sequentialmixnode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/stereo.h
)
# Their `using namespace au::au4audio` would leak into other unity-combined sources
set_source_files_properties(${MODULE_SRC} PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)

set(MODULE_LINK
    au3-clip-processing
    au3-mixer
    audio
    portaudiodevices
    au3wrap
    au3-project
    au3-realtime-effects
    au3-wave-track
)

setup_module()

if (AU_BUILD_AU4AUDIO_TESTS)
    add_subdirectory(tests)
endif()
