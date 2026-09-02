#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_base_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/config_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsutils_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/pluginmigration_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/realtimeeffectservice_tests.cpp
)

set(MODULE_TEST_LINK
        au3wrap
        au3-audio-graph
        au3-channel
        au3-components
        au3-exceptions
        au3-files
        au3-math
        au3-mixer
        au3-playable-track
        au3-project
        au3-realtime-effects
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-strings
        au3-track
        au3-utility
        au3-wave-track
        au3-xml
    effects_base
    muse_audioplugins
    au3-preferences

    Qt::Qml
)

set(MODULE_TEST_INCLUDE
    ${PROJECT_SOURCE_DIR}/src/effects/effects_base # for internal headers included relatively (e.g. realtimeeffectservice.h)
)
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
