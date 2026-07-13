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
    effects_base
    muse_audioplugins
    au3-preferences
)

# AU3 - needed because test files include AU3 headers
include(${PROJECT_SOURCE_DIR}/src/au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE
    ${AU3_INCLUDE}
    ${PROJECT_SOURCE_DIR}/src/effects/effects_base # for internal headers included relatively (e.g. realtimeeffectservice.h)
)
set(MODULE_TEST_DEF ${AU3_DEF})
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
