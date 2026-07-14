#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audiodevicesprovider_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/au3devicemanagerfake.h
    ${CMAKE_SOURCE_DIR}/src/audio/tests/mocks/audioenginemock.h
)

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE ${AU3_INCLUDE})
set(MODULE_TEST_DEF ${AU3_DEF})

set(MODULE_TEST_LINK
    au3audio
    au3wrap
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
