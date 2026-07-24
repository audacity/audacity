#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_audacity_plugin_tests)

set(MODULE_TEST_SRC
    ${PROJECT_SOURCE_DIR}/au3/tests/MockedPrefs.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacitypluginparameterextractorservice_tests.cpp
)

include(${CMAKE_CURRENT_LIST_DIR}/../../../au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    ${PROJECT_SOURCE_DIR}/au3/tests
    ${PROJECT_SOURCE_DIR}/au3/libraries/au3-preferences
    ${AU3_INCLUDE}
)
set(MODULE_TEST_DEF ${AU3_DEF})
set(MODULE_TEST_LINK
    effects_audacity_plugin
    au3wrap
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
