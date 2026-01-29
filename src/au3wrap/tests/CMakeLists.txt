#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au_au3wrap_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/mocks/au3projectmock.h
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
