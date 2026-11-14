#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST automation_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/mocks/clipgaininteractionmock.h
    )

set(MODULE_TEST_LINK
    automation
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
