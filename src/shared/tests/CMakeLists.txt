#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST shared_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/axisticks_tests.cpp
    )

set(MODULE_TEST_LINK
    shared
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
