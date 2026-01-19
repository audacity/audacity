set(MODULE_TEST au3wrap_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/cloud_tests.cpp
    )

set(MODULE_TEST_LINK
    au3wrap
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
