set(MODULE_TEST au_exporter_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/originalfileinfo_tests.cpp
)

set(MODULE_TEST_LINK
    exporter
)

include(SetupGTest)
