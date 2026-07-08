set(MODULE_TEST au_exporter_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/originalfileinfo_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/overwriteoriginalsettings_tests.cpp
)

include(${CMAKE_CURRENT_LIST_DIR}/../../../au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE ${AU3_INCLUDE})
set(MODULE_TEST_DEF ${AU3_DEF})

set(MODULE_TEST_LINK
    au3-project
    au3-import-export
    exporter
)

include(SetupGTest)
