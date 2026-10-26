#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST macromanager_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/macrooutputnames_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/macroserialization_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/macrosrepository_tests.cpp
)

set(MODULE_TEST_LINK
    macromanager
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
