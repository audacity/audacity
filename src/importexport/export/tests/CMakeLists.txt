#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST exporter_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/exportfilename_tests.cpp
)

set(MODULE_TEST_LINK Qt6::Core)

include(SetupGTest)
