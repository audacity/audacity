#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST macromanager_qml_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/commandparamsmodel_tests.cpp
)

set(MODULE_TEST_LINK
    macromanager_qml
)

include(SetupGTest)
