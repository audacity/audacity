#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au_projectrecovery_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/testprojectbuilder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/testprojectbuilder.h
    ${CMAKE_CURRENT_LIST_DIR}/binaryxml_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/documentedit_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectdocument_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectinspector_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectrepairer_tests.cpp
    )

set(MODULE_TEST_LINK
    projectrecovery
    sqlite
    au3-project
    au3-sqlite-helpers
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
