#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST appshell_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/factoryreset_actioncontroller_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/appshellconfigurationmock.h
)

set(MODULE_TEST_LINK
    appshell
    project
)

set(MODULE_TEST_INCLUDE
    ${MUSE_FRAMEWORK_PATH}/framework/multiwindows
    ${MUSE_FRAMEWORK_PATH}/framework/actions
    ${MUSE_FRAMEWORK_PATH}/framework/interactive
    ${MUSE_FRAMEWORK_PATH}/framework/global
    ${CMAKE_SOURCE_DIR}/src/appshell
    ${CMAKE_SOURCE_DIR}/src/record
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
