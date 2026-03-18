#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST appshell_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/factoryreset_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/workspaceconfigurationmock.h
)

set(MODULE_TEST_LINK
    appshell
    muse::workspace
)

set(MODULE_TEST_INCLUDE
    ${MUSE_FRAMEWORK_PATH}/framework/workspace
    ${MUSE_FRAMEWORK_PATH}/framework/multiwindows
)

set(MODULE_TEST_DEF
    BUILTIN_WORKSPACES_DIR="${CMAKE_SOURCE_DIR}/share/workspaces"
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
