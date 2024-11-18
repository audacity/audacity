#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au_project_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audacityprojectmock.h
)

set(MODULE_TEST_LINK
    project
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
