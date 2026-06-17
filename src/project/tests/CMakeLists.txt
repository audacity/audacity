#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au_project_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/audacityprojectmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/projectconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/trackeditprojectcreatormock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/dummyeffectinstancefactory.h
    ${CMAKE_CURRENT_LIST_DIR}/audacityproject_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/realtimeeffectspersistence_tests.cpp
)

set(MODULE_TEST_LINK
    project
    au3wrap
    trackedit
    muse::actions
    muse::draw
    sqlite # required for SQLITE_CANTOPEN and SQLITE_READONLY macros used in audacityproject_tests.cpp
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
