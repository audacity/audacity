#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST playback_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/timecodemodel_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/playbackcontroller_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/playbackmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/playermock.h
    )

set(MODULE_TEST_LINK
    playback
    record
    trackedit

    muse::uicomponents
    muse::ui
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
