#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST preferences_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/commonaudioapiconfigurationmodel_tests.cpp
)

set(MODULE_TEST_LINK
    preferences_qml
    playback
    projectscene
    record

    muse::uicomponents
    muse::ui

    Qt::Quick
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
