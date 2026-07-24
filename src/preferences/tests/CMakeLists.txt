#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST preferences_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/commonaudioapiconfigurationmodel_tests.cpp

    ${CMAKE_SOURCE_DIR}/src/audio/tests/mocks/audiodevicesprovidermock.h
)

set(MODULE_TEST_LINK
    preferences
    preferences_qml
    playback
    projectscene
    record

    muse::ui

    Qt6::Qml
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
