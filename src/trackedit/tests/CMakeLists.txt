#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST trackedit_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tracktemplatefactory.h
    ${CMAKE_CURRENT_LIST_DIR}/tracktemplatefactory.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/clipboardmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/selectioncontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/trackeditprojectmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/projecthistorymock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/tracknavigationcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/navigationcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/trackeditinteractionmock.h

    ${CMAKE_CURRENT_LIST_DIR}/trackeditactionscontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tracknavigationcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tracknavigationmodel_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3tracksinteraction_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3clipsinteraction_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3labelsinteractions_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/changedetection_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3trackeditclipboard_tests.cpp
    )

set(MODULE_TEST_LINK
        trackedit
        spectrogram
        au3wrap
        au3-audio-graph
        au3-channel
        au3-exceptions
        au3-files
        au3-label-track
        au3-math
        au3-mixer
        au3-playable-track
        au3-preferences
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-time-frequency-selection
        au3-track
        au3-utility
        au3-wave-track
        au3-xml
        au3-strings

        muse::ui
        muse::ui_qml

        Qt::Quick
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
