#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST trackedit_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/tests/tracktemplatefactory.cpp

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
        au3-label-track
        au3-time-frequency-selection
        au3-wave-track

        muse::ui
        muse::ui_qml

        Qt::Quick
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
