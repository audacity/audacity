#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST projectscene_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectviewstate_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/sample_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/snaptimeformatter_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tracklabelslayoutmanager_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/trackitemsselection_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/findguideline_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/playcursorcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/playpositionactioncontroller_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/snaptestaccess.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/globalcontextmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audacity4projectmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/projectsceneconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audiooutputmock.h
)

set(MODULE_TEST_LINK
    projectscene
    trackedit
    playback
    record
    context
    au3wrap
    au3-audio-graph
    au3-channel
    au3-files
    au3-math
    au3-mixer
    au3-playable-track
    au3-preferences
    au3-sample-track
    au3-stretching-sequence
    au3-strings
    au3-time-frequency-selection
    au3-wave-track
    au3-xml
    au3-wave-track-paint
    au3-import-export
    au3-label-track
    au3-project-file-io
    au3-musehub
    au3-track

    muse::ui
    muse::uicomponents
    muse::workspace
)

set(MODULE_TEST_INCLUDE ${CMAKE_CURRENT_LIST_DIR}/..)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
