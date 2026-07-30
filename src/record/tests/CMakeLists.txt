#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST record_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/recordcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../audio/tests/mocks/audiodrivercontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../context/tests/mocks/globalcontextmock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../playback/tests/mocks/playbackcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../project/tests/mocks/audacityprojectmock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../trackedit/tests/mocks/selectioncontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../trackedit/tests/mocks/trackeditinteractionmock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../trackedit/tests/mocks/tracknavigationcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../trackedit/tests/mocks/tracksinteractionmock.h
    )

set(MODULE_TEST_LINK
    record
    playback
    trackedit
    au3wrap
    muse::uicomponents
    muse::ui
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
