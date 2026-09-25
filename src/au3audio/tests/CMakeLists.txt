#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3audiodrivercontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audioioinputchannelselection_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audioioinputmeter_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/inputchannelselection_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/inputchannelselectionsettings_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    )

set(MODULE_TEST_LINK
    au3audio
    au3wrap
    au3-audio-io
    $<$<BOOL:${AU_USE_PORTMIXER}>:portmixer>
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
