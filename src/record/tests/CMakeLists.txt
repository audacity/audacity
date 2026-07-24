#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST record_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordmock.h

    ${CMAKE_CURRENT_LIST_DIR}/recordcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3record_tests.cpp
    )

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    )

set(MODULE_TEST_LINK
    record
    au3wrap

    muse::ui
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
