#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST context_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/mocks/globalcontextmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/playbackstatemock.h
    )

set(MODULE_TEST_LINK
    context
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
