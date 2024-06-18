#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST projectscene_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/sample_tests.cpp
)

set(MODULE_TEST_LINK

    projectscene
    processing
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
