#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3cloud_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/snapshotreconcile_tests.cpp
    )

set(MODULE_TEST_LINK
    au3wrap
    au3-cloud-audiocom
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
