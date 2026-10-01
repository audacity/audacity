#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3cloud_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/checkoutrebase_tests.cpp
)

set(MODULE_TEST_LINK
    au3-cloud-audiocom
    au3-project-file-io
)

include(SetupGTest)
