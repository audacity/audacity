#
# Audacity
# A Digital Audio Editor
#

set(MODULE_TEST exporter_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/exportutils_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/metadataroundtrip_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../../../au3wrap/tests/tracktemplatefactory.cpp
    )

set(MODULE_TEST_LINK
    exporter
    au3wrap
    au3-import-export
    au3-import-export-modules
    au3-tags
    au3-wave-track
    SndFile::sndfile # SF_FORMAT_* constants for the AIFF export parameters
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
