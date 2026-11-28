#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST importer_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/legacyaupimporter_tests.cpp
    )

set(MODULE_TEST_LINK
    importer
    SndFile::sndfile
    )

if (TARGET projectscene)
    list(APPEND MODULE_TEST_SRC ${CMAKE_CURRENT_LIST_DIR}/au3importerror_tests.cpp)
    list(APPEND MODULE_TEST_LINK
        iex_labels
        au3wrap
        au3-import-export
        projectscene
        muse::ui
        )
endif()

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
