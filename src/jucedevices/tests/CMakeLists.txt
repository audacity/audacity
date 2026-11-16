#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST jucedevices_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/juceaudiodevices_tests.cpp
    )

set(MODULE_TEST_LINK
    jucedevices
    )

# JUCE headers clash with the `using namespace muse` of other unity-combined sources
set_source_files_properties(${MODULE_TEST_SRC} PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
