#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST projectscene_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/sample_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/snaptimeformatter_tests.cpp
    
    ${CMAKE_CURRENT_LIST_DIR}/mocks/globalcontextmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audacity4projectmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/projectsceneconfigurationmock.h
)

set(MODULE_TEST_LINK

    projectscene
    trackedit
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
