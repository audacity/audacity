#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST preferences_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/commonaudioapiconfigurationmodel_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h
    )

set(MODULE_TEST_LINK
    audio
    Qt6::Qml
    Qt6::Quick
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
set_target_properties(${MODULE_TEST} PROPERTIES AUTOMOC ON)
