#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST preferences_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/commonaudioapiconfigurationmodel_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h
    ${MUSE_FRAMEWORK_PATH}/framework/stubs/accessibility/accessibilitycontrollerstub.cpp
    ${MUSE_FRAMEWORK_PATH}/framework/stubs/languages/languagesservicestub.cpp
    )

set(MODULE_TEST_LINK
    audio
    uicomponents
    muse_actions
    muse_rcommand
    muse::graphicaleffects_qml
    muse::ui_qml
    muse::uicomponents_qml
    Qt6::Qml
    Qt6::Quick
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
set_target_properties(${MODULE_TEST} PROPERTIES AUTOMOC ON)
