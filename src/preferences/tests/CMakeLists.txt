#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST preferences_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/commonaudioapiconfigurationmodel_tests.cpp

    # The model under test is compiled in directly: linking the whole
    # preferences_qml module would drag in every preferences model and their
    # cross-module dependencies.
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h
    )

set(MODULE_TEST_LINK
    Qt6::Qml
    Qt6::Quick
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)

# The model header carries Q_OBJECT; the plain test executable needs moc.
set_target_properties(${MODULE_TEST} PROPERTIES AUTOMOC ON)
