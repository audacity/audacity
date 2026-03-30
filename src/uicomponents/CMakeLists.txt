#
# Audacity: A Digital Audio Editor
#

declare_module(uicomponents)

set(MODULE_QRC au_uicomponents.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/uicomponentsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/uicomponentsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/types/numerictypes.h

    ${CMAKE_CURRENT_LIST_DIR}/components/bpmmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/bpmmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/components/frequencymodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/frequencymodel.h
    ${CMAKE_CURRENT_LIST_DIR}/components/timecodemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/timecodemodel.h

    ${CMAKE_CURRENT_LIST_DIR}/components/timecodemodeselector.cpp

    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/numericviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/numericviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/timecodeformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/timecodeformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/numericformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/numericformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/beatsformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/beatsformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/fieldsinteractioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/components/internal/numeric/fieldsinteractioncontroller.h

)

set(MODULE_LINK
    muse::uicomponents_qml
)

setup_module()

if (AU_BUILD_UICOMONENTS_TESTS)
    add_subdirectory(tests)
endif()
