#
# Audacity: A Digital Audio Editor
#

muse_create_module(uicomponents)

target_sources(uicomponents PRIVATE
    uicomponentsmodule.cpp
    uicomponentsmodule.h
    types/numerictypes.h

    components/bpmmodel.cpp
    components/bpmmodel.h
    components/frequencymodel.cpp
    components/frequencymodel.h
    components/timecodemodel.cpp
    components/timecodemodel.h

    components/timecodemodeselector.cpp
    components/timecodemodeselector.h

    components/tablesortfilterproxymodel.cpp
    components/tablesortfilterproxymodel.h

    components/internal/numeric/numericviewmodel.cpp
    components/internal/numeric/numericviewmodel.h
    components/internal/numeric/timecodeformatter.cpp
    components/internal/numeric/timecodeformatter.h
    components/internal/numeric/numericformatter.cpp
    components/internal/numeric/numericformatter.h
    components/internal/numeric/beatsformatter.cpp
    components/internal/numeric/beatsformatter.h
    components/internal/numeric/fieldsinteractioncontroller.cpp
    components/internal/numeric/fieldsinteractioncontroller.h
    )

qt_add_resources(uicomponents_RCC_SOURCES au_uicomponents.qrc)
target_sources(uicomponents PRIVATE ${uicomponents_RCC_SOURCES})

add_qml_import_path(${CMAKE_CURRENT_LIST_DIR}/qml)

target_link_libraries(uicomponents PRIVATE
    muse::uicomponents_qml
)

if (AU_BUILD_UICOMPONENTS_TESTS)
    add_subdirectory(tests)
endif()
