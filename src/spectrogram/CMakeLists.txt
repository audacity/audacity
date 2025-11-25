#
# Audacity: A Digital Audio Editor
#

declare_module(spectrogram)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/ispectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/spectrogrammodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/spectrogrammodule.h
    ${CMAKE_CURRENT_LIST_DIR}/spectrogramtypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramconfiguration.h

    ${CMAKE_CURRENT_LIST_DIR}/view/abstractsectionparameterslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractsectionparameterslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractspectrogramsettingsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractspectrogramsettingsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/algorithmsectionparameterlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/algorithmsectionparameterlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/colorsectionparameterlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/colorsectionparameterlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/globalspectrogramsettingsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/globalspectrogramsettingsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/spectrogramviewutils.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_USE_UNITY OFF)
set(MODULE_LINK au3wrap)

setup_module()

