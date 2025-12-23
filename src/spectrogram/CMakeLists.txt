#
# Audacity: A Digital Audio Editor
#

declare_module(spectrogram)

set(MODULE_QRC spectrogram.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/ispectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iglobalspectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackspectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ispectrogramservice.h
    ${CMAKE_CURRENT_LIST_DIR}/spectrogrammodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/spectrogrammodule.h
    ${CMAKE_CURRENT_LIST_DIR}/spectrogramtypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/ispectrogrampainter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/iclipchannelreader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/clipparameters.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/clipparameters.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/macromagic.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/numberscale.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/roseuscolormaps.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolordefinitions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolordefinitions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolorregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolorregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolors.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramcolors.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/globalspectrogramconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/globalspectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/spectrogramservice.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipchannelreader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipchannelreader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3concurrentclipchannelreader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3concurrentclipchannelreader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogrampainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogrampainter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogrampainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogrampainter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogramclipchannelpainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogramclipchannelpainter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogramsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogramsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3spectrogramutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackspectrogramconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackspectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/SpectrumCache.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/SpectrumCache.h
    
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractsectionparameterslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractsectionparameterslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractspectrogramsettingsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractspectrogramsettingsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/algorithmsectionparameterlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/algorithmsectionparameterlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/colorsectionparameterlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/colorsectionparameterlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/scalesectionparameterlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/scalesectionparameterlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/globalspectrogramsettingsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/globalspectrogramsettingsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipchannelspectrogramview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipchannelspectrogramview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/spectrogramviewutils.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_USE_UNITY OFF)
set(MODULE_LINK au3wrap Qt6::Concurrent tft)

setup_module()
