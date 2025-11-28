#
# Audacity: A Digital Audio Editor
#

declare_module(projectscene)

set(MODULE_QRC projectscene.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectscenemodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectscenemodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectsceneconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectviewstate.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectviewstatecreator.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectsceneactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/types/projectscenetypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/irealtimeeffectpaneltrackselection.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectpaneltrackselection.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectpaneltrackselection.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstate.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstate.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstatecreator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstatecreator.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/tapholdshortcut.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/tapholdshortcut.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/viewinfo.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/viewinfo.h

    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/trackviewstatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/trackviewstatemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/customcursor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/customcursor.h

    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addeffectmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addeffectmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/wavetrackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/wavetrackitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/labeltrackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/labeltrackitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistitemmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistitemmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistitemmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectlistitemmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectmenumodelbase.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectmenumodelbase.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectsectionmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/realtimeeffectsectionmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/paneltrackslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/paneltrackslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addnewtrackpopupmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addnewtrackpopupmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/undoredotoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/undoredotoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/snaptoolbaritem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/snaptoolbaritem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/workspacestoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/workspacestoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/audiosetupcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/audiosetupcontextmenumodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/isamplespainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/samplespainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/samplespainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/samplespainterutils.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/samplespainterutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/iminmaxrmspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/minmaxrmspainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/minmaxrmspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/iconnectingdotspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/connectingdotspainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/connectingdotspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/iau3wavepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/sampledata.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/WaveformSettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/WaveformSettings.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/WaveformPainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/WaveformPainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/WaveMetrics.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/wavepainterutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/au3/wavepainterutils.h

    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/viewtrackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/viewtrackitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackitemslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackitemslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackclipslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackclipslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackclipitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/trackclipitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelslayoutmanager.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelslayoutmanager.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/tracklabelitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/clipcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/clipcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/multiclipcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/multiclipcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/labelcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/labelcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/viewtrackslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/viewtrackslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/wavepainterproxy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/wavepainterproxy.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/spectrogramview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/spectrogramview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/waveview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/waveview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/iwavepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/selectionviewcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/selectionviewcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/pitchandspeedchangemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/pitchandspeedchangemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/selectioncontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/selectioncontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/canvascontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/canvascontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/splittoolcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/splittoolcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/mousehelper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracksitemsview/mousehelper.h

    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontext.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontext.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelineruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelineruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/irulerformat.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timeformat.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timeformat.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/beatsmeasuresformat.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/beatsmeasuresformat.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/gridlines.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/gridlines.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/playregioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/playregioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/playregionmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/playregionmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/snaptimeformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/snaptimeformatter.h

    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playpositionactioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playpositionactioncontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/statusbar/selectionstatusmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/statusbar/selectionstatusmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/historypanel/historypanelmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/historypanel/historypanelmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogmonoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogmonoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogstereoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogstereoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogbaseruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblogbaseruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearmonoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearmonoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearstereoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearstereoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearbaseruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/dblinearbaseruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/itrackruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearstereoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearstereoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearmonoruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearmonoruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearbaseruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/linearbaseruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/trackrulermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackruler/trackrulermodel.h

    )

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_LINK au3wrap trackedit uicomponents)

set(MODULE_INCLUDE ${CLIPSVIEW_V1_INC})

set(MODULE_DEF ${CLIPSVIEW_V1_DEF})

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_PROJECTSCENE_TESTS)
    add_subdirectory(tests)
endif()
