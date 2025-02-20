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

    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/insertsilencemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/insertsilencemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/customcursor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/customcursor.h

    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addeffectmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/addeffectmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.h
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
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.h

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

    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/isamplespainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/samplespainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/samplespainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/samplespainterutils.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/samplespainterutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/iminmaxrmspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/minmaxrmspainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/minmaxrmspainter.h 
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/iconnectingdotspainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/connectingdotspainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/connectingdotspainter.h 
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/iau3wavepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/sampledata.h 
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformScale.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformScale.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformSettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformSettings.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformPainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveformPainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/WaveMetrics.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/wavepainterutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/au3/wavepainterutils.h

    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/cliplistitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/cliplistitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/multiclipcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/multiclipcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackslistclipsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackslistclipsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/wavepainterproxy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/wavepainterproxy.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/iwavepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectionviewcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectionviewcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/pitchandspeedchangemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/pitchandspeedchangemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectioncontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectioncontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/canvascontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/canvascontextmenumodel.h

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
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/snaptimeformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/snaptimeformatter.h

    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playpositionactioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playpositionactioncontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/statusbar/selectionstatusmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/statusbar/selectionstatusmodel.h
    )

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_LINK trackedit)

set(MODULE_INCLUDE
    ${CLIPSVIEW_V1_INC}
)

set(MODULE_DEF
    ${CLIPSVIEW_V1_DEF}
)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_PROJECTSCENE_TESTS)
    add_subdirectory(tests)
endif()
