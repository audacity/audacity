/*
* Audacity: A Digital Audio Editor
*/
#include "projectscenemodule.h"

#include <QtQml>

#include "types/projectscenetypes.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/projectsceneuiactions.h"
#include "internal/projectsceneactionscontroller.h"
#include "internal/projectsceneconfiguration.h"
#include "internal/projectviewstatecreator.h"
#include "internal/realtimeeffectpaneltrackselection.h"

#include "view/common/tracksviewstatemodel.h"
#include "view/common/trackviewstatemodel.h"
#include "view/common/customcursor.h"

#include "view/toolbars/audiosetupcontextmenumodel.h"
#include "view/toolbars/projecttoolbarmodel.h"
#include "view/toolbars/undoredotoolbarmodel.h"
#include "view/toolbars/workspacestoolbarmodel.h"

#include "view/trackspanel/paneltrackslistmodel.h"
#include "view/trackspanel/addeffectmenumodel.h"
#include "view/trackspanel/realtimeeffectlistmodel.h"
#include "view/trackspanel/realtimeeffectlistitemmenumodel.h"
#include "view/trackspanel/realtimeeffectsectionmodel.h"
#include "view/trackspanel/trackcontextmenumodel.h"
#include "view/trackspanel/trackitem.h"
#include "view/trackspanel/addnewtrackpopupmodel.h"

#include "view/tracksitemsview/viewtrackslistmodel.h"
#include "view/tracksitemsview/trackclipslistmodel.h"
#include "view/tracksitemsview/trackclipitem.h"
#include "view/tracksitemsview/tracklabelslistmodel.h"
#include "view/tracksitemsview/tracklabelslayoutmanager.h"
#include "view/tracksitemsview/tracklabelitem.h"
#include "view/tracksitemsview/waveview.h"
#include "view/tracksitemsview/clipcontextmenumodel.h"
#include "view/tracksitemsview/multiclipcontextmenumodel.h"
#include "view/tracksitemsview/labelcontextmenumodel.h"
#include "view/tracksitemsview/canvascontextmenumodel.h"
#include "view/tracksitemsview/selectioncontextmenumodel.h"
#include "view/tracksitemsview/selectionviewcontroller.h"
#include "view/tracksitemsview/splittoolcontroller.h"
#include "view/tracksitemsview/pitchandspeedchangemodel.h"
#include "view/tracksitemsview/wavepainterproxy.h"
#include "view/tracksitemsview/au3/connectingdotspainter.h"
#include "view/tracksitemsview/au3/minmaxrmspainter.h"
#include "view/tracksitemsview/au3/samplespainter.h"
#include "view/tracksitemsview/mousehelper.h"
#include "view/tracksitemsview/dropcontroller.h"
#include "view/tracksitemsview/labeleditor/labelstableviewmodel.h"
#include "view/tracksitemsview/labeleditor/addnewlabeltrackmodel.h"

#include "view/timeline/timelinecontext.h"
#include "view/timeline/timelineruler.h"
#include "view/timeline/timelinecontextmenumodel.h"
#include "view/timeline/playregioncontroller.h"
#include "view/timeline/playregionmodel.h"

#include "view/timeline/gridlines.h"

#include "view/playcursor/playcursorcontroller.h"
#include "view/playcursor/playpositionactioncontroller.h"

#include "view/statusbar/selectionstatusmodel.h"

#include "view/toolbars/playbacktoolbarmodel.h"
#include "view/toolbars/playbacktoolbarcustomisemodel.h"
#include "view/toolbars/playbacktoolbarcustomiseitem.h"

#include "view/historypanel/historypanelmodel.h"

#include "view/trackruler/trackrulermodel.h"

using namespace au::projectscene;
using namespace muse::modularity;
using namespace muse::ui;

static void projectscene_init_qrc()
{
    Q_INIT_RESOURCE(projectscene);
}

std::string ProjectSceneModule::moduleName() const
{
    return "projectscene";
}

void ProjectSceneModule::registerResources()
{
    projectscene_init_qrc();
}

void ProjectSceneModule::registerExports()
{
    m_projectSceneActionsController = std::make_shared<ProjectSceneActionsController>();
    m_uiActions = std::make_shared<ProjectSceneUiActions>(m_projectSceneActionsController);
    m_configuration = std::make_shared<ProjectSceneConfiguration>();
    m_realtimeEffectPanelTrackSelection = std::make_shared<RealtimeEffectPanelTrackSelection>();

    ioc()->registerExport<IProjectSceneConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IProjectViewStateCreator>(moduleName(), new ProjectViewStateCreator());
    ioc()->registerExport<IProjectSceneActionsController>(moduleName(), m_projectSceneActionsController);
    ioc()->registerExport<IRealtimeEffectPanelTrackSelection>(moduleName(), m_realtimeEffectPanelTrackSelection);
    ioc()->registerExport<IWavePainter>(moduleName(), new WavePainterProxy());
    ioc()->registerExport<IConnectingDotsPainter>(moduleName(), new ConnectingDotsPainter());
    ioc()->registerExport<IMinMaxRMSPainter>(moduleName(), new MinMaxRMSPainter());
    ioc()->registerExport<ISamplesPainter>(moduleName(), new SamplesPainter());
}

void ProjectSceneModule::resolveImports()
{
    auto ir = ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://projectscene/editpitchandspeed"),
                           "Audacity/ProjectScene/tracksitemsview/pitchandspeed/PitchAndSpeedChangeDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://projectscene/insertsilence"),
                           "Audacity/ProjectScene/common/InsertSilence.qml");
        ir->registerQmlUri(muse::Uri("audacity://projectscene/openlabeleditor"),
                           "Audacity/ProjectScene/tracksitemsview/labeleditor/LabelEditorDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://projectscene/addnewlabeltrack"),
                           "Audacity/ProjectScene/tracksitemsview/labeleditor/AddNewLabelTrackDialog.qml");
    }
}

void ProjectSceneModule::registerUiTypes()
{
    // types
    qmlRegisterUncreatableType<TrackTypes>("Audacity.ProjectScene", 1, 0, "TrackType", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipKey>("Audacity.ProjectScene", 1, 0, "ClipKey", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipStyles>("Audacity.ProjectScene", 1, 0, "ClipStyle", "Not creatable from QML");
    qmlRegisterUncreatableType<StereoHeightsPref>("Audacity.ProjectScene", 1, 0, "AsymmetricStereoHeights", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipBoundary>("Audacity.ProjectScene", 1, 0, "ClipBoundaryAction", "Not creatable from QML");
    qmlRegisterUncreatableType<DirectionType>("Audacity.ProjectScene", 1, 0, "Direction", "Not creatable from QML");

    // common
    qmlRegisterType<TracksViewStateModel>("Audacity.ProjectScene", 1, 0, "TracksViewStateModel");
    qmlRegisterType<TrackViewStateModel>("Audacity.ProjectScene", 1, 0, "TrackViewStateModel");
    qmlRegisterType<CustomCursor>("Audacity.ProjectScene", 1, 0, "CustomCursor");

    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");
    qmlRegisterType<UndoRedoToolBarModel>("Audacity.ProjectScene", 1, 0, "UndoRedoToolBarModel");
    qmlRegisterType<WorkspacesToolBarModel>("Audacity.ProjectScene", 1, 0, "WorkspacesToolBarModel");
    qmlRegisterType<AudioSetupContextMenuModel>("Audacity.ProjectScene", 1, 0, "AudioSetupContextMenuModel");

    qmlRegisterType<PlaybackToolBarModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarModel");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");

    // tracks panel
    qmlRegisterType<PanelTracksListModel>("Audacity.ProjectScene", 1, 0, "PanelTracksListModel");
    qmlRegisterType<AddEffectMenuModel>("Audacity.ProjectScene", 1, 0, "AddEffectMenuModel");
    qmlRegisterType<RealtimeEffectListModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectListModel");
    qmlRegisterType<RealtimeEffectListItemMenuModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectListItemMenuModel");
    qmlRegisterType<RealtimeEffectSectionModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectSectionModel");
    qmlRegisterType<TrackContextMenuModel>("Audacity.ProjectScene", 1, 0, "TrackContextMenuModel");
    qmlRegisterType<AddNewTrackPopupModel>("Audacity.ProjectScene", 1, 0, "AddNewTrackPopupModel");

    // clips view
    qmlRegisterType<ViewTracksListModel>("Audacity.ProjectScene", 1, 0, "ViewTracksListModel");
    qmlRegisterType<TrackClipsListModel>("Audacity.ProjectScene", 1, 0, "TrackClipsListModel");
    qmlRegisterUncreatableType<TrackClipItem>("Audacity.ProjectScene", 1, 0, "TrackClipItem", "Not creatable from QML");
    qmlRegisterType<MouseHelper>("Audacity.ProjectScene", 1, 0, "MouseHelper");
    qmlRegisterType<TrackLabelsListModel>("Audacity.ProjectScene", 1, 0, "TrackLabelsListModel");
    qmlRegisterType<TrackLabelsLayoutManager>("Audacity.ProjectScene", 1, 0, "TrackLabelsLayoutManager");
    qmlRegisterUncreatableType<TrackLabelItem>("Audacity.ProjectScene", 1, 0, "TrackLabelItem", "Not creatable from QML");
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");
    qmlRegisterType<ClipContextMenuModel>("Audacity.ProjectScene", 1, 0, "ClipContextMenuModel");
    qmlRegisterType<MultiClipContextMenuModel>("Audacity.ProjectScene", 1, 0, "MultiClipContextMenuModel");
    qmlRegisterType<LabelContextMenuModel>("Audacity.ProjectScene", 1, 0, "LabelContextMenuModel");
    qmlRegisterType<SelectionContextMenuModel>("Audacity.ProjectScene", 1, 0, "SelectionContextMenuModel");
    qmlRegisterType<CanvasContextMenuModel>("Audacity.ProjectScene", 1, 0, "CanvasContextMenuModel");
    qmlRegisterType<SelectionViewController>("Audacity.ProjectScene", 1, 0, "SelectionViewController");
    qmlRegisterType<PitchAndSpeedChangeModel>("Audacity.ProjectScene", 1, 0, "PitchAndSpeedChangeModel");
    qmlRegisterType<SplitToolController>("Audacity.ProjectScene", 1, 0, "SplitToolController");
    qmlRegisterType<DropController>("Audacity.ProjectScene", 1, 0, "DropController");
    qmlRegisterType<LabelsTableViewModel>("Audacity.ProjectScene", 1, 0, "LabelsTableViewModel");
    qmlRegisterUncreatableMetaObject(LabelsTableViewCellType::staticMetaObject,
                                     "Audacity.ProjectScene", 1, 0, "LabelsTableViewCellType", "");
    qmlRegisterType<AddNewLabelTrackModel>("Audacity.ProjectScene", 1, 0, "AddNewLabelTrackModel");

    // timeline
    qmlRegisterType<TimelineContext>("Audacity.ProjectScene", 1, 0, "TimelineContext");
    qmlRegisterType<TimelineRuler>("Audacity.ProjectScene", 1, 0, "TimelineRuler");
    qmlRegisterType<TimelineContextMenuModel>("Audacity.ProjectScene", 1, 0, "TimelineContextMenuModel");
    qmlRegisterType<PlayRegionController>("Audacity.ProjectScene", 1, 0, "PlayRegionController");
    qmlRegisterType<PlayRegionModel>("Audacity.ProjectScene", 1, 0, "PlayRegionModel");

    // gridlines
    qmlRegisterType<GridLines>("Audacity.ProjectScene", 1, 0, "GridLines");

    // play cursor
    qmlRegisterType<PlayCursorController>("Audacity.ProjectScene", 1, 0, "PlayCursorController");
    qmlRegisterType<PlayPositionActionController>("Audacity.ProjectScene", 1, 0, "PlayPositionActionController");

    // status bar
    qmlRegisterType<SelectionStatusModel>("Audacity.ProjectScene", 1, 0, "SelectionStatusModel");

    // history panel
    qmlRegisterType<HistoryPanelModel>("Audacity.ProjectScene", 1, 0, "HistoryPanelModel");

    //track ruler
    qmlRegisterType<TrackRulerModel>("Audacity.ProjectScene", 1, 0, "TrackRulerModel");
}

void ProjectSceneModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_configuration->init();
    m_projectSceneActionsController->init();
    m_realtimeEffectPanelTrackSelection->init();

    m_uiActions->init();
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
}
