/*
* Audacity: A Digital Audio Editor
*/
#include "projectscenemodule.h"

#include <QtQml>

#include "types/projectscenetypes.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"
#include "ui/uitypes.h"

#include "internal/projectsceneuiactions.h"
#include "internal/projectsceneactionscontroller.h"
#include "internal/projectsceneconfiguration.h"
#include "internal/projectviewstatecreator.h"
#include "internal/realtimeeffectpaneltrackselection.h"

#include "view/common/tracksviewstatemodel.h"
#include "view/common/customcursor.h"
#include "view/common/insertsilencemodel.h"

#include "view/toolbars/projecttoolbarmodel.h"
#include "view/toolbars/undoredotoolbarmodel.h"

#include "view/trackspanel/addeffectmenumodel.h"
#include "view/trackspanel/realtimeeffectlistmodel.h"
#include "view/trackspanel/realtimeeffectlistitemmenumodel.h"
#include "view/trackspanel/realtimeeffectsectionmodel.h"
#include "view/trackspanel/trackslistmodel.h"
#include "view/trackspanel/trackcontextmenumodel.h"

#include "view/clipsview/trackslistclipsmodel.h"
#include "view/clipsview/clipslistmodel.h"
#include "view/clipsview/cliplistitem.h"
#include "view/clipsview/waveview.h"
#include "view/clipsview/clipcontextmenumodel.h"
#include "view/clipsview/multiclipcontextmenumodel.h"
#include "view/clipsview/canvascontextmenumodel.h"
#include "view/clipsview/selectioncontextmenumodel.h"
#include "view/clipsview/selectionviewcontroller.h"
#include "view/clipsview/pitchandspeedchangemodel.h"
#include "view/clipsview/wavepainterproxy.h"
#include "view/clipsview/au3/connectingdotspainter.h"
#include "view/clipsview/au3/minmaxrmspainter.h"
#include "view/clipsview/au3/samplespainter.h"

#include "view/timeline/timelinecontext.h"
#include "view/timeline/timelineruler.h"
#include "view/timeline/timelinecontextmenumodel.h"

#include "view/timeline/gridlines.h"

#include "view/playcursor/playcursorcontroller.h"
#include "view/playcursor/playpositionactioncontroller.h"

#include "view/statusbar/selectionstatusmodel.h"

#include "view/toolbars/playbacktoolbarmodel.h"
#include "view/toolbars/playbacktoolbarcustomisemodel.h"
#include "view/toolbars/playbacktoolbarcustomiseitem.h"

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
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
    auto ir = ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://projectscene/editpitchandspeed"),
                           "Audacity/ProjectScene/clipsview/pitchandspeed/PitchAndSpeedChangeDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://projectscene/insertsilence"),
                           "Audacity/ProjectScene/common/InsertSilence.qml");
    }
}

void ProjectSceneModule::registerUiTypes()
{
    // types
    qmlRegisterUncreatableType<TrackTypes>("Audacity.ProjectScene", 1, 0, "TrackType", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipKey>("Audacity.ProjectScene", 1, 0, "ClipKey", "Not creatable from QML");

    // common
    qmlRegisterType<TracksViewStateModel>("Audacity.ProjectScene", 1, 0, "TracksViewStateModel");
    qmlRegisterType<CustomCursor>("Audacity.ProjectScene", 1, 0, "CustomCursor");
    qmlRegisterType<InsertSilenceModel>("Audacity.ProjectScene", 1, 0, "InsertSilenceModel");

    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    qmlRegisterType<UndoRedoToolBarModel>("Audacity.ProjectScene", 1, 0, "UndoRedoToolBarModel");
    qmlRegisterType<PlaybackToolBarModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarModel");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");

    // tracks panel
    qmlRegisterType<AddEffectMenuModel>("Audacity.ProjectScene", 1, 0, "AddEffectMenuModel");
    qmlRegisterType<RealtimeEffectListModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectListModel");
    qmlRegisterType<RealtimeEffectListItemMenuModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectListItemMenuModel");
    qmlRegisterType<RealtimeEffectSectionModel>("Audacity.ProjectScene", 1, 0, "RealtimeEffectSectionModel");
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");
    qmlRegisterType<TrackContextMenuModel>("Audacity.ProjectScene", 1, 0, "TrackContextMenuModel");

    // clips view
    qmlRegisterType<TracksListClipsModel>("Audacity.ProjectScene", 1, 0, "TracksListClipsModel");
    qmlRegisterType<ClipsListModel>("Audacity.ProjectScene", 1, 0, "ClipsListModel");
    qmlRegisterUncreatableType<ClipListItem>("Audacity.ProjectScene", 1, 0, "ClipListItem", "Not creatable from QML");
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");
    qmlRegisterType<ClipContextMenuModel>("Audacity.ProjectScene", 1, 0, "ClipContextMenuModel");
    qmlRegisterType<MultiClipContextMenuModel>("Audacity.ProjectScene", 1, 0, "MultiClipContextMenuModel");
    qmlRegisterType<SelectionContextMenuModel>("Audacity.ProjectScene", 1, 0, "SelectionContextMenuModel");
    qmlRegisterType<CanvasContextMenuModel>("Audacity.ProjectScene", 1, 0, "CanvasContextMenuModel");
    qmlRegisterType<SelectionViewController>("Audacity.ProjectScene", 1, 0, "SelectionViewController");
    qmlRegisterType<PitchAndSpeedChangeModel>("Audacity.ProjectScene", 1, 0, "PitchAndSpeedChangeModel");

    // timeline
    qmlRegisterType<TimelineContext>("Audacity.ProjectScene", 1, 0, "TimelineContext");
    qmlRegisterType<TimelineRuler>("Audacity.ProjectScene", 1, 0, "TimelineRuler");
    qmlRegisterType<TimelineContextMenuModel>("Audacity.ProjectScene", 1, 0, "TimelineContextMenuModel");

    // gridlines
    qmlRegisterType<GridLines>("Audacity.ProjectScene", 1, 0, "GridLines");

    // play cursor
    qmlRegisterType<PlayCursorController>("Audacity.ProjectScene", 1, 0, "PlayCursorController");
    qmlRegisterType<PlayPositionActionController>("Audacity.ProjectScene", 1, 0, "PlayPositionActionController");

    // status bar
    qmlRegisterType<SelectionStatusModel>("Audacity.ProjectScene", 1, 0, "SelectionStatusModel");
}

void ProjectSceneModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_configuration->init();
    m_uiActions->init();
    m_projectSceneActionsController->init();
    m_realtimeEffectPanelTrackSelection->init();

    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    ar->reg(m_uiActions);
}
