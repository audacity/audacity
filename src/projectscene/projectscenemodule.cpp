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

#include "view/common/tracksviewstatemodel.h"
#include "view/common/customcursor.h"

#include "view/toolbars/projecttoolbarmodel.h"
#include "view/toolbars/undoredotoolbarmodel.h"

#include "view/trackspanel/trackslistmodel.h"
#include "view/trackspanel/trackcontextmenumodel.h"

#include "view/clipsview/trackslistclipsmodel.h"
#include "view/clipsview/clipslistmodel.h"
#include "view/clipsview/cliplistitem.h"
#include "view/clipsview/waveview.h"
#include "view/clipsview/au3/au3wavepainter.h"
#include "view/clipsview/clipcontextmenumodel.h"
#include "view/clipsview/selectionviewcontroller.h"
#include "view/clipsview/pitchandspeedchangemodel.h"

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

    ioc()->registerExport<IProjectSceneConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IProjectViewStateCreator>(moduleName(), new ProjectViewStateCreator());
    ioc()->registerExport<IProjectSceneActionsController>(moduleName(), m_projectSceneActionsController);
    ioc()->registerExport<IWavePainter>(moduleName(), new Au3WavePainter());
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

    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    qmlRegisterType<UndoRedoToolBarModel>("Audacity.ProjectScene", 1, 0, "UndoRedoToolBarModel");
    qmlRegisterType<PlaybackToolBarModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarModel");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");

    // tracks panel
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");
    qmlRegisterType<TrackContextMenuModel>("Audacity.ProjectScene", 1, 0, "TrackContextMenuModel");

    // clips view
    qmlRegisterType<TracksListClipsModel>("Audacity.ProjectScene", 1, 0, "TracksListClipsModel");
    qmlRegisterType<ClipsListModel>("Audacity.ProjectScene", 1, 0, "ClipsListModel");
    qmlRegisterUncreatableType<ClipListItem>("Audacity.ProjectScene", 1, 0, "ClipListItem", "Not creatable from QML");
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");
    qmlRegisterType<ClipContextMenuModel>("Audacity.ProjectScene", 1, 0, "ClipContextMenuModel");
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
}
