/*
* Audacity: A Digital Audio Editor
*/
#include "projectpagemodel.h"

#include "framework/global/log.h"
#include "framework/global/async/async.h"
#include "framework/dockwindow/dockcommands.h"

#include "projectscene/internal/projectsceneuiactions.h"

using namespace au::appshell;
using namespace muse;

static const muse::actions::ActionQuery PLAYBACK_LEVEL_QUERY("action://playback/level");

ProjectPageModel::ProjectPageModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ProjectPageModel::init()
{
    TRACEFUNC;

    if (!m_inited) {
        applicationActionController()->dockToggleRequested().onReceive(this, [this](const DockName& dockName) {
            toggleDock(dockName);
        });

        uiState()->toolConfigChanged(playbackToolBarName()).onNotify(this, [this]() {
            updatePlaybackMeterVisibility();
        });

        playbackUiState()->playbackMeterPositionChanged().onNotify(this, [this]() {
            updatePlaybackMeterVisibility();
        });

        m_inited = true;
    }

    updatePlaybackMeterVisibility();
}

void ProjectPageModel::updatePlaybackMeterVisibility()
{
    const auto toolConfig = uiState()->toolConfig(playbackToolBarName(),
                                                  au::projectscene::ProjectSceneUiActions::defaultPlaybackToolBarConfig());
    const auto it = std::find_if(toolConfig.items.begin(), toolConfig.items.end(),
                                 [](const muse::ui::ToolConfig::Item& item) {
        return item.intent == PLAYBACK_LEVEL_QUERY.toString();
    });

    if (it != toolConfig.items.end()) {
        const bool meterPanelVisible = it->show
                                       && (playbackUiState()->playbackMeterPosition()
                                           == playback::PlaybackMeterPosition::MeterPosition::SideBar);
        muse::async::Async::call(this, [this, meterPanelVisible]() {
            commandDispatcher()->dispatch(dock::DOCK_SET_OPEN_COMMAND, {
                { "dock_name", Val(playbackMeterPanelName().toStdString()) },
                { "open", Val(meterPanelVisible) },
            });
        });
    }
}

QString ProjectPageModel::projectToolBarName() const
{
    return PROJECT_TOOLBAR_NAME;
}

QString ProjectPageModel::playbackToolBarName() const
{
    return PLAYBACK_TOOLBAR_NAME;
}

QString ProjectPageModel::undoRedoToolBarName() const
{
    return UNDO_REDO_TOOLBAR_NAME;
}

QString ProjectPageModel::workspacesToolBarName() const
{
    return WORKSPACES_TOOLBAR_NAME;
}

QString ProjectPageModel::tracksPanelName() const
{
    return TRACKS_PANEL_NAME;
}

QString ProjectPageModel::historyPanelName() const
{
    return HISTORY_PANEL_NAME;
}

QString ProjectPageModel::playbackMeterPanelName() const
{
    return PLAYBACK_METER_PANEL_NAME;
}

QString ProjectPageModel::statusBarName() const
{
    return PROJECT_STATUSBAR_NAME;
}

void ProjectPageModel::toggleDock(const QString& name)
{
    commandDispatcher()->dispatch(dock::DOCK_TOGGLE_COMMAND, { { "dock_name", Val(name.toStdString()) } });
}
