/*
* Audacity: A Digital Audio Editor
*/
#include "projectpagemodel.h"

#include "framework/global/log.h"
#include "framework/global/async/async.h"

#include "internal/applicationuiactions.h"
#include "projectscene/internal/projectsceneuiactions.h"

using namespace au::appshell;
using namespace muse::actions;

static const ActionQuery PLAYBACK_LEVEL_QUERY("action://playback/level");
static const QString PLAYBACK_METER_POSITION_KEY("playbackToolbar/playbackMeterPosition");
static const QString TIMELINE_RULER_MODE_KEY("projectscene/timelineRulerMode");

ProjectPageModel::ProjectPageModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ProjectPageModel::init()
{
    TRACEFUNC;

    if (!m_inited) {
        for (const ActionCode& actionCode : ApplicationUiActions::toggleDockActions().keys()) {
            DockName dockName = ApplicationUiActions::toggleDockActions()[actionCode];
            dispatcher()->reg(this, actionCode, [=]() { toggleDock(dockName); });
        }

        uiState()->toolConfigChanged(playbackToolBarName()).onNotify(this, [this]() {
            updatePlaybackMeterVisibility();
        });

        playbackConfiguration()->playbackMeterPositionChanged().onNotify(this, [this]() {
            storeMeterPositionToWorkspace();
            updatePlaybackMeterVisibility();
        });

        uiState()->uiItemStateChanged(PLAYBACK_METER_POSITION_KEY).onNotify(this, [this]() {
            applyWorkspaceMeterPosition();
        });

        projectSceneConfiguration()->timelineRulerModeChanged().onNotify(this, [this]() {
            storeRulerModeToWorkspace();
        });

        uiState()->uiItemStateChanged(TIMELINE_RULER_MODE_KEY).onNotify(this, [this]() {
            applyWorkspaceRulerMode();
        });

        m_inited = true;
    }

    applyWorkspaceMeterPosition();
    applyWorkspaceRulerMode();
    updatePlaybackMeterVisibility();
}

void ProjectPageModel::applyWorkspaceMeterPosition()
{
    const QString value = uiState()->uiItemState(PLAYBACK_METER_POSITION_KEY);
    const auto position = value == "1"
                          ? playback::PlaybackMeterPosition::MeterPosition::SideBar
                          : playback::PlaybackMeterPosition::MeterPosition::TopBar;
    if (position != playbackConfiguration()->playbackMeterPosition()) {
        playbackConfiguration()->setPlaybackMeterPosition(position);
    }
}

void ProjectPageModel::storeMeterPositionToWorkspace()
{
    const QString value = QString::number(static_cast<int>(playbackConfiguration()->playbackMeterPosition()));
    if (uiState()->uiItemState(PLAYBACK_METER_POSITION_KEY) != value) {
        uiState()->setUiItemState(PLAYBACK_METER_POSITION_KEY, value);
    }
}

void ProjectPageModel::applyWorkspaceRulerMode()
{
    const QString value = uiState()->uiItemState(TIMELINE_RULER_MODE_KEY);
    const auto mode = value == "1"
                      ? projectscene::TimelineRulerMode::BEATS_AND_MEASURES
                      : projectscene::TimelineRulerMode::MINUTES_AND_SECONDS;
    if (mode != projectSceneConfiguration()->timelineRulerMode()) {
        projectSceneConfiguration()->setTimelineRulerMode(mode);
    }
}

void ProjectPageModel::storeRulerModeToWorkspace()
{
    const QString value = QString::number(static_cast<int>(projectSceneConfiguration()->timelineRulerMode()));
    if (uiState()->uiItemState(TIMELINE_RULER_MODE_KEY) != value) {
        uiState()->setUiItemState(TIMELINE_RULER_MODE_KEY, value);
    }
}

void ProjectPageModel::updatePlaybackMeterVisibility()
{
    const auto toolConfig = uiState()->toolConfig(playbackToolBarName(),
                                                  au::projectscene::ProjectSceneUiActions::defaultPlaybackToolBarConfig());
    const auto it = std::find_if(toolConfig.items.begin(), toolConfig.items.end(),
                                 [](const muse::ui::ToolConfig::Item& item) {
        return item.action == PLAYBACK_LEVEL_QUERY.toString();
    });

    if (it != toolConfig.items.end()) {
        const bool meterPanelVisible = it->show
                                       && (playbackConfiguration()->playbackMeterPosition()
                                           == playback::PlaybackMeterPosition::MeterPosition::SideBar);
        muse::async::Async::call(this, [this, meterPanelVisible]() {
            dispatcher()->dispatch("dock-set-open", ActionData::make_arg2<QString, bool>(playbackMeterPanelName(), meterPanelVisible));
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
    dispatcher()->dispatch("dock-toggle", ActionData::make_arg1<QString>(name));
}
