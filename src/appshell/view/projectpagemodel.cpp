/*
* Audacity: A Digital Audio Editor
*/
#include "projectpagemodel.h"

#include "internal/applicationuiactions.h"
#include "dockwindow/idockwindow.h"
#include "projectscene/internal/projectsceneuiactions.h"

#include "log.h"

using namespace au::appshell;
using namespace muse::actions;

static const muse::actions::ActionCode PLAYBACK_LEVEL_CODE("playback-level");

ProjectPageModel::ProjectPageModel(QObject* parent)
    : QObject(parent)
{
}

bool ProjectPageModel::isNavigatorVisible() const
{
    return configuration()->isNotationNavigatorVisible();
}

bool ProjectPageModel::isBraillePanelVisible() const
{
    return false;
    //return brailleConfiguration()->braillePanelEnabled();
}

void ProjectPageModel::init()
{
    TRACEFUNC;

    for (const ActionCode& actionCode : ApplicationUiActions::toggleDockActions().keys()) {
        DockName dockName = ApplicationUiActions::toggleDockActions()[actionCode];
        dispatcher()->reg(this, actionCode, [=]() { toggleDock(dockName); });
    }

    uiConfiguration()->toolConfigChanged(playbackToolBarName()).onNotify(this, [this]() {
        updatePlaybackMeterVisibility();
    });

    playbackConfiguration()->playbackMeterPositionChanged().onNotify(this, [this]() {
        updatePlaybackMeterVisibility();
    });

    // globalContext()->currentNotationChanged().onNotify(this, [this]() {
    //     onNotationChanged();
    // });

    // brailleConfiguration()->braillePanelEnabledChanged().onNotify(this, [this]() {
    //     emit isBraillePanelVisibleChanged();
    // });

    onNotationChanged();
    updatePlaybackMeterVisibility();
    updateDrumsetPanelVisibility();
}

void ProjectPageModel::updatePlaybackMeterVisibility()
{
    const auto toolConfig = uiConfiguration()->toolConfig(playbackToolBarName(),
                                                          au::projectscene::ProjectSceneUiActions::defaultPlaybackToolBarConfig());
    const auto it = std::find_if(toolConfig.items.begin(), toolConfig.items.end(),
                                 [](const muse::ui::ToolConfig::Item& item) {
        return item.action == PLAYBACK_LEVEL_CODE;
    });

    if (it != toolConfig.items.end()) {
        const bool meterPanelVisible = it->show
                                       && (playbackConfiguration()->playbackMeterPosition()
                                           == playback::PlaybackMeterPosition::MeterPosition::SideBar);
        dispatcher()->dispatch("dock-set-open", ActionData::make_arg2<QString, bool>(playbackMeterPanelName(), meterPanelVisible));
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

QString ProjectPageModel::noteInputBarName() const
{
    return NOTE_INPUT_BAR_NAME;
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

QString ProjectPageModel::instrumentsPanelName() const
{
    return INSTRUMENTS_PANEL_NAME;
}

QString ProjectPageModel::inspectorPanelName() const
{
    return INSPECTOR_PANEL_NAME;
}

QString ProjectPageModel::selectionFiltersPanelName() const
{
    return SELECTION_FILTERS_PANEL_NAME;
}

QString ProjectPageModel::mixerPanelName() const
{
    return MIXER_PANEL_NAME;
}

QString ProjectPageModel::pianoKeyboardPanelName() const
{
    return PIANO_KEYBOARD_PANEL_NAME;
}

QString ProjectPageModel::timelinePanelName() const
{
    return TIMELINE_PANEL_NAME;
}

QString ProjectPageModel::drumsetPanelName() const
{
    return DRUMSET_PANEL_NAME;
}

QString ProjectPageModel::statusBarName() const
{
    return PROJECT_STATUSBAR_NAME;
}

void ProjectPageModel::onNotationChanged()
{
    // INotationPtr notation = globalContext()->currentNotation();
    // if (!notation) {
    //     return;
    // }

    // INotationNoteInputPtr noteInput = notation->interaction()->noteInput();
    // noteInput->stateChanged().onNotify(this, [this]() {
    //     updateDrumsetPanelVisibility();
    // });
}

void ProjectPageModel::toggleDock(const QString& name)
{
    if (name == NOTATION_NAVIGATOR_PANEL_NAME) {
        configuration()->setIsNotationNavigatorVisible(!isNavigatorVisible());
        emit isNavigatorVisibleChanged();
        return;
    }

    // if (name == NOTATION_BRAILLE_PANEL_NAME) {
    //     brailleConfiguration()->setBraillePanelEnabled(!isBraillePanelVisible());
    //     emit isBraillePanelVisibleChanged();
    //     return;
    // }

    dispatcher()->dispatch("dock-toggle", ActionData::make_arg1<QString>(name));
}

void ProjectPageModel::updateDrumsetPanelVisibility()
{
    TRACEFUNC;

    // const dock::IDockWindow* window = dockWindowProvider()->window();
    // if (!window) {
    //     return;
    // }

    // auto setDrumsetPanelOpen = [this, window](bool open) {
    //     if (open == window->isDockOpen(DRUMSET_PANEL_NAME)) {
    //         return;
    //     }

    //     dispatcher()->dispatch("dock-set-open", ActionData::make_arg2<QString, bool>(DRUMSET_PANEL_NAME, open));
    // };

    // const INotationPtr notation = globalContext()->currentNotation();
    // if (!notation) {
    //     setDrumsetPanelOpen(false);
    //     return;
    // }

    // const INotationNoteInputPtr noteInput = notation->interaction()->noteInput();
    // bool isNeedOpen = noteInput->isNoteInputMode() && noteInput->state().drumset != nullptr;

    // setDrumsetPanelOpen(isNeedOpen);
}
