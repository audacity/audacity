/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "playback/view/toolbars/playbacktoolbarlevelitem.h"
#include "playback/view/toolbars/playbacktoolbarcontrolitem.h"
#include "playback/view/toolbars/playbacktoolbartimeitem.h"
#include "playback/view/toolbars/playbacktoolbarbpmitem.h"
#include "playback/view/toolbars/playbacktoolbartimesignatureitem.h"
#include "record/qml/Audacity/Record/playbacktoolbarrecordlevelitem.h"
#include "projectscene/view/toolbars/snaptoolbaritem.h"

#include "playbacktoolbarconfig.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::rcommand;
using namespace au::projectscene;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static PlaybackToolBarModel::ItemType itemType(const Command& command)
{
    static const std::map<Command, PlaybackToolBarModel::ItemType> types = {
        { PLAYBACK_LEVEL_COMMAND, PlaybackToolBarModel::PLAYBACK_LEVEL },
        { PLAYBACK_TIME_COMMAND, PlaybackToolBarModel::PLAYBACK_TIME },
        { PLAYBACK_BPM_COMMAND, PlaybackToolBarModel::PLAYBACK_BPM },
        { PLAYBACK_TIME_SIGNATURE_COMMAND, PlaybackToolBarModel::PLAYBACK_TIME_SIGNATURE },
        { au::record::RECORD_LEVEL_COMMAND, PlaybackToolBarModel::RECORD_LEVEL },
        { PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_STOP_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { au::record::RECORD_START_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_START_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_END_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_TOGGLE_LOOP_REGION_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PROJECTSCENE_TOGGLE_SPLIT_TOOL_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { au::trackedit::TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PROJECTSCENE_SNAP_COMMAND, PlaybackToolBarModel::SNAP }
    };

    return muse::value(types, command, PlaybackToolBarModel::PROJECT_CONTROL);
}

PlaybackToolBarModel::PlaybackToolBarModel(QObject* parent)
    : AbstractToolBarModel(parent)
{
}

void PlaybackToolBarModel::load()
{
    if (!m_inited) {
        uiState()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
            reload();
        });

        uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
            reload();
        });

        context()->currentProjectChanged().onNotify(this, [this]() {
            reload();
            emit isEnabledChanged();
            if (context()->currentProject()) {
                setupProjectConnections(*context()->currentProject());
            }
        });
        if (context()->currentProject()) {
            setupProjectConnections(*context()->currentProject());
        }

        playbackUiState()->playbackMeterPositionChanged().onNotify(this, [this]() {
            reload();
        });

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            updatePlayState();
        });

        playbackController()->loopRegionChanged().onNotify(this, [this]() {
            updateLoopState();
        });

        recordController()->isRecordingChanged().onNotify(this, [this]() {
            updatePlayState();
            updateRecordState();
        });

        m_inited = true;
    }

    updateActions();

    AbstractToolBarModel::load();
}

void PlaybackToolBarModel::reload()
{
    load();
    updateStates();
}

void PlaybackToolBarModel::setupProjectConnections(project::IAudacityProject& project)
{
    const auto vs = project.viewState();
    vs->clipGainAutomationEnabled().ch.onReceive(this, [this](bool){ updateClipGainAutomationState(); });
    vs->splitToolEnabled().ch.onReceive(this, [this](bool){ updateSplitState(); });
    vs->globalSpectrogramToggleIsOnChanged().onNotify(this, [this] { updateGlobalSpectrogramViewState(); });
}

void PlaybackToolBarModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    if (uicontextResolver()->currentUiContext() == context::UiCtxDialogOpened) {
        return;
    }

    AbstractToolBarModel::onCommandStateChanged(command, state);
}

void PlaybackToolBarModel::updateStates()
{
    updatePlayState();
    updateStopState();
    updateRecordState();
    updateLoopState();
    updateClipGainAutomationState();
    updateSplitState();
    updateGlobalSpectrogramViewState();
}

void PlaybackToolBarModel::updatePlayState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND));

    if (item == nullptr) {
        return;
    }

    bool isPlaying = playbackController()->isPlaying();
    bool isPaused = playbackController()->isPaused();
    bool isRecording = recordController()->isRecording();
    bool isLeadIn = recordController()->isLeadInRecording();

    // During recording it pauses the recorder
    CommandInfo info = commandsRegister()->commandInfo(PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND);

    // During lead-in, show as playing (green background) since audio is playing back
    bool showAsPlaying = isPlaying || isLeadIn;

    bool leadInPaused = isLeadIn && isPaused;
    if ((showAsPlaying && !leadInPaused) || (isRecording && !isLeadIn)) {
        info.decoration.iconCode = IconCode::Code::PAUSE_FILL;
    }
    item->setCommandInfo(info);
    item->setSelected(showAsPlaying);

    QColor iconColor = themeColor(PLAY_COLOR);
    QColor backgroundColor = themeColor(BUTTON_COLOR);
    if (showAsPlaying) {
        iconColor = themeColor(FONT_PRIMARY_COLOR);
        backgroundColor = themeColor(PLAY_COLOR);
    } else if (isRecording) {
        iconColor = themeColor(FONT_PRIMARY_COLOR);
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateStopState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAYBACK_STOP_COMMAND));

    if (item == nullptr) {
        return;
    }

    item->setCommandInfo(commandsRegister()->commandInfo(PLAYBACK_STOP_COMMAND));
    const QColor iconColor = themeColor(FONT_PRIMARY_COLOR);
    item->setIconColor(iconColor);
}

void PlaybackToolBarModel::updateRecordState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(record::RECORD_START_COMMAND));

    if (item == nullptr) {
        return;
    }

    bool isRecording = recordController()->isRecording();
    bool isLeadIn = recordController()->isLeadInRecording();

    // During lead-in pre-roll, the button should not appear as actively recording
    item->setSelected(isRecording && !isLeadIn);

    QColor iconColor = themeColor(RECORD_COLOR);
    QColor backgroundColor = themeColor(BUTTON_COLOR);
    if (isRecording && !isLeadIn) {
        iconColor = themeColor(BACKGROUND_PRIMARY_COLOR);
        backgroundColor = themeColor(RECORD_COLOR);
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateLoopState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAYBACK_TOGGLE_LOOP_REGION_COMMAND));

    if (item == nullptr) {
        return;
    }

    bool isLooping = playbackController()->isLoopRegionActive();
    item->setSelected(isLooping);

    item->setIconColor(themeColor(FONT_PRIMARY_COLOR));
    item->setBackgroundColor(themeColor(isLooping ? ACCENT_COLOR : BUTTON_COLOR));
}

void PlaybackToolBarModel::updateToggleState(const Command& command, bool isOn)
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(command));

    if (item == nullptr) {
        return;
    }

    item->setSelected(isOn);
    item->setBackgroundColor(themeColor(isOn ? ACCENT_COLOR : BUTTON_COLOR));
}

void PlaybackToolBarModel::updateClipGainAutomationState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    updateToggleState(PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND, prj->viewState()->clipGainAutomationEnabled().val);
}

void PlaybackToolBarModel::updateSplitState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    updateToggleState(PROJECTSCENE_TOGGLE_SPLIT_TOOL_COMMAND, prj->viewState()->splitToolEnabled().val);
}

void PlaybackToolBarModel::updateGlobalSpectrogramViewState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    updateToggleState(trackedit::TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND, prj->viewState()->globalSpectrogramToggleIsOn());
}

QColor PlaybackToolBarModel::themeColor(muse::ui::ThemeStyleKey key) const
{
    return uiConfiguration()->currentTheme().values.value(key).value<QColor>();
}

void PlaybackToolBarModel::updateActions()
{
    ToolBarItemList items;

    muse::ui::ToolConfig playbackConfig = uiState()->toolConfig(TOOLBAR_NAME, defaultPlaybackToolBarConfig());

    for (const muse::ui::ToolConfig::Item& citem : playbackConfig.items) {
        if (!citem.show) {
            continue;
        }

        if (citem.intent == PLAYBACK_LEVEL_COMMAND.toString()) {
            if (playbackUiState()->playbackMeterPosition() == playback::PlaybackMeterPosition::MeterPosition::SideBar) {
                // Skip playback meter item if it is set to be displayed in the sidebar
                continue;
            }
        }

        if (citem.isSeparator()) {
            items << AbstractToolBarModel::makeSeparator();
            continue;
        }

        ToolBarItem* item = makeLocalItem(Command(citem.intent));
        if (!item) {
            continue;
        }

        item->setIsTransparent(false);

        items << item;
    }

    setItems(items);
}

ToolBarItem* PlaybackToolBarModel::makeLocalItem(const Command& command)
{
    PlaybackToolBarModel::ItemType type = itemType(command);

    if (type == PlaybackToolBarModel::PROJECT_CONTROL) {
        ToolBarItem* item = AbstractToolBarModel::makeItem(command);
        if (item) {
            item->setType(static_cast<ToolBarItemType::Type>(type));
        }
        return item;
    }

    const CommandInfo& info = commandsRegister()->commandInfo(command);
    if (!info.isValid()) {
        LOGW() << "not found command: " << command;
        return nullptr;
    }

    ToolBarItem* result = nullptr;

    switch (type) {
    case PlaybackToolBarModel::PLAYBACK_LEVEL:
        result = new PlaybackToolBarLevelItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_TIME:
        result = new PlaybackToolBarTimeItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_BPM:
        result = new PlaybackToolBarBPMItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_TIME_SIGNATURE:
        result = new PlaybackToolBarTimeSignatureItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::RECORD_LEVEL:
        result = new record::PlaybackToolBarRecordLevelItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_CONTROL: {
        PlaybackToolBarControlItem* item = new PlaybackToolBarControlItem(info, static_cast<ToolBarItemType::Type>(type), this);
        item->setIconColor(themeColor(FONT_PRIMARY_COLOR));
        item->setBackgroundColor(themeColor(BUTTON_COLOR));
        result = std::move(item);
        break;
    }
    case PlaybackToolBarModel::SNAP:
        result = new projectscene::SnapToolBarItem(info, static_cast<ToolBarItemType::Type>(type), this);
        break;
    default:
        break;
    }

    if (result) {
        result->setCommandState(commandsState()->commandState(command));
    }

    return result;
}

bool PlaybackToolBarModel::isEnabled() const
{
    return context()->currentProject() != nullptr;
}
