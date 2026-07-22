/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "framework/global/containers.h"

#include "internal/projectsceneuiactions.h"

#include "playback/view/toolbars/playbacktoolbarlevelitem.h"
#include "playback/view/toolbars/playbacktoolbarcontrolitem.h"
#include "playback/view/toolbars/playbacktoolbartimeitem.h"
#include "playback/view/toolbars/playbacktoolbarbpmitem.h"
#include "playback/view/toolbars/playbacktoolbartimesignatureitem.h"
#include "record/view/toolbars/playbacktoolbarrecordlevelitem.h"
#include "projectscene/view/toolbars/snaptoolbaritem.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::actions;
using namespace au::projectscene;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static const ActionQuery PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY("action://playback/toggle-play-pause");
static const ActionQuery PLAYBACK_STOP_QUERY("action://playback/stop");

static const ActionQuery RECORD_START_QUERY("action://record/start");
static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");
static const ActionQuery RECORD_LEVEL_QUERY("action://record/level");

static const ActionQuery PLAYBACK_REWIND_START_QUERY("action://playback/rewind-start");
static const ActionQuery PLAYBACK_REWIND_END_QUERY("action://playback/rewind-end");
static const ActionCode LOOP_ACTION_CODE("toggle-loop-region");

static const ActionCode CLIP_GAIN_AUTOMATION_CODE("clip-gain");
static const ActionCode SPLIT_TOOL_ACTION_CODE("split-tool");

static const ActionCode TOGGLE_GLOBAL_SPECTROGRAM_VIEW_ACTION_CODE("action://trackedit/global-view-spectrogram");

static const ActionQuery PLAYBACK_LEVEL_QUERY("action://playback/level");
static const ActionCode PLAYBACK_TIME("playback-time");
static const ActionCode PLAYBACK_BPM("playback-bpm");
static const ActionCode PLAYBACK_TIME_SIGNATURE("playback-time-signature");

static const ActionCode SNAP_ACTION_CODE("snap");

static PlaybackToolBarModel::ItemType itemType(const ActionCode& actionCode)
{
    std::map<ActionCode, PlaybackToolBarModel::ItemType> types = {
        { PLAYBACK_LEVEL_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_LEVEL },
        { PLAYBACK_TIME, PlaybackToolBarModel::PLAYBACK_TIME },
        { PLAYBACK_BPM, PlaybackToolBarModel::PLAYBACK_BPM },
        { PLAYBACK_TIME_SIGNATURE, PlaybackToolBarModel::PLAYBACK_TIME_SIGNATURE },
        { RECORD_LEVEL_QUERY.toString(), PlaybackToolBarModel::RECORD_LEVEL },
        { PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_STOP_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { RECORD_START_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_START_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_END_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { LOOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { CLIP_GAIN_AUTOMATION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { SPLIT_TOOL_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { TOGGLE_GLOBAL_SPECTROGRAM_VIEW_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { SNAP_ACTION_CODE, PlaybackToolBarModel::SNAP }
    };

    return muse::value(types, actionCode, PlaybackToolBarModel::PROJECT_CONTROL);
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

        configuration()->playbackMeterPositionChanged().onNotify(this, [this]() {
            reload();
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

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (uicontextResolver()->currentUiContext() == context::UiCtxDialogOpened) {
        return;
    }

    if (containsAction(codes, PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY.toString())
        || containsAction(codes, RECORD_PAUSE_QUERY.toString())) {
        updatePlayState();
    }

    if (containsAction(codes, PLAYBACK_STOP_QUERY.toString()) || containsAction(codes, RECORD_STOP_QUERY.toString())) {
        updateStopState();
    }

    if (containsAction(codes, RECORD_START_QUERY.toString())) {
        updateRecordState();
    }

    if (containsAction(codes, LOOP_ACTION_CODE)) {
        updateLoopState();
    }

    AbstractToolBarModel::onActionsStateChanges(codes);
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
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY.toString()));

    if (item == nullptr) {
        return;
    }

    bool isPlaying = playbackController()->isPlaying();
    bool isRecording = recordController()->isRecording();
    bool isLeadIn = recordController()->isLeadInRecording();

    //! NOTE: the action never changes; PlaybackController decides whether it pauses
    //! the player or the recorder. Only the icon reflects the current state.
    UiAction action = uiActionsRegister()->action(PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY.toString());

    // During lead-in, show as playing (green background) since audio is playing back
    bool showAsPlaying = isPlaying || isLeadIn;
    if (showAsPlaying || (isRecording && !isLeadIn)) {
        action.iconCode = IconCode::Code::PAUSE_FILL;
    }
    item->setAction(action);
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
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAYBACK_STOP_QUERY.toString()));

    if (item == nullptr) {
        return;
    }

    //! NOTE: the action never changes; PlaybackController decides whether it stops
    //! the player or the recorder.
    const UiAction action = uiActionsRegister()->action(PLAYBACK_STOP_QUERY.toString());
    item->setAction(action);
    const QColor iconColor = themeColor(FONT_PRIMARY_COLOR);
    item->setIconColor(iconColor);
}

void PlaybackToolBarModel::updateRecordState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(RECORD_START_QUERY.toString()));

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
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(LOOP_ACTION_CODE));

    if (item == nullptr) {
        return;
    }

    bool isLooping = playbackController()->isLoopRegionActive();
    item->setSelected(isLooping);

    item->setIconColor(themeColor(FONT_PRIMARY_COLOR));
    item->setBackgroundColor(themeColor(isLooping ? ACCENT_COLOR : BUTTON_COLOR));
}

void PlaybackToolBarModel::updateToggleState(const ActionCode& actionCode, bool isOn)
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(actionCode));

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

    updateToggleState(CLIP_GAIN_AUTOMATION_CODE, prj->viewState()->clipGainAutomationEnabled().val);
}

void PlaybackToolBarModel::updateSplitState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    updateToggleState(SPLIT_TOOL_ACTION_CODE, prj->viewState()->splitToolEnabled().val);
}

void PlaybackToolBarModel::updateGlobalSpectrogramViewState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    updateToggleState(TOGGLE_GLOBAL_SPECTROGRAM_VIEW_ACTION_CODE, prj->viewState()->globalSpectrogramToggleIsOn());
}

QColor PlaybackToolBarModel::themeColor(muse::ui::ThemeStyleKey key) const
{
    return uiConfiguration()->currentTheme().values.value(key).value<QColor>();
}

void PlaybackToolBarModel::updateActions()
{
    ToolBarItemList items;

    muse::ui::ToolConfig playbackConfig = uiState()->toolConfig(TOOLBAR_NAME,
                                                                ProjectSceneUiActions::defaultPlaybackToolBarConfig());

    for (const muse::ui::ToolConfig::Item& citem : playbackConfig.items) {
        if (!citem.show) {
            continue;
        }

        if (citem.action == PLAYBACK_LEVEL_QUERY.toString()) {
            if (configuration()->playbackMeterPosition() == playback::PlaybackMeterPosition::MeterPosition::SideBar) {
                // Skip playback meter item if it is set to be displayed in the sidebar
                continue;
            }
        }

        if (citem.action == AbstractToolBarModel::SEPARATOR_ID) {
            items << AbstractToolBarModel::makeSeparator();
            continue;
        }

        ToolBarItem* item = makeLocalItem(citem.action);
        if (!item) {
            continue;
        }

        item->setIsTransparent(false);

        items << item;
    }

    setItems(items);
}

ToolBarItem* PlaybackToolBarModel::makeLocalItem(const ActionCode& actionCode)
{
    PlaybackToolBarModel::ItemType type = itemType(actionCode);

    if (type == PlaybackToolBarModel::PROJECT_CONTROL) {
        ToolBarItem* item = AbstractToolBarModel::makeItem(actionCode);
        item->setType(static_cast<ToolBarItemType::Type>(type));
        return item;
    }

    ToolBarItem* result = nullptr;
    const UiAction& action = uiActionsRegister()->action(actionCode);

    switch (type) {
    case PlaybackToolBarModel::PLAYBACK_LEVEL:
        result = new PlaybackToolBarLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_TIME:
        result = new PlaybackToolBarTimeItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_BPM:
        result = new PlaybackToolBarBPMItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_TIME_SIGNATURE:
        result = new PlaybackToolBarTimeSignatureItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::RECORD_LEVEL:
        result = new record::PlaybackToolBarRecordLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    case PlaybackToolBarModel::PLAYBACK_CONTROL: {
        PlaybackToolBarControlItem* item = new PlaybackToolBarControlItem(action, static_cast<ToolBarItemType::Type>(type), this);
        item->setIconColor(themeColor(FONT_PRIMARY_COLOR));
        item->setBackgroundColor(themeColor(BUTTON_COLOR));
        result = std::move(item);
        break;
    }
    case PlaybackToolBarModel::SNAP:
        result = new projectscene::SnapToolBarItem(action, static_cast<ToolBarItemType::Type>(type), this);
        break;
    default:
        break;
    }

    if (result) {
        result->setState(uiActionsRegister()->actionState(actionCode));
    }

    return result;
}

bool PlaybackToolBarModel::isEnabled() const
{
    return context()->currentProject() != nullptr;
}
