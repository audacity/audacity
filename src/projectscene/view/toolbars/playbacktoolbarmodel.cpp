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

static const QString PLAY_PAUSE_ITEM_ID("play-pause-id");
static const QString STOP_ITEM_ID("stop-id");

static const ActionQuery PLAYBACK_PLAY_QUERY("action://playback/play");
static const ActionQuery PLAYBACK_PAUSE_QUERY("action://playback/pause");
static const ActionQuery PLAYBACK_STOP_QUERY("action://playback/stop");

static const ActionQuery RECORD_START_QUERY("action://record/start");
static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");
static const ActionQuery RECORD_LEVEL_QUERY("action://record/level");

static const ActionQuery PLAYBACK_REWIND_START_QUERY("action://playback/rewind-start");
static const ActionQuery PLAYBACK_REWIND_END_QUERY("action://playback/rewind-end");
static const ActionCode LOOP_ACTION_CODE("toggle-loop-region");

static const ActionCode AUTOMATION_CODE("automation");
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
        { PLAYBACK_PLAY_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_STOP_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { RECORD_START_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_START_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { PLAYBACK_REWIND_END_QUERY.toString(), PlaybackToolBarModel::PLAYBACK_CONTROL },
        { LOOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { AUTOMATION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
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
        uiConfiguration()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
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
    vs->automationEnabled().ch.onReceive(this, [this](bool){ updateAutomationState(); });
    vs->splitToolEnabled().ch.onReceive(this, [this](bool){ updateSplitState(); });
    vs->globalSpectrogramViewToggleChanged().onNotify(this, [this] { updateGlobalSpectrogramViewState(); });
}

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (containsAction(codes, PLAYBACK_PLAY_QUERY.toString()) || containsAction(codes, PLAYBACK_PAUSE_QUERY.toString())
        || containsAction(codes, RECORD_PAUSE_QUERY.toString())) {
        updatePlayState();
    }

    if (containsAction(codes, PLAYBACK_STOP_QUERY.toString()) || containsAction(codes, RECORD_STOP_QUERY.toString())) {
        updateStopState();
    }

    if (containsAction(codes, RECORD_START_QUERY.toString())) {
        updateRecordState();
    }

    if (containsAction(codes, "toggle-loop-region")) {
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
    updateAutomationState();
    updateSplitState();
    updateGlobalSpectrogramViewState();
}

void PlaybackToolBarModel::updatePlayState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAY_PAUSE_ITEM_ID));

    if (item == nullptr) {
        return;
    }

    bool isPlaying = playbackController()->isPlaying();
    bool isRecording = recordController()->isRecording();

    ActionCode code = isPlaying ? PLAYBACK_PAUSE_QUERY.toString() : PLAYBACK_PLAY_QUERY.toString();
    if (isRecording) {
        code = RECORD_PAUSE_QUERY.toString();
    }

    UiAction action = uiActionsRegister()->action(code);
    item->setAction(action);

    item->setSelected(isPlaying);

    QColor iconColor = uiConfiguration()->currentTheme().values.value(muse::ui::PLAY_COLOR).value<QColor>();
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isPlaying) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
        backgroundColor = uiConfiguration()->currentTheme().values.value(muse::ui::PLAY_COLOR).value<QColor>();
    } else if (isRecording) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateStopState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(STOP_ITEM_ID));

    if (item == nullptr) {
        return;
    }

    const bool isRecording = recordController()->isRecording();
    const ActionCode code = isRecording ? RECORD_STOP_QUERY.toString() : PLAYBACK_STOP_QUERY.toString();
    const UiAction action = uiActionsRegister()->action(code);
    item->setAction(action);
    const QColor iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    item->setIconColor(iconColor);
}

void PlaybackToolBarModel::updateRecordState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(RECORD_START_QUERY.toString()));

    if (item == nullptr) {
        return;
    }

    bool isRecording = recordController()->isRecording();

    item->setSelected(isRecording);

    QColor iconColor = uiConfiguration()->currentTheme().values.value(muse::ui::RECORD_COLOR).value<QColor>();
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isRecording) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_PRIMARY_COLOR).toString());
        backgroundColor = uiConfiguration()->currentTheme().values.value(muse::ui::RECORD_COLOR).value<QColor>();
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

    QColor iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isLooping) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
        backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).toString());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateAutomationState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(AUTOMATION_CODE));

    if (item == nullptr) {
        return;
    }

    auto vs = prj->viewState();

    bool automationEnabled = vs->automationEnabled().val;
    item->setSelected(automationEnabled);

    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (automationEnabled) {
        backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).toString());
    }

    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateSplitState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(SPLIT_TOOL_ACTION_CODE));

    if (item == nullptr) {
        return;
    }

    auto vs = prj->viewState();

    bool splitToolEnabled = vs->splitToolEnabled().val;
    item->setSelected(splitToolEnabled);

    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (splitToolEnabled) {
        backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).toString());
    }

    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateGlobalSpectrogramViewState()
{
    auto prj = context()->currentProject();

    if (!prj) {
        return;
    }

    PlaybackToolBarControlItem* const item
        = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(TOGGLE_GLOBAL_SPECTROGRAM_VIEW_ACTION_CODE));

    if (item == nullptr) {
        return;
    }

    const auto vs = prj->viewState();
    const bool isOn = vs->globalSpectrogramViewIsOn();
    item->setSelected(isOn);

    const QColor backgroundColor{ uiConfiguration()->currentTheme().values.value(isOn ? muse::ui::ACCENT_COLOR : muse::ui::BUTTON_COLOR).
                                  toString() };
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateActions()
{
    ToolBarItemList items;

    muse::ui::ToolConfig playbackConfig = uiConfiguration()->toolConfig(TOOLBAR_NAME,
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

        if (citem.action == PLAYBACK_PLAY_QUERY.toString()) {
            item->setId(PLAY_PAUSE_ITEM_ID); // for quick finding
        }

        if (citem.action == PLAYBACK_STOP_QUERY.toString()) {
            item->setId(STOP_ITEM_ID); // for quick finding
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
        item->setIconColor(QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString()));
        item->setBackgroundColor(QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString()));
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
