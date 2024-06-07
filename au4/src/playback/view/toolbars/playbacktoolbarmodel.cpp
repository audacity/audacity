/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "internal/playbackuiactions.h"

#include "view/toolbars/playbacktoolbarlevelitem.h"
#include "view/toolbars/playbacktoolbarcontrolitem.h"
#include "record/view/toolbars/playbacktoolbarrecordlevelitem.h"

#include "containers.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::actions;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static const ActionCode PLAY_ACTION_CODE("play");
static const ActionCode STOP_ACTION_CODE("stop");
static const ActionCode RECORD_ACTION_CODE("record");
static const ActionCode REWIND_START_ACTION_CODE("rewind-start");
static const ActionCode REWIND_END_ACTION_CODE("rewind-end");
static const ActionCode LOOP_ACTION_CODE("loop");

static const ActionCode PLAYBACK_LEVEL("playback-level");
static const ActionCode RECORD_LEVEL("record-level");

static PlaybackToolBarModel::ItemType itemType(const ActionCode& actionCode)
{
    std::map<ActionCode, PlaybackToolBarModel::ItemType> types = {
        { PLAYBACK_LEVEL, PlaybackToolBarModel::PLAYBACK_LEVEL },
        { RECORD_LEVEL, PlaybackToolBarModel::RECORD_LEVEL },
        { PLAY_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { STOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { RECORD_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { REWIND_START_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { REWIND_END_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { LOOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
    };

    return muse::value(types, actionCode, PlaybackToolBarModel::PROJECT_CONTROL);
}

PlaybackToolBarModel::PlaybackToolBarModel(QObject* parent)
    : AbstractToolBarModel(parent)
{
}

void PlaybackToolBarModel::load()
{
    uiConfiguration()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
        load();
    });

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        load();
    });

    context()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    updateActions();
    setupConnections();

    AbstractToolBarModel::load();
}

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (containsAction(codes, PLAY_ACTION_CODE) || containsAction(codes, RECORD_ACTION_CODE)) {
        updatePlayState();
        updateRecordState();
        updateRewindState();
        updateLoopState();
    } else if (containsAction(codes, LOOP_ACTION_CODE)) {
        updateLoopState();
    }

    AbstractToolBarModel::onActionsStateChanges(codes);
}

void PlaybackToolBarModel::updatePlayState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAY_ACTION_CODE));
    UiAction action = uiActionsRegister()->action(PLAY_ACTION_CODE);

    bool isPlaying = controller()->isPlaying();
    bool isRecording = recordController()->isRecording();

    action.iconCode = isPlaying || isRecording ? IconCode::Code::PAUSE_FILL : IconCode::Code::PLAY_FILL;
    item->setAction(action);

    item->setSelected(isPlaying);

    QColor iconColor = QColor(configuration()->playColor().toQColor());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isPlaying) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_PRIMARY_COLOR).toString());
        backgroundColor = QColor(configuration()->playColor().toQColor());
    } else if (isRecording) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateRecordState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(RECORD_ACTION_CODE));
    UiAction action = uiActionsRegister()->action(RECORD_ACTION_CODE);
    UiActionState state = uiActionsRegister()->actionState(RECORD_ACTION_CODE);

    bool isPlaying = controller()->isPlaying();
    bool isRecording = recordController()->isRecording();

    state.enabled = !isPlaying;
    item->setState(state);

    item->setSelected(isRecording);

    QColor iconColor = QColor(recordConfiguration()->recordColor().toQColor());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isRecording) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_PRIMARY_COLOR).toString());
        backgroundColor = QColor(recordConfiguration()->recordColor().toQColor());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateLoopState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(LOOP_ACTION_CODE));
    UiActionState state = uiActionsRegister()->actionState(LOOP_ACTION_CODE);

    bool isLooping = false; // todo: from controller
    item->setSelected(isLooping);

    bool isRecording = recordController()->isRecording();
    state.enabled = !isRecording;
    item->setState(state);

    QColor iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isLooping) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_PRIMARY_COLOR).toString());
        backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).toString());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateRewindState()
{
    PlaybackToolBarControlItem* rewindStartItem = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(REWIND_START_ACTION_CODE));
    UiActionState startState = uiActionsRegister()->actionState(RECORD_ACTION_CODE);

    bool isPlaying = controller()->isPlaying();
    bool isRecording = recordController()->isRecording();

    startState.enabled = !isPlaying && !isRecording;
    rewindStartItem->setState(startState);

    PlaybackToolBarControlItem* rewindEndItem = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(REWIND_END_ACTION_CODE));
    UiActionState endState = uiActionsRegister()->actionState(REWIND_END_ACTION_CODE);

    endState.enabled = !isPlaying && !isRecording;
    rewindEndItem->setState(startState);
}

void PlaybackToolBarModel::setupConnections()
{
    controller()->isPlayingChanged().onNotify(this, [this]() {
        onActionsStateChanges({ PLAY_ACTION_CODE });
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        onActionsStateChanges({ RECORD_ACTION_CODE });
    });
}

void PlaybackToolBarModel::onProjectChanged()
{
    updateActions();
}

void PlaybackToolBarModel::updateActions()
{
    ToolBarItemList items;

    beginResetModel();

    muse::ui::ToolConfig playbackConfig
        = uiConfiguration()->toolConfig(TOOLBAR_NAME, PlaybackUiActions::defaultPlaybackToolConfig());

    for (const muse::ui::ToolConfig::Item& citem : playbackConfig.items) {
        if (!citem.show) {
            continue;
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

    endResetModel();
}

ToolBarItem* PlaybackToolBarModel::makeLocalItem(const ActionCode& actionCode)
{
    PlaybackToolBarModel::ItemType type = itemType(actionCode);

    const UiAction& action = uiActionsRegister()->action(actionCode);

    switch (type) {
    case PlaybackToolBarModel::PLAYBACK_LEVEL:
        return new PlaybackToolBarLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
    case PlaybackToolBarModel::RECORD_LEVEL:
        return new record::PlaybackToolBarRecordLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
    case au::playback::PlaybackToolBarModel::PLAYBACK_CONTROL: {
        PlaybackToolBarControlItem* item = new PlaybackToolBarControlItem(action, static_cast<ToolBarItemType::Type>(type), this);
        item->setIconColor(QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString()));
        item->setBackgroundColor(QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString()));
        return item;
    }
    case au::playback::PlaybackToolBarModel::PROJECT_CONTROL: {
        ToolBarItem* item = AbstractToolBarModel::makeItem(actionCode);
        item->setType(static_cast<ToolBarItemType::Type>(type));
        return item;
    }
    default:
        break;
    }

    return nullptr;
}
