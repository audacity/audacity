/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "internal/projectsceneuiactions.h"

#include "playback/view/toolbars/playbacktoolbarlevelitem.h"
#include "playback/view/toolbars/playbacktoolbarcontrolitem.h"
#include "playback/view/toolbars/playbacktoolbartimeitem.h"
#include "playback/view/toolbars/playbacktoolbarbpmitem.h"
#include "playback/view/toolbars/playbacktoolbartimesignatureitem.h"
#include "record/view/toolbars/playbacktoolbarrecordlevelitem.h"
#include "projectscene/view/toolbars/snaptoolbaritem.h"

#include "containers.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::actions;
using namespace au::projectscene;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static const QString PLAY_PAUSE_ITEM_ID("play-pause-id");
static const QString STOP_ITEM_ID("stop-id");

static const ActionCode PLAY_ACTION_CODE("play");
static const ActionCode PAUSE_ACTION_CODE("pause");
static const ActionCode STOP_ACTION_CODE("stop");

static const ActionCode RECORD_ACTION_CODE("record");
static const ActionCode PAUSE_RECORD_ACTION_CODE("pause-record");
static const ActionCode STOP_RECORD_ACTION_CODE("stop-record");

static const ActionCode REWIND_START_ACTION_CODE("rewind-start");
static const ActionCode REWIND_END_ACTION_CODE("rewind-end");
static const ActionCode LOOP_ACTION_CODE("loop");

static const ActionCode PLAYBACK_LEVEL("playback-level");
static const ActionCode PLAYBACK_TIME("playback-time");
static const ActionCode PLAYBACK_BPM("playback-bpm");
static const ActionCode PLAYBACK_TIME_SIGNATURE("playback-time-signature");
static const ActionCode RECORD_LEVEL("record-level");

static const ActionCode SNAP_ACTION_CODE("snap");

static PlaybackToolBarModel::ItemType itemType(const ActionCode& actionCode)
{
    std::map<ActionCode, PlaybackToolBarModel::ItemType> types = {
        { PLAYBACK_LEVEL, PlaybackToolBarModel::PLAYBACK_LEVEL },
        { PLAYBACK_TIME, PlaybackToolBarModel::PLAYBACK_TIME },
        { PLAYBACK_BPM, PlaybackToolBarModel::PLAYBACK_BPM },
        { PLAYBACK_TIME_SIGNATURE, PlaybackToolBarModel::PLAYBACK_TIME_SIGNATURE },
        { RECORD_LEVEL, PlaybackToolBarModel::RECORD_LEVEL },
        { PLAY_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { STOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { RECORD_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { REWIND_START_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { REWIND_END_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
        { LOOP_ACTION_CODE, PlaybackToolBarModel::PLAYBACK_CONTROL },
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
    uiConfiguration()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
        reload();
    });

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        reload();
    });

    context()->currentProjectChanged().onNotify(this, [this]() {
        reload();

        emit isEnabledChanged();
    });

    updateActions();

    AbstractToolBarModel::load();
}

void PlaybackToolBarModel::reload()
{
    load();
    updateStates();
}

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (containsAction(codes, PLAY_ACTION_CODE) || containsAction(codes, PAUSE_ACTION_CODE)
        || containsAction(codes, PAUSE_RECORD_ACTION_CODE)) {
        updatePlayState();
    }

    if (containsAction(codes, STOP_ACTION_CODE) || containsAction(codes, STOP_RECORD_ACTION_CODE)) {
        updateStopState();
    }

    if (containsAction(codes, RECORD_ACTION_CODE)) {
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
}

void PlaybackToolBarModel::updatePlayState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(PLAY_PAUSE_ITEM_ID));

    if (item == nullptr) {
        return;
    }

    bool isPlaying = controller()->isPlaying();
    bool isRecording = recordController()->isRecording();

    ActionCode code = isPlaying ? PAUSE_ACTION_CODE : PLAY_ACTION_CODE;
    if (isRecording) {
        code = PAUSE_RECORD_ACTION_CODE;
    }

    UiAction action = uiActionsRegister()->action(code);
    item->setAction(action);

    item->setSelected(isPlaying);

    QColor iconColor = QColor(configuration()->playColor().toQColor());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isPlaying) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
        backgroundColor = QColor(configuration()->playColor().toQColor());
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

    bool isRecording = recordController()->isRecording();

    ActionCode code = STOP_ACTION_CODE;
    if (isRecording) {
        code = STOP_RECORD_ACTION_CODE;
    }

    UiAction action = uiActionsRegister()->action(code);
    item->setAction(action);

    QColor iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    item->setIconColor(iconColor);
}

void PlaybackToolBarModel::updateRecordState()
{
    PlaybackToolBarControlItem* item = dynamic_cast<PlaybackToolBarControlItem*>(findItemPtr(RECORD_ACTION_CODE));

    if (item == nullptr) {
        return;
    }

    bool isRecording = recordController()->isRecording();

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

    if (item == nullptr) {
        return;
    }

    bool isLooping = false; // todo: from controller
    item->setSelected(isLooping);

    QColor iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    QColor backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).toString());
    if (isLooping) {
        iconColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_PRIMARY_COLOR).toString());
        backgroundColor = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).toString());
    }

    item->setIconColor(iconColor);
    item->setBackgroundColor(backgroundColor);
}

void PlaybackToolBarModel::updateActions()
{
    ToolBarItemList items;

    beginResetModel();

    muse::ui::ToolConfig playbackConfig
        = uiConfiguration()->toolConfig(TOOLBAR_NAME, ProjectSceneUiActions::defaultPlaybackToolBarConfig());

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

        if (citem.action == PLAY_ACTION_CODE) {
            item->setId(PLAY_PAUSE_ITEM_ID); // for quick finding
        }

        if (citem.action == STOP_ACTION_CODE) {
            item->setId(STOP_ITEM_ID); // for quick finding
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
