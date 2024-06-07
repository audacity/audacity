/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "internal/playbackuiactions.h"

#include "view/toolbars/playbacktoolbarlevelitem.h"
#include "record/view/toolbars/playbacktoolbarrecordlevelitem.h"

#include "containers.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::actions;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static const ActionCode SECTION_CODE("");
static const ActionCode PLAY_ACTION_CODE("play");
static const ActionCode PLAYBACK_LEVEL("playback-level");
static const ActionCode RECORD_ACTION_CODE("record");
static const ActionCode RECORD_LEVEL("record-level");

static PlaybackToolBarModel::ItemType itemType(const ActionCode& actionCode)
{
    std::map<ActionCode, PlaybackToolBarModel::ItemType> types = {
        { PLAYBACK_LEVEL, PlaybackToolBarModel::ItemType::PLAYBACK_LEVEL },
        { RECORD_LEVEL, PlaybackToolBarModel::ItemType::RECORD_LEVEL }
    };

    return muse::value(types, actionCode, PlaybackToolBarModel::ItemType::UNDEFINED);
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

    context()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    updateActions();
    setupConnections();

    AbstractToolBarModel::load();
}

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (containsAction(codes, PLAY_ACTION_CODE)) {
        ToolBarItem& item = findItem(PLAY_ACTION_CODE);
        item.setAction(playAction());
    }

    if (containsAction(codes, RECORD_ACTION_CODE)) {
        ToolBarItem& item = findItem(RECORD_ACTION_CODE);
        item.setAction(recordAction());
    }

    AbstractToolBarModel::onActionsStateChanges(codes);
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
            item = AbstractToolBarModel::makeItem(citem.action);
        }

        if (citem.action == PLAY_ACTION_CODE) {
            item->setAction(playAction());
        }

        if (citem.action == RECORD_ACTION_CODE) {
            item->setAction(recordAction());
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
    if (type == PlaybackToolBarModel::ItemType::UNDEFINED) {
        return nullptr;
    }

    const UiAction& action = uiActionsRegister()->action(actionCode);

    switch (type) {
    case PlaybackToolBarModel::ItemType::PLAYBACK_LEVEL:
        return new PlaybackToolBarLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
    case PlaybackToolBarModel::ItemType::RECORD_LEVEL:
        return new record::PlaybackToolBarRecordLevelItem(action, static_cast<ToolBarItemType::Type>(type), this);
    default:
        break;
    }

    return nullptr;
}

UiAction PlaybackToolBarModel::playAction() const
{
    UiAction action = uiActionsRegister()->action(PLAY_ACTION_CODE);

    bool isPlaying = controller()->isPlaying();
    action.iconCode =  isPlaying ? IconCode::Code::PAUSE : IconCode::Code::PLAY;

    return action;
}

UiAction PlaybackToolBarModel::recordAction() const
{
    UiAction action = uiActionsRegister()->action(RECORD_ACTION_CODE);

    bool isPlaying = recordController()->isRecording();
    action.iconCode =  isPlaying ? IconCode::Code::PAUSE : IconCode::Code::RECORD_FILL; // todo

    return action;
}
