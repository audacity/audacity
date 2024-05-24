/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "internal/playbackuiactions.h"

#include "view/toolbars/playbacktoolbarabstractitem.h"
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

static PlaybackToolBarAbstractItem::ItemType itemType(const ActionCode& actionCode)
{
    std::map<ActionCode, PlaybackToolBarAbstractItem::ItemType> types = {
        { SECTION_CODE, PlaybackToolBarAbstractItem::ItemType::SECTION },
        { PLAYBACK_LEVEL, PlaybackToolBarAbstractItem::ItemType::PLAYBACK_LEVEL },
        { RECORD_LEVEL, PlaybackToolBarAbstractItem::ItemType::RECORD_LEVEL }
    };

    return muse::value(types, actionCode, PlaybackToolBarAbstractItem::ItemType::ACTION);
}

PlaybackToolBarModel::PlaybackToolBarModel(QObject* parent)
    : QAbstractListModel(parent)
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
}

void PlaybackToolBarModel::handleMenuItem(const QString& itemId)
{
    PlaybackToolBarAbstractItem* item = findItem(itemId.toStdString());
    dispatcher()->dispatch(item->action().code, item->args());
}

QVariantMap PlaybackToolBarModel::get(int index)
{
    QVariantMap result;

    QHash<int, QByteArray> names = roleNames();
    QHashIterator<int, QByteArray> i(names);
    while (i.hasNext()) {
        i.next();
        QModelIndex idx = this->index(index, 0);
        QVariant data = idx.data(i.key());
        result[i.value()] = data;
    }

    return result;
}

QVariant PlaybackToolBarModel::data(const QModelIndex& index, int role) const
{
    TRACEFUNC;

    int row = index.row();

    if (!index.isValid() || row >= rowCount()) {
        return QVariant();
    }

    PlaybackToolBarAbstractItem* item = m_items[row];
    ActionCode actionCode = item->action().code;

    switch (role) {
    case ItemRole: return QVariant::fromValue(item);
    case IsMenuSecondaryRole: return isMenuSecondary(actionCode);
    case OrderRole: return row;
    case SectionRole: return item->section();
    default: return QVariant();
    }
}

QHash<int, QByteArray> PlaybackToolBarModel::roleNames() const
{
    QHash<int, QByteArray> roles;
    roles[ItemRole] = "item";
    roles[IsMenuSecondaryRole] = "isMenuSecondary";
    roles[OrderRole] = "order";
    roles[SectionRole] = "section";

    return roles;
}

int PlaybackToolBarModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

PlaybackToolBarAbstractItem* PlaybackToolBarModel::findItem(const muse::actions::ActionCode& actionCode)
{
    for (PlaybackToolBarAbstractItem* item : m_items) {
        if (item->action().code == actionCode) {
            return item;
        }
    }

    return nullptr;
}

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (containsAction(codes, PLAY_ACTION_CODE)) {
        PlaybackToolBarAbstractItem* item = findItem(PLAY_ACTION_CODE);
        item->setAction(playAction());
    }

    if (containsAction(codes, RECORD_ACTION_CODE)) {
        PlaybackToolBarAbstractItem* item = findItem(RECORD_ACTION_CODE);
        item->setAction(recordAction());
    }
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
}

void PlaybackToolBarModel::updateActions()
{
    m_items.clear();

    beginResetModel();

    muse::ui::ToolConfig playbackConfig
        = uiConfiguration()->toolConfig(TOOLBAR_NAME, PlaybackUiActions::defaultPlaybackToolConfig());

    for (const muse::ui::ToolConfig::Item& citem : playbackConfig.items) {
        if (!citem.show) {
            continue;
        }

        PlaybackToolBarAbstractItem* item = makeItem(uiActionsRegister()->action(citem.action));

        if (citem.action == PLAY_ACTION_CODE) {
            item->setAction(playAction());
        }

        if (citem.action == RECORD_ACTION_CODE) {
            item->setAction(recordAction());
        }

        m_items << item;
    }

    endResetModel();

    emit itemsChanged();
}

bool PlaybackToolBarModel::isMenuSecondary(const muse::actions::ActionCode& actionCode) const
{
    UNUSED(actionCode);

    return false;
}

PlaybackToolBarAbstractItem* PlaybackToolBarModel::makeItem(const muse::ui::UiAction& action)
{
    PlaybackToolBarAbstractItem::ItemType type = itemType(action.code);

    switch (type) {
    case PlaybackToolBarAbstractItem::PLAYBACK_LEVEL: return new PlaybackToolBarLevelItem(action, type, this);
    case PlaybackToolBarAbstractItem::RECORD_LEVEL: return new record::PlaybackToolBarRecordLevelItem(action, type, this);
    default:
        break;
    }

    return new PlaybackToolBarAbstractItem(action, type, this);
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
