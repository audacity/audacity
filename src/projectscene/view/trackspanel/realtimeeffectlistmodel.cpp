/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistmodel.h"
#include "realtimeeffectlistitemmodel.h"
#include "global/defer.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace au::effects;

RealtimeEffectListModel::RealtimeEffectListModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent)
{
}

void RealtimeEffectListModel::onProjectChanged()
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        resetList();
        return;
    }

    project->trackRemoved().onReceive(this, [this](au::trackedit::Track track)
    {
        removeTrack(track.id);
    });

    project->trackChanged().onReceive(this, [this](au::trackedit::Track track)
    {
        if (trackId() == track.id) {
            emit trackNameChanged();
        }
    });
}

void RealtimeEffectListModel::doLoad()
{
    realtimeEffectService()->realtimeEffectAdded().onReceive(this,
                                                             [this](effects::TrackId trackId, EffectChainLinkIndex index,
                                                                    EffectStateId item)
    { insertEffect(trackId, index, item); });

    realtimeEffectService()->realtimeEffectRemoved().onReceive(this,
                                                               [this](effects::TrackId trackId, EffectChainLinkIndex index,
                                                                      EffectStateId item) {
        removeEffect(trackId, index, item);
    });

    realtimeEffectService()->realtimeEffectReplaced().onReceive(this,
                                                                [this](effects::TrackId trackId, EffectChainLinkIndex index,
                                                                       EffectStateId oldItem,
                                                                       EffectStateId newItem) {
        removeEffect(trackId, index, oldItem);
        insertEffect(trackId, index, newItem);
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this] { onProjectChanged(); });
    onProjectChanged();

    populateMenu();
}

void RealtimeEffectListModel::doResetList()
{
    if (m_trackEffectLists.empty()) {
        return;
    }
    m_trackEffectLists.clear();
}

void RealtimeEffectListModel::doRemoveTrack(const au::trackedit::TrackId& trackId)
{
    if (!m_trackEffectLists.count(trackId)) {
        return;
    }
    m_trackEffectLists.erase(trackId);
}

void RealtimeEffectListModel::handleMenuItemWithState(const QString& itemId, const RealtimeEffectListItemModel* item)
{
    TRACEFUNC;

    IF_ASSERT_FAILED(trackId().has_value()) {
        return;
    }

    MenuItem& menuItem = findItem(itemId);

    if (itemId == "realtimeeffect-remove") {
        menuItem.setArgs(actions::ActionData::make_arg2(*trackId(), item->effectStateId));
    }
    if (itemId == "realtimeeffect-replace") {
        const auto& list = m_trackEffectLists.at(*trackId());
        const auto it = std::find(list.begin(), list.end(), item);
        IF_ASSERT_FAILED(it != list.end()) {
            return;
        }
        const int itemIndex = it - list.begin();
        menuItem.setArgs(actions::ActionData::make_arg3(*trackId(), itemIndex, menuItem.args().arg<effects::EffectId>(1)));
    }

    AbstractMenuModel::handleMenuItem(itemId);
}

void RealtimeEffectListModel::populateMenu()
{
    MenuItemList items;

    const auto categoryList = effectsProvider()->effectsCategoryList();
    std::unordered_map<String, MenuItemList> menuCategories;

    {
        MenuItem* noEffectItem = makeMenuItem("realtimeeffect-remove", muse::TranslatableString("projectscene", "No effect"));
        noEffectItem->setArgs(actions::ActionData::make_arg2(effects::TrackId { -1 }, EffectStateId { 0 }));
        items << noEffectItem;
    }

    // Populate with available realtime effect.
    for (const effects::EffectMeta& meta : effectsProvider()->effectMetaList()) {
        if (!meta.isRealtimeCapable) {
            continue;
        }
        // TODO no one reacts to "realtimeeffect-replace" actions at the moment.
        MenuItem* item = makeMenuItem("realtimeeffect-replace", muse::TranslatableString::untranslatable(meta.title));
        item->setArgs(actions::ActionData::make_arg3(effects::TrackId { -1 }, EffectChainLinkIndex { -1 },
                                                     effects::EffectId { meta.id }));
        menuCategories[meta.categoryId].push_back(item);
    }

    for (const auto& entry : menuCategories) {
        items << makeMenu(muse::TranslatableString::untranslatable(entry.first), entry.second);
    }

    setItems(items);

    emit availableEffectsChanged();
}

void RealtimeEffectListModel::insertEffect(effects::TrackId trackId, EffectChainLinkIndex index, const EffectStateId& e)
{
    const auto sizeBefore = m_trackEffectLists.size();
    auto& list = m_trackEffectLists[trackId];
    IF_ASSERT_FAILED(index <= list.size()) {
        return;
    }
    const auto affectsSelectedTrack = trackId == this->trackId();
    if (affectsSelectedTrack) {
        beginInsertRows(QModelIndex(), index, index);
    }
    list.insert(list.begin() + index, new RealtimeEffectListItemModel(this, e));
    if (affectsSelectedTrack) {
        endInsertRows();
    }
}

void RealtimeEffectListModel::removeEffect(effects::TrackId trackId, EffectChainLinkIndex index, const EffectStateId& e)
{
    if (!m_trackEffectLists.count(trackId)) {
        return;
    }
    auto& list = m_trackEffectLists.at(trackId);
    IF_ASSERT_FAILED(index < list.size() && list[index]->effectStateId == e) {
        return;
    }
    const auto affectsSelectedTrack = trackId == this->trackId();
    if (affectsSelectedTrack) {
        beginRemoveRows(QModelIndex(), index, index);
    }
    list.erase(list.begin() + index);
    if (affectsSelectedTrack) {
        endRemoveRows();
    }
}

QVariantList RealtimeEffectListModel::availableEffects()
{
    return menuItemListToVariantList(items());
}

void RealtimeEffectListModel::onTrackIdChanged()
{
    emit trackNameChanged();
}

QString RealtimeEffectListModel::prop_trackName() const
{
    if (!trackId().has_value()) {
        return QString();
    }
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    return QString::fromStdString(project ? project->trackName(*trackId()) : "");
}

QHash<int, QByteArray> RealtimeEffectListModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rItemData, "itemData" }
    };

    return roles;
}

QVariant RealtimeEffectListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount() || role != rItemData || !trackId().has_value()
        || !m_trackEffectLists.count(*trackId())) {
        return QVariant();
    }
    auto it = m_trackEffectLists.at(*trackId()).begin();
    std::advance(it, index.row());
    return QVariant::fromValue(*it);
}

int RealtimeEffectListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    if (!trackId().has_value() || !m_trackEffectLists.count(*trackId())) {
        return 0;
    }
    return static_cast<int>(m_trackEffectLists.at(*trackId()).size());
}
