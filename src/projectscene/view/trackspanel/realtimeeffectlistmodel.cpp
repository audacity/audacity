/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistmodel.h"
#include "realtimeeffectlistitemmodel.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace au::audio;
using namespace au::effects;

RealtimeEffectListModel::RealtimeEffectListModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent)
{
}

void RealtimeEffectListModel::doLoad()
{
    populateMenu();

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    { setListenerOnCurrentTrackeditProject(); });

    setListenerOnCurrentTrackeditProject();
}

void RealtimeEffectListModel::handleMenuItemWithState(const QString& itemId, const RealtimeEffectListItemModel* item)
{
    TRACEFUNC;

    MenuItem& menuItem = findItem(itemId);

    if (itemId == "realtimeeffect-remove") {
        menuItem.setArgs(actions::ActionData::make_arg2(m_trackId, item->effectStateId));
    }
    if (itemId == "realtimeeffect-replace") {
        const auto& list = m_trackEffectLists.at(m_trackId);
        const auto it = std::find(list.begin(), list.end(), item);
        IF_ASSERT_FAILED(it != list.end()) {
            return;
        }
        const int itemIndex = it - list.begin();
        menuItem.setArgs(actions::ActionData::make_arg3(m_trackId, itemIndex, menuItem.args().arg<effects::EffectId>(2)));
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

void RealtimeEffectListModel::setListenerOnCurrentTrackeditProject()
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
}

void RealtimeEffectListModel::insertEffect(effects::TrackId trackId, EffectChainLinkIndex index, const EffectStateId& e)
{
    auto& list = m_trackEffectLists[trackId];
    IF_ASSERT_FAILED(index <= list.size()) {
        return;
    }
    const auto affectsSelectedTrack = trackId == m_trackId;
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
    auto& list = m_trackEffectLists[trackId];
    IF_ASSERT_FAILED(index < list.size() && list[index]->effectStateId == e) {
        return;
    }
    const auto affectsSelectedTrack = trackId == m_trackId;
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

QHash<int, QByteArray> RealtimeEffectListModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rItemData, "itemData" }
    };

    return roles;
}

QVariant RealtimeEffectListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount() || role != rItemData || !m_trackEffectLists.count(m_trackId)) {
        return QVariant();
    }
    auto it = m_trackEffectLists.at(m_trackId).begin();
    std::advance(it, index.row());
    return QVariant::fromValue(*it);
}

int RealtimeEffectListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    if (!m_trackEffectLists.count(m_trackId)) {
        return 0;
    }
    return static_cast<int>(m_trackEffectLists.at(m_trackId).size());
}
