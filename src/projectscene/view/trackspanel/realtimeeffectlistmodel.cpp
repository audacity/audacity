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
        beginResetModel();
        m_trackEffectLists.clear();
        endResetModel();
        emit trackNameChanged();
        emit trackEffectsActiveChanged();
        return;
    }

    project->trackChanged().onReceive(this, [this](au::trackedit::Track track)
    {
        if (trackId() == track.id) {
            emit trackNameChanged();
        }
    });
}

bool RealtimeEffectListModel::belongsWithMe(effects::TrackId trackId) const
{
    return isMasterTrack() == (trackId == IRealtimeEffectService::masterTrackId);
}

void RealtimeEffectListModel::doLoad()
{
    //! Note: we listen to the individual effect-list changes because `onChanged` and its layout change reset the scrollbar position. Unless ...
    //! the RealtimeEffectListItem.qml listens to layoutAboutToBeChanged and layoutChanged signals and saves and restores the `contentY` property.
    //! At the time of writing, we actually do this (see RealtimeEffectListItem.qml), but it may have some unexpected drawback, so for now we call less drastic
    //! `beginInsertRows`, `dataChanged`, etc. whenever possible.
    //! (When undo or redo is called, we just get a new list, and it is hard to know which of insert, remove, replace and especially move happened,
    //! so we just call layoutAboutToChange.)

    realtimeEffectService()->realtimeEffectAdded().onReceive(this,
                                                             [this](effects::TrackId trackId, RealtimeEffectStatePtr state) {
        if (belongsWithMe(
                trackId)) {
            onAdded(trackId, state);
        }
    });

    realtimeEffectService()->realtimeEffectReplaced().onReceive(this,
                                                                [this](effects::TrackId trackId, EffectChainLinkIndex index,
                                                                       RealtimeEffectStatePtr newState) {
        if (belongsWithMe(trackId)) {
            onReplaced(trackId, index, newState);
        }
    });

    realtimeEffectService()->realtimeEffectRemoved().onReceive(this, [this](effects::TrackId trackId, RealtimeEffectStatePtr state) {
        if (belongsWithMe(trackId)) {
            onRemoved(trackId, state);
        }
    });

    realtimeEffectService()->realtimeEffectMoved().onReceive(this, [this](effects::TrackId trackId, EffectChainLinkIndex from,
                                                                          EffectChainLinkIndex to) {
        if (belongsWithMe(trackId)) {
            onMoved(trackId, from, to);
        }
    });

    realtimeEffectService()->realtimeEffectStackChanged().onReceive(this, [this](effects::TrackId trackId)
    {
        if (belongsWithMe(trackId)) {
            onChanged(trackId);
        }
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    {
        onProjectChanged();
    });
    onProjectChanged();

    doPopulateMenu();
}

void RealtimeEffectListModel::onAdded(effects::TrackId trackId, const effects::RealtimeEffectStatePtr& newState)
{
    const auto it = m_trackEffectLists.find(trackId);
    IF_ASSERT_FAILED(it != m_trackEffectLists.end()) {
        return;
    }

    auto& list = it->second;
    const int index = list.size();
    beginInsertRows(QModelIndex(), index, index);
    list.insert(list.begin() + index, std::make_shared<RealtimeEffectListItemModel>(this, newState));
    endInsertRows();
}

void RealtimeEffectListModel::onReplaced(effects::TrackId trackId, effects::EffectChainLinkIndex index,
                                         const effects::RealtimeEffectStatePtr& newState)
{
    const auto it = m_trackEffectLists.find(trackId);
    IF_ASSERT_FAILED(it != m_trackEffectLists.end()) {
        return;
    }

    auto& list = it->second;
    IF_ASSERT_FAILED(index >= 0 && index < static_cast<int>(list.size())) {
        return;
    }

    list[index] = std::make_shared<RealtimeEffectListItemModel>(this, newState);
    emit dataChanged(createIndex(index, 0), createIndex(index, 0));
}

void RealtimeEffectListModel::onRemoved(effects::TrackId trackId, const RealtimeEffectStatePtr& state)
{
    const auto it = m_trackEffectLists.find(trackId);
    IF_ASSERT_FAILED(it != m_trackEffectLists.end()) {
        return;
    }

    const auto& list = it->second;
    const auto it2 = std::find_if(list.begin(), list.end(), [state](const RealtimeEffectListItemModelPtr& item) {
        return item->effectStateId == state;
    });
    IF_ASSERT_FAILED(it2 != list.end()) {
        return;
    }

    const int index = it2 - list.begin();
    beginRemoveRows(QModelIndex(), index, index);
    m_trackEffectLists[trackId].erase(it2);
    endRemoveRows();
}

void RealtimeEffectListModel::onMoved(effects::TrackId trackId, effects::EffectChainLinkIndex from, effects::EffectChainLinkIndex to)
{
    if (from == to) {
        return;
    }

    const auto it = m_trackEffectLists.find(trackId);
    IF_ASSERT_FAILED(it != m_trackEffectLists.end()) {
        return;
    }

    auto& list = it->second;
    IF_ASSERT_FAILED(from >= 0 && from < static_cast<int>(list.size())) {
        return;
    }

    IF_ASSERT_FAILED(to >= 0 && to < static_cast<int>(list.size())) {
        return;
    }

    // Using beginMoveRows/endMoveRows unfortunately causes a weird behavior
    // when reordering while the scrollbar is not at the top.
    // This approach has the drawback of resetting the scrollbar position to 0,
    // unless the `contentY` property is saved and restored (see RealtimeEffectListItem.qml).

    emit layoutAboutToBeChanged();

    const QModelIndex oldIndex = index(from, 0);
    const QModelIndex newIndex = index(to, 0);
    const QVector<QModelIndex> persistentIndexes = { oldIndex };
    const QVector<QModelIndex> newIndexes = { newIndex };
    changePersistentIndexList(persistentIndexes, newIndexes);
    auto item = list.takeAt(from);
    list.insert(to, std::move(item));

    emit layoutChanged();
}

void RealtimeEffectListModel::handleMenuItemWithState(const QString& itemId, const RealtimeEffectListItemModel* item)
{
    TRACEFUNC;

    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }

    const MenuItem& menuItem = findItem(itemId);

    if (itemId == "realtimeeffect-remove") {
        realtimeEffectService()->removeRealtimeEffect(*tId, item->effectStateId);
        return;
    }

    if (itemId == "realtimeeffect-replace") {
        const auto& list = m_trackEffectLists.at(*tId);
        const auto it = std::find_if(list.begin(), list.end(), [item](const RealtimeEffectListItemModelPtr& listItem) {
            return listItem.get() == item;
        });
        IF_ASSERT_FAILED(it != list.end()) {
            return;
        }
        const int itemIndex = it - list.begin();
        const auto effectId = menuItem.args().arg<effects::EffectId>(0);
        if (const RealtimeEffectStatePtr newState = realtimeEffectService()->replaceRealtimeEffect(*tId, itemIndex, effectId)) {
            effectsProvider()->showEffect(newState);
        }
        return;
    }
}

void RealtimeEffectListModel::doPopulateMenu()
{
    MenuItemList items;

    const auto categoryList = effectsProvider()->effectsCategoryList();
    std::unordered_map<String, MenuItemList> menuCategories;

    items << makeMenuItem("realtimeeffect-remove", muse::TranslatableString("projectscene", "No effect")) << makeSeparator();

    // Populate with available realtime effect.
    for (const effects::EffectMeta& meta : effectsProvider()->effectMetaList()) {
        if (!meta.isRealtimeCapable) {
            continue;
        }
        MenuItem* item = makeMenuItem("realtimeeffect-replace", muse::TranslatableString::untranslatable(meta.title));
        item->setArgs(actions::ActionData::make_arg1(effects::EffectId { meta.id }));
        menuCategories[meta.categoryId].push_back(item);
    }

    for (const auto& entry : menuCategories) {
        items << makeMenu(muse::TranslatableString::untranslatable(entry.first), entry.second);
    }

    setItems(items);

    emit availableEffectsChanged();
}

void RealtimeEffectListModel::onChanged(effects::TrackId trackId)
{
    const std::optional<std::vector<RealtimeEffectStatePtr> > newStack = realtimeEffectService()->effectStack(trackId);

    emit layoutAboutToBeChanged();

    if (!newStack.has_value()) {
        m_trackEffectLists.erase(trackId);
        // Track was deleted, apparently.
        emit trackNameChanged();
        emit trackEffectsActiveChanged();
    } else {
        // Do not brute-force delete and re-create everything: in case the dialog for a RealtimeEffectListItemModel is open,
        // we don't want it to close unless the effect state it refers to was removed.
        const EffectList& oldList = m_trackEffectLists[trackId];
        EffectList newList(newStack->size());
        for (auto i = 0; i < static_cast<int>(newStack->size()); ++i) {
            const auto& state = newStack->at(i);
            const auto it = std::find_if(oldList.begin(), oldList.end(), [state](const RealtimeEffectListItemModelPtr& item) {
                return item->effectStateId == state;
            });
            if (it != oldList.end()) {
                newList[i] = *it;
            } else {
                newList[i] = std::make_shared<RealtimeEffectListItemModel>(this, state);
            }
        }
        m_trackEffectLists[trackId] = std::move(newList);
    }

    emit layoutChanged();
}

QVariantList RealtimeEffectListModel::availableEffects()
{
    return menuItemListToVariantList(items());
}

int RealtimeEffectListModel::count() const
{
    return rowCount();
}

void RealtimeEffectListModel::moveRow(int from, int to)
{
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }

    const auto& list = m_trackEffectLists.at(*tId);

    IF_ASSERT_FAILED(from >= 0 && from < list.size()) {
        return;
    }

    to = std::clamp<int>(to, 0, static_cast<int>(list.size()) - 1);

    if (from == to) {
        return;
    }

    realtimeEffectService()->moveRealtimeEffect(list[from]->effectStateId, to);
}

void RealtimeEffectListModel::onSelectedTrackIdChanged()
{
    emit trackNameChanged();
    emit trackEffectsActiveChanged();
}

QString RealtimeEffectListModel::prop_trackName() const
{
    if (!trackId().has_value()) {
        return QString();
    } else if (isMasterTrack()) {
        return muse::TranslatableString("projectScene", "Master").translated().toQString();
    }

    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project) {
        return QString();
    }

    const auto trackName = project->trackName(*trackId());
    IF_ASSERT_FAILED(trackName.has_value()) {
        return QString();
    }

    return QString::fromStdString(*trackName);
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
    const RealtimeEffectListItemModelPtr& item = *it;
    return QVariant::fromValue(item.get());
}

int RealtimeEffectListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    if (!trackId().has_value() || !m_trackEffectLists.count(*trackId())) {
        return 0;
    }
    return static_cast<int>(m_trackEffectLists.at(*trackId()).size());
}

bool RealtimeEffectListModel::prop_trackEffectsActive() const
{
    const auto tId = trackId();
    if (!tId.has_value()) {
        return false;
    }
    return realtimeEffectService()->trackEffectsActive(*tId);
}

void RealtimeEffectListModel::prop_setTrackEffectsActive(bool active)
{
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }
    realtimeEffectService()->setTrackEffectsActive(*tId, active);
}
