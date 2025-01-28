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
                                                                    RealtimeEffectStatePtr item)
    {
        if (isMasterTrack() && trackId != IRealtimeEffectService::masterTrackId) {
            return;
        }
        insertEffect(trackId, index, item);
    });

    realtimeEffectService()->realtimeEffectRemoved().onReceive(this,
                                                               [this](effects::TrackId trackId, RealtimeEffectStatePtr item) {
        if (isMasterTrack() && trackId != IRealtimeEffectService::masterTrackId) {
            return;
        }
        removeEffect(trackId, item);
    });

    realtimeEffectService()->realtimeEffectReplaced().onReceive(this,
                                                                [this](effects::TrackId trackId, EffectChainLinkIndex index,
                                                                       RealtimeEffectStatePtr oldItem, RealtimeEffectStatePtr newItem)
    {
        if (isMasterTrack() && trackId != IRealtimeEffectService::masterTrackId) {
            return;
        }
        removeEffect(trackId, oldItem);
        insertEffect(trackId, index, newItem);
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    {
        onProjectChanged();
    });
    onProjectChanged();

    populateMenu();
}

void RealtimeEffectListModel::doResetList()
{
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

void RealtimeEffectListModel::populateMenu()
{
    MenuItemList items;

    const auto categoryList = effectsProvider()->effectsCategoryList();
    std::unordered_map<String, MenuItemList> menuCategories;

    items << makeMenuItem("realtimeeffect-remove", muse::TranslatableString("projectscene", "No effect"));

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

void RealtimeEffectListModel::insertEffect(effects::TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& e)
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
    list.insert(list.begin() + index, std::make_shared<RealtimeEffectListItemModel>(this, e));
    if (affectsSelectedTrack) {
        endInsertRows();
    }
}

void RealtimeEffectListModel::removeEffect(effects::TrackId trackId, const RealtimeEffectStatePtr& e)
{
    if (!m_trackEffectLists.count(trackId)) {
        return;
    }

    std::vector<RealtimeEffectListItemModelPtr>& list = m_trackEffectLists.at(trackId);
    const auto it = std::find_if(list.begin(), list.end(), [e](const RealtimeEffectListItemModelPtr& item) {
        return item->effectStateId == e;
    });

    IF_ASSERT_FAILED(it != list.end()) {
        return;
    }
    const int index = it - list.begin();

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
