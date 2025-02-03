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
        resetModel([this] {
            m_trackEffectLists.clear();
        }, [this] {
            emit trackNameChanged();
            emit trackEffectsActiveChanged();
        });
        return;
    }

    project->trackRemoved().onReceive(this, [this](au::trackedit::Track track)
    {
        if (trackId() != track.id) {
            // Not the active track, no need to reset the model.
            m_trackEffectLists.erase(track.id);
            return;
        }
        resetModel([this, tId = track.id]
        {
            m_trackEffectLists.erase(tId);
        }, [this]
        {
            emit trackNameChanged();
            emit trackEffectsActiveChanged();
        });
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
    realtimeEffectService()->realtimeEffectStackChanged().onReceive(this, [this](effects::TrackId trackId)
                                                                    {
        const auto belongsWithMe = isMasterTrack() == (trackId == IRealtimeEffectService::masterTrackId);
        if (belongsWithMe) {
            refresh(trackId);
        } });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    {
        onProjectChanged();
    });
    onProjectChanged();

    doPopulateMenu();
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

void RealtimeEffectListModel::refresh(effects::TrackId trackId)
{
    const std::optional<std::vector<RealtimeEffectStatePtr> > newStack = realtimeEffectService()->effectStack(trackId);
    beginResetModel();
    if (!newStack.has_value()) {
        m_trackEffectLists.erase(trackId);
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
    endResetModel();
}

QVariantList RealtimeEffectListModel::availableEffects()
{
    return menuItemListToVariantList(items());
}

void RealtimeEffectListModel::moveRow(int from, int to)
{
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }

    if (from == to) {
        return;
    }

    const auto& list = m_trackEffectLists.at(*tId);
    IF_ASSERT_FAILED(from >= 0 && from < list.size() && to >= 0 && to < list.size()) {
        return;
    }

    realtimeEffectService()->reorderRealtimeEffect(list[from]->effectStateId, to);
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
