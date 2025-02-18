/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmenumodel.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "global/defer.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace au::effects;

RealtimeEffectListItemMenuModel::RealtimeEffectListItemMenuModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent)
{
}

bool RealtimeEffectListItemMenuModel::belongsWithMe(effects::TrackId trackId) const
{
    return isMasterTrack() == (trackId == IRealtimeEffectService::masterTrackId);
}

void RealtimeEffectListItemMenuModel::doLoad()
{
    doPopulateMenu();
}

void RealtimeEffectListItemMenuModel::doPopulateMenu()
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
        MenuItem* const menu = makeMenu(muse::TranslatableString::untranslatable(entry.first), entry.second);
        menu->setCheckable(true);
        items << menu;
    }

    setItems(items);
    updateEffectCheckmarks();
}

void RealtimeEffectListItemMenuModel::handleMenuItem(const QString& itemId)
{
    const MenuItem& menuItem = findItem(itemId);
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value() && m_effectState) {
        return;
    }

    if (menuItem.id() == "realtimeeffect-replace") {
        const auto effectId = menuItem.args().arg<effects::EffectId>(0);
        if (const RealtimeEffectStatePtr newState = realtimeEffectService()->replaceRealtimeEffect(*tId, m_effectState, effectId)) {
            effectsProvider()->showEffect(newState);
        }
    } else if (menuItem.id() == "realtimeeffect-remove") {
        realtimeEffectService()->removeRealtimeEffect(*tId, m_effectState);
    } else {
        assert(false);
        LOGE() << "Unknown menu item id: " << itemId.toStdString();
    }
}

QVariantList RealtimeEffectListItemMenuModel::availableEffects() const
{
    return menuItemListToVariantList(items());
}

au::effects::RealtimeEffectStatePtr RealtimeEffectListItemMenuModel::prop_effectState() const
{
    return m_effectState;
}

void RealtimeEffectListItemMenuModel::prop_setEffectState(effects::RealtimeEffectStatePtr effectState)
{
    if (m_effectState == effectState) {
        return;
    }
    m_effectState = effectState;
    updateEffectCheckmarks();
    emit effectStateChanged();
}

void RealtimeEffectListItemMenuModel::updateEffectCheckmarks()
{
    const MenuItemList& itemList = items();
    const auto myEffectId = muse::String::fromStdString(m_effectState->GetID().ToStdString());
    for (MenuItem* category : itemList) {
        auto categoryChecked = false;
        for (MenuItem* subItem : category->subitems()) {
            const auto effectId = subItem->args().arg<effects::EffectId>(0);
            categoryChecked |= myEffectId == effectId;
            subItem->setChecked(myEffectId == effectId);
        }
        category->setChecked(categoryChecked);
    }
}
