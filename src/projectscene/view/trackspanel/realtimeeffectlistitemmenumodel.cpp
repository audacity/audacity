/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmenumodel.h"
#include "effects/effects_base/effectstypes.h"
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
        MenuItem* item = makeMenuItem(effects::makeEffectAction(effects::REALTIME_EFFECT_REPLACE_ACTION,
                                                                meta.id).toString(), muse::TranslatableString::untranslatable(meta.title));
        item->setCheckable(true);
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

    if (menuItem.id() == "realtimeeffect-remove") {
        realtimeEffectService()->removeRealtimeEffect(*tId, m_effectState);
    } else {
        const auto effectId = effects::effectIdFromAction(menuItem.id());
        if (const RealtimeEffectStatePtr newState = realtimeEffectService()->replaceRealtimeEffect(*tId, m_effectState, effectId)) {
            effectsProvider()->showEffect(newState);
        }
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
    const auto myEffectId = muse::String::fromStdString(m_effectState->GetPluginID().ToStdString());
    for (MenuItem* category : itemList) {
        auto categoryChecked = false;
        for (MenuItem* subItem : category->subitems()) {
            const auto effectId = effects::effectIdFromAction(subItem->id());
            categoryChecked |= myEffectId == effectId;
            subItem->setChecked(myEffectId == effectId);
        }
        category->setChecked(categoryChecked);
    }
}
