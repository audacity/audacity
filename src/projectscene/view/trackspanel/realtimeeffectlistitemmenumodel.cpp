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
    IF_ASSERT_FAILED(m_stateId.has_value()) {
        return;
    }
    const MenuItem& menuItem = findItem(itemId);
    const auto tId = trackId();
    const auto state = stateRegister()->stateById(*m_stateId);
    IF_ASSERT_FAILED(tId.has_value() && state) {
        return;
    }

    if (menuItem.id() == "realtimeeffect-remove") {
        realtimeEffectService()->removeRealtimeEffect(*tId, state);
    } else {
        const auto effectId = effects::effectIdFromAction(menuItem.id());
        if (const RealtimeEffectStatePtr newState = realtimeEffectService()->replaceRealtimeEffect(*tId, state, effectId)) {
            effectsProvider()->showEffect(newState);
        }
    }
}

QVariantList RealtimeEffectListItemMenuModel::availableEffects() const
{
    return menuItemListToVariantList(items());
}

au::effects::RealtimeEffectStateId RealtimeEffectListItemMenuModel::prop_effectStateId() const
{
    return m_stateId.value_or(-1);
}

void RealtimeEffectListItemMenuModel::prop_setEffectStateId(effects::RealtimeEffectStateId stateId)
{
    if (m_stateId == stateId) {
        return;
    }
    m_stateId = stateId;
    updateEffectCheckmarks();
    emit effectStateIdChanged();
}

void RealtimeEffectListItemMenuModel::updateEffectCheckmarks()
{
    if (!m_stateId.has_value()) {
        return;
    }
    const auto state = stateRegister()->stateById(*m_stateId);
    if (!state) {
        return;
    }
    const MenuItemList& itemList = items();
    const auto myEffectId = muse::String::fromStdString(state->GetPluginID().ToStdString());
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
