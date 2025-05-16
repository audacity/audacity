/*
 * Audacity: A Digital Audio Editor
 */
#include "addeffectmenumodel.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/effectsutils.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;

AddEffectMenuModel::AddEffectMenuModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent) {}

void AddEffectMenuModel::doLoad()
{
    doPopulateMenu();
}

void AddEffectMenuModel::doPopulateMenu()
{
    const MenuItemList items
        = au::effects::utils::realtimeEffectMenu(effectsConfiguration()->effectMenuOrganization(),
                                                 effectsProvider()->effectMetaList(), m_effectFilter, *this);
    setItems(items);
}

void AddEffectMenuModel::handleMenuItem(const QString& itemId)
{
    const MenuItem& menuItem = findItem(itemId);
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }

    const auto effectId = effects::effectIdFromAction(menuItem.id());
    if (const auto state = realtimeEffectService()->addRealtimeEffect(*tId, effectId)) {
        effectsProvider()->showEffect(state);
    }
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffectItem(const effects::EffectId& effectId)
{
    return makeMenuItem(effects::makeEffectAction(effects::REALTIME_EFFECT_ADD_ACTION, effectId).toString());
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffect(const muse::String& title,
                                                                 const muse::uicomponents::MenuItemList& items)
{
    return makeMenu(muse::TranslatableString::untranslatable(title), items);
}
