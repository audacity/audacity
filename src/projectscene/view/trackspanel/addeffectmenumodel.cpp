/*
 * Audacity: A Digital Audio Editor
 */
#include "addeffectmenumodel.h"
#include "effects/effects_base/effectstypes.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;

AddEffectMenuModel::AddEffectMenuModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent) {}

void AddEffectMenuModel::handleMenuItem(const QString& itemId)
{
    const MenuItem& menuItem = findItem(itemId);
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value()) {
        return;
    }

    const auto effectId = effects::effectIdFromAction(menuItem.id());
    // first use of a plugin in this session validates it in the background first
    effectsProvider()->loadEffectAsync(effectId).onResolve(this, [this, trackId = *tId, effectId](bool loaded) {
        if (!loaded) {
            LOGW() << "effect not available: " << effectId;
            return;
        }
        if (const auto state = realtimeEffectService()->addRealtimeEffect(trackId, effectId)) {
            effectViewController()->showEffect(state);
        }
    });
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffectItem(const effects::EffectId& effectId)
{
    return makeMenuItem(effects::makeEffectAction(effects::REALTIME_EFFECT_ADD_ACTION, effectId));
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffect(const muse::String& title,
                                                                 const muse::uicomponents::MenuItemList& items)
{
    return makeMenu(muse::TranslatableString::untranslatable(title), items);
}
