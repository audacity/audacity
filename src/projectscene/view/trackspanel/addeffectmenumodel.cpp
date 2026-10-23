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

    const effects::EffectId effectId = effects::EffectId::fromQString(menuItem.id());
    if (const auto state = realtimeEffectService()->addRealtimeEffect(*tId, effectId)) {
        effectViewController()->showEffect(state);
    }
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffectItem(const effects::EffectId& effectId)
{
    return makeEffectItem(effectId);
}

muse::uicomponents::MenuItem* AddEffectMenuModel::makeMenuEffect(const muse::String& title,
                                                                 const muse::uicomponents::MenuItemList& items)
{
    return makeMenu(muse::TranslatableString::untranslatable(title), items);
}
