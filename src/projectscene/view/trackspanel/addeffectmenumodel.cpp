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

void AddEffectMenuModel::doLoad()
{
    doPopulateMenu();
}

void AddEffectMenuModel::doPopulateMenu()
{
    MenuItemList items;

    const auto metaList = effectsProvider()->effectMetaList();
    const auto categoryList = effectsProvider()->effectsCategoryList();
    std::unordered_map<String, MenuItemList> menuCategories;

    for (const effects::EffectMeta& meta : metaList) {
        if (!meta.isRealtimeCapable) {
            continue;
        }
        const auto actionId = effects::makeEffectAction(effects::REALTIME_EFFECT_ADD_ACTION,
                                                        meta.id).toString();
        MenuItem* item = makeMenuItem(actionId, TranslatableString::untranslatable(meta.title));
        IF_ASSERT_FAILED(item) {
            continue;
        }
        menuCategories[meta.categoryId].push_back(item);
    }

    for (const auto& entry : menuCategories) {
        items << makeMenu(TranslatableString::untranslatable(entry.first), entry.second);
    }

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
