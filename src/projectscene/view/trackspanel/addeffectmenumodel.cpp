/*
 * Audacity: A Digital Audio Editor
 */
#include "addeffectmenumodel.h"
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
        MenuItem* item = makeMenuItem("realtimeeffect-add", TranslatableString::untranslatable(meta.title));
        item->setArgs(actions::ActionData::make_arg1(meta.id));
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

    const auto effectId = menuItem.args().arg<effects::EffectId>(0);
    if (const auto state = realtimeEffectService()->addRealtimeEffect(*tId, effectId)) {
        effectsProvider()->showEffect(state);
    }
}
