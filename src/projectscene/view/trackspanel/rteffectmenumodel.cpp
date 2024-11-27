/*
 * Audacity: A Digital Audio Editor
 */
#include "rteffectmenumodel.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;

RtEffectMenuModel::RtEffectMenuModel(QObject* parent)
    : RtEffectMenuModelBase(parent) {}

void RtEffectMenuModel::load()
{
    TRACEFUNC;

    RtEffectMenuModelBase::load();

    MenuItemList items;

    const auto metaList = effectsProvider()->effectMetaList();
    const auto categoryList = effectsProvider()->effectsCategoryList();
    std::unordered_map<String, MenuItemList> menuCategories;

    for (const effects::EffectMeta& meta : metaList) {
        if (!meta.isRealtimeCapable) {
            continue;
        }
        MenuItem* item = makeMenuItem("realtimeeffect-add", TranslatableString::untranslatable(meta.title));
        item->setArgs(actions::ActionData::make_arg2<String, au::trackedit::TrackId>(meta.id, m_trackId));
        menuCategories[meta.categoryId].push_back(item);
    }

    for (const auto& entry : menuCategories) {
        items << makeMenu(TranslatableString::untranslatable(entry.first), entry.second);
    }

    setItems(items);
}
