/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"
#include "log.h"

namespace impl {
using namespace muse;
using namespace muse::uicomponents;
using namespace au::effects;

using EffectIdSet = std::set<EffectId>;
using EffectMetaSet = std::unordered_set<const EffectMeta*>;
using AmbiguousTitleEntries = std::map<String /*effect title*/, EffectIdSet>;

MenuItemList makeItemsOrDisambiguationSubmenus(const AmbiguousTitleEntries& entries, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList items;
    for (const auto&[title, effectIds] : entries) {
        if (effectIds.size() == 1) {
            items.push_back(effectMenu.makeMenuEffectItem(*effectIds.begin()));
        } else {
            MenuItemList subItems;
            for (const auto& effectId : effectIds) {
                subItems.push_back(effectMenu.makeMenuEffectItem(effectId));
            }
            items << effectMenu.makeMenuEffect(title, subItems);
        }
    }
    return items;
}

MenuItem* makeEffectSubmenu(const String& title, const EffectIdSet& effectIds, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList subItems;
    for (const auto& effectId : effectIds) {
        subItems.push_back(effectMenu.makeMenuEffectItem(effectId));
    }
    return effectMenu.makeMenuEffect(title, subItems);
}

MenuItem* makeRealtimeBuiltinEffectSubmenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    EffectIdSet ids;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            ids.insert(meta.id);
        }
    }
    return makeEffectSubmenu(muse::String { "Audacity" }, ids, effectMenu);
}

MenuItemList makeDestructiveBuiltinEffectSubmenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<String /*category*/, EffectIdSet> categories;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            categories[meta.category].insert(meta.id);
        }
    }
    MenuItemList items;
    for (const auto& [category, effectIds] : categories) {
        items << makeEffectSubmenu(category, effectIds, effectMenu);
    }
    return items;
}

MenuItemList makeNonBuiltinEffectSubmenus(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<String /*family*/, std::map<String /*publisher*/, std::map<String /*title*/, EffectMetaSet> > > families;

    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::VST3) {
            families[muse::String{ "VST3" }][meta.vendor][meta.title].insert(&meta);
        }
    }

    MenuItemList items;

    if (families.empty()) {
        return items;
    }

    items << effectMenu.makeMenuSeparator();

    for (const auto& [family, publishers] : families) {
        MenuItemList publisherMenus;
        AmbiguousTitleEntries aloneEffectItems;
        for (const auto& [publisher, titles] : publishers) {
            if (titles.size() == 1) {
                // Only one title for this publisher (but possibly several binaries in different locations):
                // no need for a submenu
                const EffectMetaSet& metas = titles.begin()->second;
                for (const EffectMeta* meta : metas) {
                    aloneEffectItems[meta->title].insert(meta->id);
                }
            } else {
                AmbiguousTitleEntries publisherEffects;
                for (const auto& [title, effectMetas] : titles) {
                    for (const EffectMeta* meta : effectMetas) {
                        publisherEffects[meta->title].insert(meta->id);
                    }
                }
                publisherMenus << effectMenu.makeMenuEffect(publisher, makeItemsOrDisambiguationSubmenus(publisherEffects, effectMenu));
            }
        }

        publisherMenus << effectMenu.makeMenuSeparator() << makeItemsOrDisambiguationSubmenus(aloneEffectItems, effectMenu);
        items << effectMenu.makeMenuEffect(family, publisherMenus);
    }

    return items;
}

MenuItemList realtimeEffectMenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList items;
    items << makeRealtimeBuiltinEffectSubmenu(effects, effectMenu);
    items << makeNonBuiltinEffectSubmenus(effects, effectMenu);
    return items;
}

MenuItemList destructiveEffectMenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList items;
    items << makeDestructiveBuiltinEffectSubmenu(effects, effectMenu);
    items << makeNonBuiltinEffectSubmenus(effects, effectMenu);
    return items;
}

MenuItemList makeFlatList(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList items;
    AmbiguousTitleEntries otherFamilies;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            items << effectMenu.makeMenuEffectItem(meta.id);
        } else {
            otherFamilies[meta.title].insert(meta.id);
        }
    }
    if (!otherFamilies.empty()) {
        items << effectMenu.makeMenuSeparator();
        items << makeItemsOrDisambiguationSubmenus(otherFamilies, effectMenu);
    }
    return items;
}
} // namespace

muse::uicomponents::MenuItemList au::effects::utils::destructiveEffectMenu(EffectMenuOrganization organization,
                                                                           EffectMetaList effects,
                                                                           const EffectFilter& filter,
                                                                           IEffectMenuItemFactory& effectMenu)
{
    effects.erase(std::remove_if(effects.begin(), effects.end(), filter), effects.end());
    if (organization == EffectMenuOrganization::Flat) {
        return impl::makeFlatList(effects, effectMenu);
    } else {
        assert(organization == EffectMenuOrganization::Grouped);
        return impl::destructiveEffectMenu(effects, effectMenu);
    }
}

muse::uicomponents::MenuItemList au::effects::utils::realtimeEffectMenu(EffectMenuOrganization organization,
                                                                        EffectMetaList effects,
                                                                        const EffectFilter& filter,
                                                                        IEffectMenuItemFactory& effectMenu)
{
    effects.erase(std::remove_if(effects.begin(), effects.end(), filter), effects.end());
    if (organization == EffectMenuOrganization::Flat) {
        return impl::makeFlatList(effects, effectMenu);
    } else {
        assert(organization == EffectMenuOrganization::Grouped);
        return impl::realtimeEffectMenu(effects, effectMenu);
    }
}
