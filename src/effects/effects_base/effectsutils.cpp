/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"
#include "log.h"

namespace {
using namespace muse;
using namespace muse::uicomponents;
using namespace au::effects;

using EffectIdSet = std::set<EffectId>;

//! It can be that (different versions of) the same effect is installed in several places.
//! For those, we need a disambiguation submenu, whose items show the effect's path rather than title.
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

MenuItemList makeDisambiguatedSubmenus(const std::map<String, AmbiguousTitleEntries>& map, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList items;
    for (const auto& [family, entries] : map) {
        const MenuItemList subItems = makeItemsOrDisambiguationSubmenus(entries, effectMenu);
        items << effectMenu.makeMenuEffect(family, subItems);
    }
    return items;
}

muse::uicomponents::MenuItemList effectMenusByType(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    struct EffectTreeByType {
        //! Built-in effects are in the binary, there cannot be multiple installations - no need for disambiguation.
        EffectIdSet builtinEffects;
        std::map<String /*family*/, AmbiguousTitleEntries> otherFamilies;
    };

    EffectTreeByType tree;

    for (const EffectMeta& meta : effects) {
        switch (meta.family) {
        case EffectFamily::Builtin:
            tree.builtinEffects.insert(meta.id);
            break;
        case EffectFamily::VST3:
        {
            auto& familyMenu = tree.otherFamilies[muse::String{ "VST3" }];
            familyMenu[meta.title].insert(meta.id);
        }
        break;
        case EffectFamily::Unknown:
            assert(false);
            continue;
        }
    }

    MenuItemList items;

    if (!tree.builtinEffects.empty()) {
        items << makeEffectSubmenu(muse::String { "Audacity" }, tree.builtinEffects, effectMenu);
    }

    if (!tree.builtinEffects.empty() && !tree.otherFamilies.empty()) {
        items << effectMenu.makeMenuSeparator();
    }

    items << makeDisambiguatedSubmenus(tree.otherFamilies, effectMenu);

    return items;
}

muse::uicomponents::MenuItemList effectMenusByCategory(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    struct EffectTreeByCategory {
        //! Categorized effects are exclusively built-in effects, of which there cannot be multiple installations - no need for disambiguation.
        std::map<String /*category*/, EffectIdSet> categorizedEffects;
        std::map<String /*vendor*/, AmbiguousTitleEntries> vendoredEffects;
        //! Unmatched effects aren't nested in sub-menus.
        AmbiguousTitleEntries unmatchedEffects;
    };

    EffectTreeByCategory tree;

    for (const EffectMeta& meta : effects) {
        if (!meta.category.isEmpty()) {
            tree.categorizedEffects[meta.category].insert(meta.id);
        } else if (!meta.vendor.isEmpty()) {
            tree.vendoredEffects[meta.vendor][meta.title].insert(meta.id);
        } else {
            tree.unmatchedEffects[meta.title].insert(meta.id);
        }
    }

    MenuItemList items;

    for (const auto& [category, effectIds] : tree.categorizedEffects) {
        items << makeEffectSubmenu(category, effectIds, effectMenu);
    }

    if (!tree.categorizedEffects.empty() && !(tree.vendoredEffects.empty() && tree.unmatchedEffects.empty())) {
        items << effectMenu.makeMenuSeparator();
    }

    items << makeDisambiguatedSubmenus(tree.vendoredEffects, effectMenu);

    items << makeItemsOrDisambiguationSubmenus(tree.unmatchedEffects, effectMenu);

    return items;
}
}

muse::uicomponents::MenuItemList au::effects::utils::effectMenus(EffectMenuOrganization organization, EffectMetaList effects,
                                                                 const EffectFilter& filter,
                                                                 IEffectMenuItemFactory& effectMenu)
{
    effects.erase(std::remove_if(effects.begin(), effects.end(), filter), effects.end());

    switch (organization) {
    case EffectMenuOrganization::ByCategory:
        return effectMenusByCategory(effects, effectMenu);
    case EffectMenuOrganization::ByType:
        return effectMenusByType(effects, effectMenu);
    default:
        assert(false);
        return {};
    }
}
