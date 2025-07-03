/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"
#include "log.h"

namespace impl {
using namespace muse;
using namespace muse::uicomponents;
using namespace au::effects;

// String with Case-Insensitive comparison
class CiString : public String
{
public:
    bool operator<(const CiString& other) const
    {
        const auto lhs = this->toStdString();
        const auto rhs = other.toStdString();
        return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                                            [](char a, char b) { return std::tolower(a) < std::tolower(b); });
    }
};

using CiStringSet = std::set<CiString>;
using EffectMetaSet = std::unordered_set<const EffectMeta*>;
using AmbiguousTitleEntries = std::map<CiString /*effect title*/, CiStringSet>;

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

MenuItem* makeEffectSubmenu(const String& title, const CiStringSet& effectIds, IEffectMenuItemFactory& effectMenu)
{
    MenuItemList subItems;
    for (const auto& effectId : effectIds) {
        subItems.push_back(effectMenu.makeMenuEffectItem(effectId));
    }
    return effectMenu.makeMenuEffect(title, subItems);
}

MenuItem* makeRealtimeBuiltinEffectSubmenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    CiStringSet ids;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            ids.insert(CiString { meta.id });
        }
    }
    return makeEffectSubmenu(muse::String { "Audacity" }, ids, effectMenu);
}

MenuItemList makeDestructiveBuiltinEffectSubmenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<CiString /*category*/, CiStringSet> categories;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            categories[CiString{ meta.category }].insert(CiString { meta.id });
        }
    }
    MenuItemList items;
    for (const auto& [category, effectIds] : categories) {
        items << makeEffectSubmenu(category, effectIds, effectMenu);
    }
    return items;
}

namespace {
constexpr const char* effectFamiliyString(EffectFamily family)
{
    switch (family) {
    case EffectFamily::Builtin: return "Audacity";
    case EffectFamily::VST3: return "VST3";
    case EffectFamily::LV2: return "LV2";
    case EffectFamily::AudioUnit: return "AudioUnit";
    default:
        assert(false);
        return "Unknown";
    }
}
} // namespace

MenuItemList makeNonBuiltinEffectSubmenus(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<CiString /*family*/, std::map<CiString /*publisher*/, std::map<CiString /*title*/, EffectMetaSet> > > families;

    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            // Built-in effects are handled separately
            continue;
        }
        families[CiString{ muse::String{ effectFamiliyString(meta.family) } }][CiString{ meta.vendor }][CiString{ meta.title }].insert(&meta);
    }

    MenuItemList items;

    if (families.empty()) {
        return items;
    }

    items << effectMenu.makeMenuSeparator();

    for (const auto& [family, publishers] : families) {
        MenuItemList publisherMenus;
        for (const auto& [publisher, titles] : publishers) {
            AmbiguousTitleEntries publisherEffects;
            for (const auto& [title, effectMetas] : titles) {
                for (const EffectMeta* meta : effectMetas) {
                    publisherEffects[CiString{ meta->title }].insert(CiString { meta->id });
                }
            }
            publisherMenus << effectMenu.makeMenuEffect(publisher, makeItemsOrDisambiguationSubmenus(publisherEffects, effectMenu));
        }
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
            otherFamilies[CiString{ meta.title }].insert(CiString { meta.id });
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
