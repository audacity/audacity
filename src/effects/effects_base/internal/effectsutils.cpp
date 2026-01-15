/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"
#include "effects/builtin/internal/builtineffectsrepository.h"
#include "log.h"

muse::String au::effects::utils::builtinEffectCategoryIdString(BuiltinEffectCategoryId category)
{
    switch (category) {
    case BuiltinEffectCategoryId::None:
        // Maybe a temporary solution to generator menu organization:
        // since generators don't have a category, with this they'll end up in the Audacity "category".
        // When we implement Nyquist support, though, either we create the
        // "Nyquist" category and are happy with that, or we
        // will have to ask our designers to specify organization of generators
        // (and analyzers).
        return muse::String{ "Audacity" };
    case BuiltinEffectCategoryId::VolumeAndCompression:
        return muse::String{ "Volume and compression" };
    case BuiltinEffectCategoryId::Fading:
        return muse::String{ "Fading" };
    case BuiltinEffectCategoryId::PitchAndTempo:
        return muse::String{ "Pitch and tempo" };
    case BuiltinEffectCategoryId::EqAndFilters:
        return muse::String{ "EQ and filters" };
    case BuiltinEffectCategoryId::NoiseRemovalAndRepair:
        return muse::String{ "Noise removal and repair" };
    case BuiltinEffectCategoryId::DelayAndReverb:
        return muse::String{ "Delay and reverb" };
    case BuiltinEffectCategoryId::DistortionAndModulation:
        return muse::String{ "Distortion and modulation" };
    case BuiltinEffectCategoryId::Special:
        return muse::String{ "Special" };
    case BuiltinEffectCategoryId::Legacy:
        return muse::String{ "Legacy" };
    default:
        assert(false);
        return muse::String{ "" };
    }
}

int au::effects::utils::builtinEffectCategoryIdOrder(const muse::String& category)
{
    using namespace au::effects::utils;
    static const std::map<muse::String, int> categoryOrder = {
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::VolumeAndCompression), 0 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::Fading), 1 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::PitchAndTempo), 2 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::EqAndFilters), 3 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::NoiseRemovalAndRepair), 4 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::DelayAndReverb), 5 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::DistortionAndModulation), 6 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::Special), 7 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::Legacy), 8 },
        { builtinEffectCategoryIdString(BuiltinEffectCategoryId::None), 9 },
    };
    auto it = categoryOrder.find(category);
    return it == categoryOrder.end() ? INT_MAX : it->second;
}

namespace impl {
using namespace muse;
using namespace muse::uicomponents;
using namespace au::effects;

// String with Case-Insensitive comparison
class CiString : public String
{
public:
    CiString() = default;
    CiString(const String& s)
        : String(s), m_cache(toLower(s)) {}

    bool operator<(const CiString& other) const noexcept
    {
        return m_cache < other.m_cache;
    }

private:
    std::string m_cache;

    static std::string toLower(const String& s)
    {
        auto u8 = s.toStdString();
        std::transform(u8.begin(), u8.end(), u8.begin(), [](unsigned char c){ return std::tolower(c); });
        return u8;
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
    std::map<muse::String /*category*/, CiStringSet> categories;
    for (const EffectMeta& meta : effects) {
        if (meta.family == EffectFamily::Builtin) {
            categories[meta.category].insert(CiString { meta.id });
        }
    }

    std::vector<std::pair<muse::String, CiStringSet> > categoriesSorted;
    for (const auto& [category, effectIds] : categories) {
        categoriesSorted.push_back({ category, effectIds });
    }
    std::sort(categoriesSorted.begin(), categoriesSorted.end(), [&](const auto& a, const auto& b) {
        return au::effects::utils::builtinEffectCategoryIdOrder(a.first) < au::effects::utils::builtinEffectCategoryIdOrder(b.first);
    });

    MenuItemList items;
    for (const auto& [category, effectIds] : categoriesSorted) {
        items << makeEffectSubmenu(CiString { category }, effectIds, effectMenu);
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
    case EffectFamily::Nyquist: return "Nyquist";
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
        families[CiString{ muse::String{ effectFamiliyString(meta.family) } }][CiString{ meta.vendor }][CiString{ meta.title }].insert(
            &meta);
    }

    MenuItemList items;

    if (families.empty()) {
        return items;
    }

    items << effectMenu.makeMenuSeparator();

    for (const auto& [family, publishers] : families) {
        MenuItemList publisherMenus;
        publisherMenus.reserve(publishers.size());

        for (const auto& [publisher, titles] : publishers) {
            AmbiguousTitleEntries publisherEffects;

            for (const auto& [title, effectMetas] : titles) {
                const CiString ciTitle{ title };
                auto& ids = publisherEffects[ciTitle];

                for (const EffectMeta* meta : effectMetas) {
                    ids.insert(CiString { meta->id });
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
