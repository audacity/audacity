/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"
#include "effectstypes.h"
#include "framework/global/log.h"

namespace au::effects {
muse::String utils::effectFamilyToString(EffectFamily family)
{
    switch (family) {
    case EffectFamily::Builtin: return u"Audacity";
    case EffectFamily::VST3: return u"VST3";
#ifdef Q_OS_LINUX
    case EffectFamily::LV2: return u"LV2";
#endif
#ifdef Q_OS_MACOS
    case EffectFamily::AudioUnit: return u"AudioUnit";
#endif
    case EffectFamily::Nyquist: return u"Nyquist";
    default:
        assert(false);
        return u"Unknown";
    }
}

EffectFamily utils::effectFamilyFromString(const muse::String& family)
{
    if (family == u"Audacity") {
        return EffectFamily::Builtin;
    } else if (family == u"VST3") {
        return EffectFamily::VST3;
#ifdef Q_OS_LINUX
    } else if (family == u"LV2") {
        return EffectFamily::LV2;
#endif
#ifdef Q_OS_MACOS
    } else if (family == u"AudioUnit") {
        return EffectFamily::AudioUnit;
#endif
    } else if (family == u"Nyquist") {
        return EffectFamily::Nyquist;
    }
    assert(false);
    return EffectFamily::Unknown;
}

namespace {
static const muse::String unspecifiedEffectCategoryString{ "Third-party" };
static const muse::String noneEffectCategoryString{ "None" };
static const muse::String volumeAndCompressionEffectCategoryString{ "Volume and compression" };
static const muse::String fadingEffectCategoryString{ "Fading" };
static const muse::String pitchAndTempoEffectCategoryString{ "Pitch and tempo" };
static const muse::String eqAndFiltersEffectCategoryString{ "EQ and filters" };
static const muse::String noiseRemovalAndRepairEffectCategoryString{ "Noise removal and repair" };
static const muse::String delayAndReverbEffectCategoryString{ "Delay and reverb" };
static const muse::String distortionAndModulationEffectCategoryString{ "Distortion and modulation" };
static const muse::String specialEffectCategoryString{ "Special" };
static const muse::String spectralToolsEffectCategoryString{ "Spectral tools" };
static const muse::String legacyEffectCategoryString{ "Legacy" };
}

muse::String utils::effectCategoryToString(EffectCategory category)
{
    switch (category) {
    case EffectCategory::Unspecified:
        return unspecifiedEffectCategoryString;
    case EffectCategory::None:
        return noneEffectCategoryString;
    case EffectCategory::VolumeAndCompression:
        return volumeAndCompressionEffectCategoryString;
    case EffectCategory::Fading:
        return fadingEffectCategoryString;
    case EffectCategory::PitchAndTempo:
        return pitchAndTempoEffectCategoryString;
    case EffectCategory::EqAndFilters:
        return eqAndFiltersEffectCategoryString;
    case EffectCategory::NoiseRemovalAndRepair:
        return noiseRemovalAndRepairEffectCategoryString;
    case EffectCategory::DelayAndReverb:
        return delayAndReverbEffectCategoryString;
    case EffectCategory::DistortionAndModulation:
        return distortionAndModulationEffectCategoryString;
    case EffectCategory::Special:
        return specialEffectCategoryString;
    case EffectCategory::SpectralTools:
        return spectralToolsEffectCategoryString;
    case EffectCategory::Legacy:
        return legacyEffectCategoryString;
    default:
        assert(false);
        return noneEffectCategoryString;
    }
}

EffectCategory utils::effectCategoryFromString(const muse::String& category)
{
    if (category == volumeAndCompressionEffectCategoryString) {
        return EffectCategory::VolumeAndCompression;
    } else if (category == fadingEffectCategoryString) {
        return EffectCategory::Fading;
    } else if (category == pitchAndTempoEffectCategoryString) {
        return EffectCategory::PitchAndTempo;
    } else if (category == eqAndFiltersEffectCategoryString) {
        return EffectCategory::EqAndFilters;
    } else if (category == noiseRemovalAndRepairEffectCategoryString) {
        return EffectCategory::NoiseRemovalAndRepair;
    } else if (category == delayAndReverbEffectCategoryString) {
        return EffectCategory::DelayAndReverb;
    } else if (category == distortionAndModulationEffectCategoryString) {
        return EffectCategory::DistortionAndModulation;
    } else if (category == specialEffectCategoryString) {
        return EffectCategory::Special;
    } else if (category == spectralToolsEffectCategoryString) {
        return EffectCategory::SpectralTools;
    } else if (category == legacyEffectCategoryString) {
        return EffectCategory::Legacy;
    } else if (category == noneEffectCategoryString) {
        return EffectCategory::None;
    } else if (category == unspecifiedEffectCategoryString) {
        return EffectCategory::Unspecified;
    }
    assert(false);
    return EffectCategory::Unspecified;
}

muse::String utils::effectTypeToString(EffectType type)
{
    switch (type) {
    case EffectType::Unknown: return u"Unknown";
    case EffectType::Analyzer: return u"Analyzer";
    case EffectType::Generator: return u"Generator";
    case EffectType::Processor: return u"Effect";
    case EffectType::Tool: return u"Tool";
    default:
        assert(false);
        return u"Unknown";
    }
}

EffectType utils::effectTypeFromString(const muse::String& type)
{
    if (type == "Analyzer") {
        return EffectType::Analyzer;
    } else if (type == "Generator") {
        return EffectType::Generator;
    } else if (type == "Effect") {
        return EffectType::Processor;
    } else if (type == "Tool") {
        return EffectType::Tool;
    } else if (type.isEmpty() || type == "Unknown") {
        return EffectType::Unknown;
    }
    assert(false);
    return EffectType::Unknown;
}

muse::audio::AudioResourceMeta utils::auToMuseEffectMeta(const EffectMeta& meta)
{
    muse::audio::AudioResourceMeta museMeta;
    // Use the AU3 plugin ID (same as Audio Unit does)
    // This is necessary for looking up the plugin in PluginManager later
    museMeta.id = meta.id.toStdString();
    museMeta.type = toMuseAudioResourceType(meta.family);
    museMeta.vendor = meta.vendor.toStdString();

    // Add attributes using the map interface
    museMeta.attributes.emplace(EFFECT_CATEGORY_ATTRIBUTE, meta.category);
    // Store the plugin name (from $name directive) for display
    museMeta.attributes.emplace(EFFECT_TITLE_ATTRIBUTE, meta.title);
    museMeta.attributes.emplace(EFFECT_DESCRIPTION_ATTRIBUTE, meta.description);
    museMeta.attributes.emplace(EFFECT_PARAMS_ARE_INPUT_AGNOSTIC_ATTRIBUTE, meta.paramsAreInputAgnostic ? u"true" : u"false");
    museMeta.attributes.emplace(EFFECT_IS_REALTIME_CAPABLE_ATTRIBUTE, meta.isRealtimeCapable ? u"true" : u"false");
    museMeta.attributes.emplace(EFFECT_TYPE_ATTRIBUTE, utils::effectTypeToString(meta.type));
    museMeta.attributes.emplace(EFFECT_VERSION_ATTRIBUTE, meta.version);
    museMeta.attributes.emplace(EFFECT_MODULE_ATTRIBUTE, meta.module);
    museMeta.attributes.emplace(EFFECT_ACTIVATED_ATTRIBUTE, meta.isActivated ? u"true" : u"false");

    return museMeta;
}

namespace {
template<typename T>
T value(const muse::String& str)
{
    return str;
}

template<>
bool value<bool>(const muse::String& str)
{
    return str == u"true";
}

template<typename T = muse::String>
T attributeValue(const muse::audio::AudioResourceMeta& meta, const muse::String& key, bool enabled)
{
    const auto valStr = meta.attributeVal(key);
    IF_ASSERT_FAILED(!enabled || !valStr.empty()) {
        return {};
    }
    return value<T>(valStr);
}
}

EffectMeta utils::museToAuEffectMeta(const muse::io::path_t& path, const muse::audio::AudioResourceMeta& meta, bool enabled)
{
    EffectMeta effectMeta;
    effectMeta.path = path;
    effectMeta.id = muse::String::fromStdString(meta.id);
    effectMeta.family = fromMuseAudioResourceType(meta.type);
    effectMeta.vendor = muse::String::fromStdString(meta.vendor);
    effectMeta.type = utils::effectTypeFromString(attributeValue(meta, EFFECT_TYPE_ATTRIBUTE, enabled));
    effectMeta.title = attributeValue(meta, EFFECT_TITLE_ATTRIBUTE, enabled);
    effectMeta.description = effectMeta.title; // TODO use attributeValue(meta, EFFECT_DESCRIPTION_ATTRIBUTE, enabled);
    effectMeta.category = attributeValue(meta, EFFECT_CATEGORY_ATTRIBUTE, enabled);
    effectMeta.isRealtimeCapable = attributeValue<bool>(meta, EFFECT_IS_REALTIME_CAPABLE_ATTRIBUTE, enabled);
    effectMeta.paramsAreInputAgnostic = attributeValue<bool>(meta, EFFECT_PARAMS_ARE_INPUT_AGNOSTIC_ATTRIBUTE, enabled);
    effectMeta.version = attributeValue(meta, EFFECT_VERSION_ATTRIBUTE, enabled);
    effectMeta.module = attributeValue(meta, EFFECT_MODULE_ATTRIBUTE, enabled);
    effectMeta.isActivated = attributeValue<bool>(meta, EFFECT_ACTIVATED_ATTRIBUTE, enabled);
    effectMeta.isLoadable = enabled;

    return effectMeta;
}

namespace impl {
using namespace muse;
using namespace muse::uicomponents;
using namespace au::effects;

int builtinEffectCategoryIdOrder(const String& category)
{
    using namespace utils;
    static const std::map<String, int> categoryOrder = {
        { effectCategoryToString(EffectCategory::None), 0 },
        { effectCategoryToString(EffectCategory::VolumeAndCompression), 1 },
        { effectCategoryToString(EffectCategory::Fading), 2 },
        { effectCategoryToString(EffectCategory::PitchAndTempo), 3 },
        { effectCategoryToString(EffectCategory::EqAndFilters), 4 },
        { effectCategoryToString(EffectCategory::NoiseRemovalAndRepair), 5 },
        { effectCategoryToString(EffectCategory::DelayAndReverb), 6 },
        { effectCategoryToString(EffectCategory::DistortionAndModulation), 7 },
        { effectCategoryToString(EffectCategory::Special), 8 },
        { effectCategoryToString(EffectCategory::SpectralTools), 9 },
        { effectCategoryToString(EffectCategory::Legacy), 10 },
        { effectCategoryToString(EffectCategory::Unspecified), 11 },
    };
    auto it = categoryOrder.find(category);
    return it == categoryOrder.end() ? INT_MAX : it->second;
}

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

bool shippedByAudacity(const EffectMeta& meta)
{
    return meta.family == EffectFamily::Builtin
           || (meta.family == EffectFamily::Nyquist
               && meta.category != utils::effectCategoryToString(EffectCategory::Unspecified));
}

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

MenuItemList audacityDestructiveEffectsGroup(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<muse::String /*category*/, CiStringSet> categories;
    for (const EffectMeta& meta : effects) {
        categories[meta.category].insert(CiString { meta.id });
    }

    std::vector<std::pair<muse::String, CiStringSet> > categoriesSorted;
    for (const auto& [category, effectIds] : categories) {
        categoriesSorted.push_back({ category, effectIds });
    }
    std::sort(categoriesSorted.begin(), categoriesSorted.end(), [&](const auto& a, const auto& b) {
        return impl::builtinEffectCategoryIdOrder(a.first) < impl::builtinEffectCategoryIdOrder(b.first);
    });

    MenuItemList items;
    for (const auto& [category, effectIds] : categoriesSorted) {
        if (category == noneEffectCategoryString) {
            for (const auto& effectId : effectIds) {
                items << effectMenu.makeMenuEffectItem(effectId);
            }
        } else {
            items << makeEffectSubmenu(CiString { category }, effectIds, effectMenu);
        }
    }
    return items;
}

MenuItemList thirdPartyGroup(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    std::map<CiString /*family*/, std::map<CiString /*publisher*/, std::map<CiString /*title*/, EffectMetaSet> > > families;

    for (const EffectMeta& meta : effects) {
        families[CiString{ utils::effectFamilyToString(meta.family) }][CiString{ meta.vendor }][CiString{ meta.title }].insert(&meta);
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

struct EffectGroups {
    EffectMetaList audacityEffects;
    EffectMetaList thirdPartyEffects;
};

EffectGroups splitEffects(const EffectMetaList& effects)
{
    EffectGroups groups;
    for (const EffectMeta& meta : effects) {
        if (shippedByAudacity(meta)) {
            groups.audacityEffects.push_back(meta);
        } else {
            groups.thirdPartyEffects.push_back(meta);
        }
    }
    return groups;
}

MenuItemList realtimeEffectMenu(const EffectMetaList& effects, IEffectMenuItemFactory& effectMenu)
{
    const EffectGroups groups = splitEffects(effects);
    MenuItemList items;
    items << makeRealtimeBuiltinEffectSubmenu(groups.audacityEffects, effectMenu);
    items << thirdPartyGroup(groups.thirdPartyEffects, effectMenu);
    return items;
}

MenuItemList destructiveEffectMenu(EffectMetaList effects, IEffectMenuItemFactory& effectMenu)
{
    const EffectGroups groups = splitEffects(effects);
    MenuItemList items;
    items << audacityDestructiveEffectsGroup(groups.audacityEffects, effectMenu);
    items << thirdPartyGroup(groups.thirdPartyEffects, effectMenu);
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

void removeAlsoDisabledEffects(EffectMetaList& effects, const utils::EffectFilter& filter)
{
    effects.erase(std::remove_if(effects.begin(), effects.end(), [&](const EffectMeta& meta) {
        return !meta.isLoadable || !meta.isActivated || filter(meta);
    }), effects.end());
}
} // namespace

muse::uicomponents::MenuItemList utils::destructiveEffectMenu(EffectMenuOrganization organization,
                                                              EffectMetaList effects,
                                                              const EffectFilter& filter,
                                                              IEffectMenuItemFactory& effectMenu)
{
    impl::removeAlsoDisabledEffects(effects, filter);
    if (organization == EffectMenuOrganization::Flat) {
        return impl::makeFlatList(effects, effectMenu);
    } else {
        assert(organization == EffectMenuOrganization::Grouped);
        return impl::destructiveEffectMenu(effects, effectMenu);
    }
}

muse::uicomponents::MenuItemList utils::realtimeEffectMenu(EffectMenuOrganization organization,
                                                           EffectMetaList effects,
                                                           const EffectFilter& filter,
                                                           IEffectMenuItemFactory& effectMenu)
{
    impl::removeAlsoDisabledEffects(effects, filter);
    if (organization == EffectMenuOrganization::Flat) {
        return impl::makeFlatList(effects, effectMenu);
    } else {
        assert(organization == EffectMenuOrganization::Grouped);
        return impl::realtimeEffectMenu(effects, effectMenu);
    }
}
}
