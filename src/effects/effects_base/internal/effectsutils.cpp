/*
 * Audacity: A Digital Audio Editor
 */

#include "effectsutils.h"

#include <wx/string.h>

#include "framework/global/iglobalconfiguration.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/log.h"
#include "framework/global/stringutils.h"

#include "effectstypes.h"
#include "effects/vst/internal/vsttypes.h"
#include "effects/audio_unit/internal/audiounittypes.h"
#include "effects/lv2/internal/lv2types.h"
#include "effects/nyquist/internal/nyquisttypes.h"
#include "effects/builtin/internal/builtintypes.h"

#include "au3-components/EffectInterface.h"
#include "au3-strings/wxArrayStringEx.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include <algorithm>
#include <cctype>
#include <iterator>

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

std::string utils::effectFamilyToCacheType(EffectFamily family)
{
    switch (family) {
    case EffectFamily::VST3:      return std::string(vst::AUDIO_RESOURCE_TYPE_NAME);
#ifdef Q_OS_MACOS
    case EffectFamily::AudioUnit: return std::string(audio_unit::AUDIO_RESOURCE_TYPE_NAME);
#endif
#ifdef Q_OS_LINUX
    case EffectFamily::LV2:       return std::string(lv2::AUDIO_RESOURCE_TYPE_NAME);
#endif
    case EffectFamily::Nyquist:   return std::string(nyquist::AUDIO_RESOURCE_TYPE_NAME);
    case EffectFamily::Builtin:   return std::string(builtin::AUDIO_RESOURCE_TYPE_NAME);
    case EffectFamily::Unknown:
    case EffectFamily::_count:
        break;
    }
    return {};
}

EffectFamily utils::effectFamilyFromCacheType(const std::string& cacheType)
{
    if (cacheType == vst::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::VST3;
    }
#ifdef Q_OS_MACOS
    if (cacheType == audio_unit::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::AudioUnit;
    }
#endif
#ifdef Q_OS_LINUX
    if (cacheType == lv2::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::LV2;
    }
#endif
    if (cacheType == nyquist::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::Nyquist;
    }
    if (cacheType == builtin::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::Builtin;
    }
    return EffectFamily::Unknown;
}

bool utils::isFamilyType(const muse::audioplugins::PluginMeta& meta, EffectFamily family)
{
    return utils::effectFamilyFromCacheType(meta.type) == family;
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

EffectId utils::effectId(const EffectDefinitionInterface* effect)
{
    static muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;
    const muse::io::path_t rawPath = au3::wxToStdString(effect->GetPath());
    const muse::io::path_t portablePath = globalConfiguration()->isBundledWithApp(rawPath)
                                          ? globalConfiguration()->toBundledPath(rawPath)
                                          : rawPath;

    // Using wxJoin/wxSplit for now, as they handle the presence of the separator being part of any of its constituents
    // (e.g. an effect vendor that has an underscore in its name.)
    // TODO add defense in muse::strings utils.
    return au3::wxToString(wxJoin(wxArrayStringEx {
            wxString { "Effect" },
            effect->GetFamily().Internal(),
            effect->GetVendor().Internal(),
            effect->GetSymbol().Internal(),
            au3::wxFromStdString(portablePath.toStdString())
        }, '_'));
}

namespace {
constexpr size_t effectIdPartCount = 5;

std::string parseEffectIdPart(const effects::EffectId& effectId, size_t partIndex)
{
    const auto strings = wxSplit(au3::wxFromString(effectId), '_');
    if (strings.size() != effectIdPartCount) {
        LOGW() << "Unexpected effect ID format: " << effectId;
    }
    if (partIndex < strings.size()) {
        return au3::wxToStdString(strings[partIndex]);
    }
    return {};
}

std::string toLower(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(), [](unsigned char ch) {
        return static_cast<char>(std::tolower(ch));
    });
    return value;
}

std::string vst3ClassId(const EffectId& effectId)
{
    const std::string path = utils::parseEffectPath(effectId);
    const auto separator = path.rfind(';');
    if (separator == std::string::npos || separator + 1 == path.size()) {
        return {};
    }
    return toLower(path.substr(separator + 1));
}
}

bool utils::isEffectId(const EffectId& effectId)
{
    const auto strings = wxSplit(au3::wxFromString(effectId), '_');
    return strings.size() == effectIdPartCount && strings[0] == wxString("Effect");
}

EffectId utils::findRelocatedVst3EffectId(const EffectId& effectId, const EffectMetaList& metaList)
{
    if (utils::parseEffectFamily(effectId) != "VST3") {
        return {};
    }

    const std::string classId = vst3ClassId(effectId);
    if (classId.empty()) {
        return {};
    }

    const auto exact = std::find_if(metaList.begin(), metaList.end(), [&](const EffectMeta& meta) {
        return meta.family == EffectFamily::VST3
               && meta.isLoadable()
               && meta.id == effectId;
    });
    if (exact != metaList.end()) {
        return {};
    }

    EffectMetaList candidates;
    std::copy_if(metaList.begin(), metaList.end(), std::back_inserter(candidates), [&](const EffectMeta& meta) {
        return meta.family == EffectFamily::VST3
               && meta.isLoadable()
               && meta.id != effectId
               && vst3ClassId(meta.id) == classId;
    });

    if (candidates.size() == 1) {
        return candidates.front().id;
    }

    const std::string vendor = utils::parseEffectVendor(effectId);
    const std::string name = utils::parseEffectName(effectId);

    candidates.erase(std::remove_if(candidates.begin(), candidates.end(), [&](const EffectMeta& meta) {
        return utils::parseEffectVendor(meta.id) != vendor
               || utils::parseEffectName(meta.id) != name;
    }), candidates.end());

    return candidates.size() == 1 ? candidates.front().id : EffectId {};
}

std::string utils::parseEffectName(const EffectId& effectId)
{
    return parseEffectIdPart(effectId, 3);
}

std::string utils::parseEffectVendor(const EffectId& effectId)
{
    return parseEffectIdPart(effectId, 2);
}

std::string utils::parseEffectFamily(const EffectId& effectId)
{
    return parseEffectIdPart(effectId, 1);
}

std::string utils::parseEffectPath(const EffectId& effectId)
{
    return parseEffectIdPart(effectId, 4);
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

muse::audioplugins::PluginMeta utils::auToMuseEffectMeta(const EffectMeta& meta)
{
    muse::audioplugins::PluginMeta museMeta;
    // Use the AU3 plugin ID (same as Audio Unit does)
    // This is necessary for looking up the plugin in PluginManager later
    museMeta.id = meta.id.toStdString();
    museMeta.type = utils::effectFamilyToCacheType(meta.family);
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
T attributeValue(const muse::audioplugins::PluginMeta& meta, const muse::String& key, bool isLoadable)
{
    const auto valStr = meta.attributeVal(key);
    IF_ASSERT_FAILED(!isLoadable || !valStr.empty()) {
        return {};
    }
    return value<T>(valStr);
}
}

EffectMeta utils::museToAuEffectMeta(const muse::io::path_t& path, const muse::audioplugins::PluginMeta& meta,
                                     muse::audioplugins::AudioPluginState state)
{
    // only Validated entries are guaranteed complete meta; gate the asserts on that
    const bool isLoadable = (state == muse::audioplugins::AudioPluginState::Validated);

    EffectMeta effectMeta;
    effectMeta.path = path;
    effectMeta.id = muse::String::fromStdString(meta.id);
    effectMeta.family = utils::effectFamilyFromCacheType(meta.type);
    effectMeta.vendor = muse::String::fromStdString(meta.vendor);
    effectMeta.type = utils::effectTypeFromString(attributeValue(meta, EFFECT_TYPE_ATTRIBUTE, isLoadable));
    effectMeta.title = attributeValue(meta, EFFECT_TITLE_ATTRIBUTE, isLoadable);
    effectMeta.description = effectMeta.title; // TODO use attributeValue(meta, EFFECT_DESCRIPTION_ATTRIBUTE, isLoadable);
    effectMeta.category = attributeValue(meta, EFFECT_CATEGORY_ATTRIBUTE, isLoadable);
    effectMeta.isRealtimeCapable = attributeValue<bool>(meta, EFFECT_IS_REALTIME_CAPABLE_ATTRIBUTE, isLoadable);
    effectMeta.paramsAreInputAgnostic = attributeValue<bool>(meta, EFFECT_PARAMS_ARE_INPUT_AGNOSTIC_ATTRIBUTE, isLoadable);
    effectMeta.version = attributeValue(meta, EFFECT_VERSION_ATTRIBUTE, isLoadable);
    effectMeta.module = attributeValue(meta, EFFECT_MODULE_ATTRIBUTE, isLoadable);
    effectMeta.isActivated = attributeValue<bool>(meta, EFFECT_ACTIVATED_ATTRIBUTE, isLoadable);
    effectMeta.state = state;

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
        return !meta.isLoadable() || !meta.isActivated || filter(meta);
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
