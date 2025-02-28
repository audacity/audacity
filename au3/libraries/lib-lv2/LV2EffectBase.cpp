/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2EffectBase.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/
#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2EffectBase.h"
#include "LV2Instance.h"
#include "LV2Wrapper.h"
#include "ConfigInterface.h"

LV2EffectBase::LV2EffectBase(const LilvPlugin& plug)
    : mPlug{plug}
{
}

LV2EffectBase::~LV2EffectBase() = default;

// ============================================================================
// ComponentInterface Implementation
// ============================================================================

PluginPath LV2EffectBase::GetPath() const
{
    return LilvString(lilv_plugin_get_uri(&mPlug));
}

ComponentInterfaceSymbol LV2EffectBase::GetSymbol() const
{
    return LV2FeaturesList::GetPluginSymbol(mPlug);
}

VendorSymbol LV2EffectBase::GetVendor() const
{
    wxString vendor = LilvStringMove(lilv_plugin_get_author_name(&mPlug));

    if (vendor.empty()) {
        return XO("n/a");
    }

    return { vendor };
}

wxString LV2EffectBase::GetVersion() const
{
    return wxT("1.0");
}

TranslatableString LV2EffectBase::GetDescription() const
{
    return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType LV2EffectBase::GetType() const
{
    if (mPorts.mAudioIn == 0 && mPorts.mAudioOut == 0) {
        return EffectTypeTool;
    }

    if (mPorts.mAudioIn == 0) {
        return EffectTypeGenerate;
    }

    if (mPorts.mAudioOut == 0) {
        return EffectTypeAnalyze;
    }

    return EffectTypeProcess;
}

EffectFamilySymbol LV2EffectBase::GetFamily() const
{
    return LV2EFFECTS_FAMILY;
}

bool LV2EffectBase::IsInteractive() const
{
    return mPorts.mControlPorts.size() != 0;
}

bool LV2EffectBase::IsDefault() const
{
    return false;
}

auto LV2EffectBase::RealtimeSupport() const -> RealtimeSince
{
    // TODO reenable after achieving statelessness
    return GetType() == EffectTypeProcess
           ? RealtimeSince::Always
           : RealtimeSince::Never;
}

bool LV2EffectBase::SupportsAutomation() const
{
    return true;
}

bool LV2EffectBase::InitializePlugin()
{
    if (!mFeatures.mOk) {
        return false;
    }

    // Do a check only on temporary feature list objects
    auto instanceFeatures = LV2InstanceFeaturesList{ mFeatures };
    if (!instanceFeatures.mOk) {
        return false;
    }
    if (!LV2InstanceFeaturesList::ValidatePlugin::Call(mPlug, instanceFeatures)) {
        return false;
    }

    // Determine available extensions
    mWantsOptionsInterface = false;
    mWantsStateInterface = false;
    if (LilvNodesPtr extdata{ lilv_plugin_get_extension_data(&mPlug) }) {
        LILV_FOREACH(nodes, i, extdata.get()) {
            const auto node = lilv_nodes_get(extdata.get(), i);
            const auto uri = lilv_node_as_string(node);
            if (strcmp(uri, LV2_OPTIONS__interface) == 0) {
                mWantsOptionsInterface = true;
            } else if (strcmp(uri, LV2_STATE__interface) == 0) {
                mWantsStateInterface = true;
            }
        }
    }

    return true;
}

EffectSettings LV2EffectBase::MakeSettings() const
{
    auto result = EffectSettings::Make<LV2EffectSettings>();
    auto& settings = GetSettings(result);
    // This may waste a bit of space on output ports, but not likely much
    settings.values.reserve(mPorts.mControlPorts.size());
    for (auto& controlPort : mPorts.mControlPorts) {
        auto& value = settings.values.emplace_back();
        value = controlPort->mDef;
    }
    return result;
}

bool LV2EffectBase::CopySettingsContents(
    const EffectSettings& src, EffectSettings& dst) const
{
    auto& srcControls = GetSettings(src).values;
    auto& dstControls = GetSettings(dst).values;

    const auto& controlPorts = mPorts.mControlPorts;
    const auto portsCount = controlPorts.size();
    // Do not use the copy constructor of std::vector.  Do an in-place rewrite
    // of the destination vector, which will not allocate memory if dstControls
    // began with sufficient capacity.
    // And that will be true if dstControls originated with MakeSettings() or a
    // copy of it, because the set of control ports does not vary after
    // initialization of the plug-in.
    assert(srcControls.size() == portsCount);
    assert(dstControls.size() == portsCount);
    // But let's be sure
    const auto portValuesCount
        =std::min(srcControls.size(), dstControls.size());

    if (portValuesCount != portsCount) {
        return false;
    }

    size_t portIndex {};

    for (auto& port : controlPorts) {
        if (port->mIsInput) {
            dstControls[portIndex] = srcControls[portIndex];
        }

        ++portIndex;
    }

    // Ignore mpState

    return true;
}

auto LV2EffectBase::MakeOutputs() const -> std::unique_ptr<EffectOutputs>
{
    auto result = std::make_unique<LV2EffectOutputs>();
    auto& values = result->values;
    // This may waste a bit of space on input ports, but not likely much
    values.resize(mPorts.mControlPorts.size());
    return result;
}

std::shared_ptr<EffectInstance> LV2EffectBase::MakeInstance() const
{
    auto result = std::make_shared<LV2Instance>(*this, mFeatures, mPorts);
    if (result->IsOk()) {
        return result;
    }
    return nullptr;
}

bool LV2EffectBase::SaveSettings(
    const EffectSettings& settings, CommandParameters& parms) const
{
    auto& values = GetSettings(settings).values;
    size_t index = 0;
    for (auto& port : mPorts.mControlPorts) {
        if (port->mIsInput) {
            if (!parms.Write(port->mName, values[index])) {
                return false;
            }
        }
        ++index;
    }
    return true;
}

bool LV2EffectBase::LoadSettings(
    const CommandParameters& parms, EffectSettings& settings) const
{
    // First pass validates values
    for (auto& port : mPorts.mControlPorts) {
        if (port->mIsInput) {
            double d = 0.0;
            if (!parms.Read(port->mName, &d)) {
                return false;
            }
            // Use unscaled range here
            if (d < port->mMin || d > port->mMax) {
                return false;
            }
        }
    }

    // Second pass actually sets the values
    auto& values = GetSettings(settings).values;
    size_t index = 0;
    for (auto& port : mPorts.mControlPorts) {
        if (port->mIsInput) {
            double d = 0.0;
            if (!parms.Read(port->mName, &d)) {
                return false;
            }
            values[index] = d;
        }
        ++index;
    }

    return true;
}

OptionalMessage LV2EffectBase::LoadUserPreset(
    const RegistryPath& name, EffectSettings& settings) const
{
    return LoadParameters(name, settings);
}

bool LV2EffectBase::SaveUserPreset(
    const RegistryPath& name, const EffectSettings& settings) const
{
    return SaveParameters(name, settings);
}

RegistryPaths LV2EffectBase::GetFactoryPresets() const
{
    using namespace LV2Symbols;
    if (mFactoryPresetsLoaded) {
        return mFactoryPresetNames;
    }

    if (LilvNodesPtr presets{ lilv_plugin_get_related(&mPlug, node_Preset) }) {
        LILV_FOREACH(nodes, i, presets.get()) {
            const auto preset = lilv_nodes_get(presets.get(), i);

            mFactoryPresetUris.push_back(LilvString(preset));

            lilv_world_load_resource(gWorld, preset);

            if (LilvNodesPtr labels{ lilv_world_find_nodes(gWorld, preset,
                                                           node_Label, nullptr) }) {
                const auto label = lilv_nodes_get_first(labels.get());
                mFactoryPresetNames.push_back(LilvString(label));
            } else {
                mFactoryPresetNames.push_back(
                    LilvString(preset).AfterLast(wxT('#')));
            }
        }
    }

    mFactoryPresetsLoaded = true;

    return mFactoryPresetNames;
}

OptionalMessage
LV2EffectBase::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    using namespace LV2Symbols;
    if (id < 0 || id >= (int)mFactoryPresetUris.size()) {
        return {}
    }

    LilvNodePtr preset{ lilv_new_uri(gWorld, mFactoryPresetUris[id].ToUTF8()) };
    if (!preset) {
        return {}
    }

    using LilvStatePtr = Lilv_ptr<LilvState, lilv_state_free>;
    LilvStatePtr state{
        lilv_state_new_from_world(gWorld,
                                  mFeatures.URIDMapFeature(), preset.get())
    };
    if (!state) {
        return {}
    }

    auto& mySettings = GetSettings(settings);
    mPorts.EmitPortValues(*state, mySettings);
    // Save the state, for whatever might not be contained in port values
    mySettings.mpState = move(state);
    return { nullptr };
}

bool LV2EffectBase::CanExportPresets() const
{
    return false;
}

bool LV2EffectBase::HasOptions() const
{
    return true;
}

// ============================================================================
// LV2Effect Implementation
// ============================================================================

OptionalMessage LV2EffectBase::LoadParameters(
    const RegistryPath& group, EffectSettings& settings) const
{
    wxString parms;
    if (!GetConfig(*this,
                   PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString)) {
        return {}
    }
    CommandParameters eap;
    if (!eap.SetParameters(parms)) {
        return {}
    }
    if (!LoadSettings(eap, settings)) {
        return {}
    }
    return { nullptr };
}

bool LV2EffectBase::SaveParameters(
    const RegistryPath& group, const EffectSettings& settings) const
{
    // PRL: This function just dumps the several control port values to the
    // config files.  Should it be reimplemented with
    // lilv_state_new_from_instance to capture -- I don't know what -- other
    // important state?

    CommandParameters eap;
    if (!SaveSettings(settings, eap)) {
        return false;
    }

    wxString parms;
    if (!eap.GetParameters(parms)) {
        return false;
    }

    return SetConfig(*this,
                     PluginSettings::Private, group, wxT("Parameters"), parms);
}

#endif
