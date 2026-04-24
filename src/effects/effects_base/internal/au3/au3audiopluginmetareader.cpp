#include "au3audiopluginmetareader.h"
#include "effecterrors.h"
#include "../effectsutils.h"

#include "au3-components/PluginProvider.h"
#include "au3-strings/TranslatableString.h"
#include "au3-module-manager/ModuleManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "internal/au3/au3effectsutils.h"
#include "log.h"

namespace au::effects {
Au3AudioPluginMetaReader::Au3AudioPluginMetaReader(PluginProvider& provider)
    : m_pluginProvider{provider}
{
}

Au3AudioPluginMetaReader::~Au3AudioPluginMetaReader()
{
    IF_ASSERT_FAILED(m_terminated) {
        LOGW() << "Better call deinit() on module deinit";
        m_pluginProvider.Terminate();
    }
}

void Au3AudioPluginMetaReader::init()
{
    m_pluginProvider.Initialize();
    doInit();
    m_initialized = true;
}

void Au3AudioPluginMetaReader::deinit()
{
    m_pluginProvider.Terminate();
    m_terminated = true;
}

muse::RetVal<muse::audio::AudioResourceMetaList> Au3AudioPluginMetaReader::readMeta(const muse::io::path_t& pluginPath) const
{
    IF_ASSERT_FAILED(m_initialized && !m_terminated) {
        return make_ret(muse::Ret::Code::InternalError);
    }

    std::vector<PluginDescriptor> descriptors;
    ::PluginDescriptor empty;

    wxString wxPluginPath = au3::wxFromString(pluginPath.toString());
    muse::String errorStr;
    bool ok = true;
    try
    {
        TranslatableString errorMessage{};
        auto validator = m_pluginProvider.MakeValidator();
        auto numPlugins = m_pluginProvider.DiscoverPluginsAtPath(
            wxPluginPath, errorMessage, [&](PluginProvider* provider, ComponentInterface* ident) -> const PluginID&
        {
            const auto effect = dynamic_cast<const EffectDefinitionInterface*>(ident);
            IF_ASSERT_FAILED(effect) {
                return empty.GetID();
            }

            descriptors.push_back({});
            auto& desc = descriptors.back();
            desc.SetProviderID(ModuleManager::GetID(provider));
            desc.SetPluginType(PluginTypeEffect);
            desc.SetPath(ident->GetPath());
            desc.SetSymbol(ident->GetSymbol());
            desc.SetVendor(ident->GetVendor().Internal());
            desc.SetVersion(ident->GetVersion());
            desc.SetEffectFamily(provider->GetOptionalFamilySymbol().Internal());

            desc.SetID(effects::effectId(effect));
            desc.SetDescription(effect->GetDescription().Translation());
            desc.SetEffectType(effect->GetClassification());
            desc.SetEffectFamily(effect->GetFamily().Internal());
            desc.SetEffectGroup(effect->GetGroup());
            desc.SetEffectInteractive(effect->IsInteractive());
            desc.SetEffectDefault(effect->IsDefault());
            desc.SetRealtimeSupport(effect->RealtimeSupport());
            desc.SetEffectAutomatable(effect->SupportsAutomation());
            desc.SetParamsAreInputAgnostic(effect->ParamsAreInputAgnostic());

            desc.SetEnabled(true);
            desc.SetValid(true);

            return desc.GetID();
        });

        if (!errorMessage.empty()) {
            ok = false;
            errorStr = au3::wxToString(errorMessage.Debug());
        } else if (numPlugins == 0) {
            ok = false;
            errorStr = "no plugins found";
        }
    }
    catch (...)
    {
        ok = false;
        errorStr = "unknown error";
    }

    if (!ok) {
        LOGE() << "error: " << errorStr;
        return make_ret(muse::Ret::Code::InternalError); //! todo
    }

    if (std::none_of(descriptors.begin(), descriptors.end(),
                     [](const PluginDescriptor& desc) { return desc.IsValid(); })) {
        return effects::make_ret(effects::Err::EffectLoadFailed);
    }

    muse::audio::AudioResourceMetaList metaList;
    metaList.reserve(descriptors.size());
    for (const PluginDescriptor& desc : descriptors) {
        metaList.emplace_back(utils::auToMuseEffectMeta(toEffectMeta(desc)));
    }

    return muse::RetVal<muse::audio::AudioResourceMetaList>::make_ok(metaList);
}
}
