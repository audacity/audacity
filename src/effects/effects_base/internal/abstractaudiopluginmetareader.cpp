#include "abstractaudiopluginmetareader.h"
#include "effecterrors.h"

#include "libraries/lib-components/PluginProvider.h"
#include "libraries/lib-strings/TranslatableString.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/ModuleManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

namespace au::effects {
AbstractAudioPluginMetaReader::AbstractAudioPluginMetaReader(PluginProvider& provider)
    : m_pluginProvider{provider}
{
}

AbstractAudioPluginMetaReader::~AbstractAudioPluginMetaReader()
{
    IF_ASSERT_FAILED(m_terminated) {
        LOGW() << "Better call deinit() on module deinit";
        m_pluginProvider.Terminate();
    }
}

void AbstractAudioPluginMetaReader::init(const muse::IApplication::RunMode& mode)
{
    doInit(mode);
    m_initialized = true;
}

void AbstractAudioPluginMetaReader::doInit(const muse::IApplication::RunMode&)
{
    m_pluginProvider.Initialize();
}

void AbstractAudioPluginMetaReader::deinit()
{
    m_pluginProvider.Terminate();
    m_terminated = true;
}

muse::RetVal<muse::audio::AudioResourceMetaList> AbstractAudioPluginMetaReader::readMeta(const muse::io::path_t& pluginPath) const
{
    IF_ASSERT_FAILED(m_initialized && !m_terminated) {
        return make_ret(muse::Ret::Code::InternalError);
    }

    std::vector<PluginDescriptor> descriptors;

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
            //Workaround: use DefaultRegistrationCallback to create all descriptors for us
            //and then put a copy into result
            auto& id = PluginManager::DefaultRegistrationCallback(provider, ident);
            if (const auto ptr = PluginManager::Get().GetPlugin(id)) {
                auto desc = *ptr;
                try
                {
                    if (validator) {
                        validator->Validate(*ident);
                    }
                }
                catch (...)
                {
                    desc.SetEnabled(false);
                    desc.SetValid(false);
                }
                descriptors.emplace_back(std::move(desc));
            }
            return id;
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
    for (const PluginDescriptor& desc : descriptors) {
        //! NOTE At the moment AU only supports Fx and Fx|Generator,
        //! see VST3EffectBase::GetType
        muse::String type;
        if (desc.GetEffectType() == EffectType::EffectTypeProcess || desc.GetEffectType() == EffectType::EffectTypeGenerate) {
            type = u"Fx";
        } else {
            type = u"None";
        }

        muse::audio::AudioResourceMeta meta;
        meta.id = desc.GetID();
        meta.type = metaType();
        meta.attributes.emplace(muse::audio::CATEGORIES_ATTRIBUTE, type);
        meta.vendor = desc.GetVendor();
        meta.hasNativeEditorSupport = true;

        metaList.emplace_back(std::move(meta));
    }

    return muse::RetVal<muse::audio::AudioResourceMetaList>::make_ok(metaList);
}
}
