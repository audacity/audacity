#include "au3audiopluginmetareader.h"
#include "effecterrors.h"
#include "../effectsutils.h"

#include "au3-components/PluginProvider.h"
#include "au3-strings/TranslatableString.h"
#include "au3-module-manager/PluginManager.h"
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

void Au3AudioPluginMetaReader::init(const muse::IApplication::RunMode& mode)
{
    doInit(mode);
    m_initialized = true;
}

void Au3AudioPluginMetaReader::doInit(const muse::IApplication::RunMode&)
{
    m_pluginProvider.Initialize();
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
        // For now - we will overwrite `museMeta.type` in the next conversion step
        constexpr auto effectFamily = EffectFamily::Unknown;

        const auto title = muse::String::fromStdString(desc.GetSymbol().Msgid().MSGID().GET().ToStdString());

        // TODO in a follow-up commit will be a property of the AU3 effect type, then we could get the actual value.
        constexpr auto supportsMultipleClipSelection = false;

        const auto auMeta = toEffectMeta(desc, effectFamily, title, title, supportsMultipleClipSelection);
        auto museMeta = utils::auToMuseEffectMeta(auMeta);
        museMeta.type = this->metaType();

        metaList.emplace_back(std::move(museMeta));
    }

    return muse::RetVal<muse::audio::AudioResourceMetaList>::make_ok(metaList);
}
}
