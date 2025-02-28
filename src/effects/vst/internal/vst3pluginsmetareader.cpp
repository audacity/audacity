/*
* Audacity: A Digital Audio Editor
*/

#include "vst3pluginsmetareader.h"

#include "libraries/lib-components/PluginProvider.h"
#include "libraries/lib-strings/TranslatableString.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/ModuleManager.h"
#include "libraries/lib-vst3/VST3EffectsModule.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;
using namespace muse;
using namespace muse::audio;

bool Vst3PluginsMetaReader::canReadMeta(const io::path_t&) const
{
    return true;
}

RetVal<AudioResourceMetaList> Vst3PluginsMetaReader::readMeta(const io::path_t& pluginPath) const
{
    std::vector<PluginDescriptor> descriptors;

    wxString wxPluginPath = au3::wxFromString(pluginPath.toString());
    bool error = false;
    String errorStr;

    VST3EffectsModule vst3Module;

    try
    {
        TranslatableString errorMessage{};
        auto validator = vst3Module.MakeValidator();
        auto numPlugins = vst3Module.DiscoverPluginsAtPath(
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
            error = true;
            errorStr = au3::wxToString(errorMessage.Debug());
        } else if (numPlugins == 0) {
            error = true;
            errorStr = "no plugins found";
        }
    }
    catch (...)
    {
        error = true;
        errorStr = "unknown error";
    }

    if (error) {
        LOGE() << "error: " << errorStr;
        return make_ret(Ret::Code::InternalError); //! todo
    }

    AudioResourceMetaList metaList;
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
        meta.type = muse::audio::AudioResourceType::VstPlugin;
        meta.attributes.emplace(muse::audio::CATEGORIES_ATTRIBUTE, type);
        meta.vendor = desc.GetVendor();
        meta.hasNativeEditorSupport = true;

        metaList.emplace_back(std::move(meta));
    }

    return RetVal<AudioResourceMetaList>::make_ok(metaList);
}
