/*
* Audacity: A Digital Audio Editor
*/

#include "nyquistpluginsmetareader.h"

#include "au3-module-manager/PluginManager.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "framework/global/log.h"

using namespace au::effects;
using namespace muse;

NyquistPluginsMetaReader::NyquistPluginsMetaReader()
    : AbstractAudioPluginMetaReader{m_module}
{
}

void NyquistPluginsMetaReader::doInit(const IApplication::RunMode&)
{
    // Don't call m_module.Initialize() here.
    // At this point FileNames::AudacityPathList() is not yet populated
    // (InitializePathList is called later in EffectsModule::onInit),
    // so Initialize() would always fail to find nyquist.lsp.
    // Instead, readMeta() handles lazy initialization when paths are ready.
}

bool NyquistPluginsMetaReader::canReadMeta(const io::path_t& pluginPath) const
{
    // Handle the special "Nyquist Prompt" plugin (which is not a file)
    if (pluginPath.toString() == "Nyquist Prompt") {
        return true;
    }

    // Handle regular .ny files
    return io::suffix(pluginPath) == "ny";
}

audio::AudioResourceType NyquistPluginsMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::NyquistPlugin;
}

RetVal<audio::AudioResourceMetaList> NyquistPluginsMetaReader::readMeta(const io::path_t& pluginPath) const
{
    try {
        // For Nyquist plugins, we bypass the validation step because they are simple script files
        // We just need to discover them and create metadata without running validation

        // Lazy initialization: if not initialized yet, initialize now
        // This is needed because the subprocess that registers plugins creates new instances
        // of meta readers that haven't been initialized via onInit()
        auto& module = const_cast<::NyquistEffectsModule&>(m_module);

        // Initialize() is idempotent - it's safe to call multiple times
        module.Initialize();

        muse::audio::AudioResourceMetaList metaList;
        wxString wxPluginPath = au3::wxFromString(pluginPath.toString());
        TranslatableString errorMessage{};

        // Discover the plugin to get its metadata
        int numPlugins = module.DiscoverPluginsAtPath(
            wxPluginPath, errorMessage, [&](PluginProvider* provider, ComponentInterface* ident) -> const PluginID&
        {
            // Use DefaultRegistrationCallback to create the descriptor
            auto& id = PluginManager::DefaultRegistrationCallback(provider, ident);

            if (const auto ptr = PluginManager::Get().GetPlugin(id)) {
                auto desc = *ptr;

                // For Nyquist plugins, we skip validation and just mark them as enabled and valid
                // This bypasses the subprocess validation that was causing error code 95
                desc.SetEnabled(true);
                desc.SetValid(true);

                // Determine the category based on the effect type from the $type directive in the .ny file
                // Type mapping for Nyquist effects:
                // - $type process  → Effects menu (Fx)
                // - $type generate → Generate menu (Fx)
                // - $type analyze  → Analyze menu (Fx)
                // - $type tool     → Tools menu (Fx)
                // NOTE: All Nyquist effects are treated as "Fx" for the plugin system
                // The specific menu placement is handled by the EffectType in the repository
                muse::String category;
                if (desc.GetEffectType() == ::EffectType::EffectTypeProcess
                    || desc.GetEffectType() == ::EffectType::EffectTypeGenerate
                    || desc.GetEffectType() == ::EffectType::EffectTypeAnalyze
                    || desc.GetEffectType() == ::EffectType::EffectTypeTool) {
                    category = u"Fx";
                } else {
                    category = u"None";
                }

                // Convert to AudioResourceMeta
                muse::audio::AudioResourceMeta meta;
                // Use the AU3 plugin ID (same as Audio Unit does)
                // This is necessary for looking up the plugin in PluginManager later
                meta.id = au3::wxToStdString(id);
                meta.type = muse::audio::AudioResourceType::NyquistPlugin;
                meta.vendor = au3::wxToStdString(desc.GetVendor());

                // Add attributes using the map interface
                meta.attributes.emplace(muse::audio::CATEGORIES_ATTRIBUTE, category);
                // Store the plugin name (from $name directive) for display
                meta.attributes.emplace(muse::audio::PLAYBACK_SETUP_DATA_ATTRIBUTE,
                                        muse::String::fromStdString(desc.GetSymbol().Msgid().Translation().ToStdString()));

                metaList.push_back(std::move(meta));
            } else {
                LOGW() << "NyquistPluginsMetaReader::readMeta - Could not get plugin descriptor for ID: " << au3::wxToStdString(id);
            }
            return id;
        });

        if (!errorMessage.empty()) {
            LOGE() << "Failed to read Nyquist plugin: " << au3::wxToString(errorMessage.Debug());
            return make_ret(Ret::Code::InternalError);
        }

        if (numPlugins == 0 || metaList.empty()) {
            LOGE() << "No Nyquist plugins found at path: " << pluginPath;
            return make_ret(Ret::Code::InternalError);
        }

        return RetVal<muse::audio::AudioResourceMetaList>::make_ok(metaList);
    }
    catch (const std::exception& e) {
        LOGE() << "NyquistPluginsMetaReader::readMeta - Exception: " << e.what();
        return make_ret(Ret::Code::InternalError);
    }
    catch (...) {
        LOGE() << "NyquistPluginsMetaReader::readMeta - Unknown exception";
        return make_ret(Ret::Code::InternalError);
    }
}
