/*
 * Audacity: A Digital Audio Editor
 */
#include "audacityextensionsmodule.h"

#include "framework/global/api/iapiregister.h"

#include "api/audiotransformapi.h"
#include "api/nativeapi.h"
#include "api/preferencesapi.h"
#include "extensionpreferences.h"

namespace au::extensions {
namespace {
const std::string mname = "audacity_extensions";
}

std::string AudacityExtensionsModule::moduleName() const
{
    return mname;
}

void AudacityExtensionsModule::registerApi()
{
    auto registry = muse::modularity::globalIoc()->resolve<muse::api::IApiRegister>(mname);
    if (registry) {
        registry->regApiCreator(mname, "MuseApi.Native", new muse::api::ApiCreator<NativeApi>());
        registry->regApiCreator(mname, "Audacity.AudioTransform", new muse::api::ApiCreator<AudioTransformApi>());
        registry->regApiCreator(mname, "Audacity.Preferences", new muse::api::ApiCreator<PreferencesApi>());
    }
}

muse::modularity::IContextSetup* AudacityExtensionsModule::newContext(const muse::modularity::ContextPtr& context) const
{
    return new AudacityExtensionsContext(context);
}

AudacityExtensionsContext::AudacityExtensionsContext(const muse::modularity::ContextPtr& context)
    : IContextSetup(context)
{
}

void AudacityExtensionsContext::onInit(const muse::IApplication::RunMode&)
{
    m_extensions = ioc()->resolve<muse::extensions::IExtensionsProvider>(mname);
    enableExtensionsByDefault();
    registerPreferenceDefaults();
    if (m_extensions) {
        m_extensions->manifestListChanged().onNotify(this, [this] {
            enableExtensionsByDefault();
            registerPreferenceDefaults();
        });
    }
}

void AudacityExtensionsContext::enableExtensionsByDefault() const
{
    if (!m_extensions) {
        return;
    }
    for (const auto& manifest : m_extensions->manifestList()) {
        m_extensions->setEnabled(manifest.uri, true);
    }
}

void AudacityExtensionsContext::registerPreferenceDefaults() const
{
    if (!m_extensions) {
        return;
    }
    for (const auto& manifest : m_extensions->manifestList()) {
        registerExtensionPreferenceDefaults(manifest);
    }
}
} // namespace au::extensions
