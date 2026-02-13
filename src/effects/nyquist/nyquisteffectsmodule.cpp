/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "internal/nyquisteffectsrepository.h"
#include "internal/nyquistparameterextractorservice.h"
#include "internal/nyquistviewlauncher.h"
#include "internal/nyquistpluginsscanner.h"
#include "internal/nyquistpluginsmetareader.h"

#include "view/nyquistpromptviewmodel.h"

static void nyquist_init_qrc()
{
    Q_INIT_RESOURCE(nyquist);
}

au::effects::NyquistEffectsModule::NyquistEffectsModule()
    : m_nyquistMetaReader(std::make_shared<NyquistPluginsMetaReader>())
{
    nyquist_init_qrc();
}

std::string au::effects::NyquistEffectsModule::moduleName() const
{
    return "effects_nyquist";
}

void au::effects::NyquistEffectsModule::registerExports()
{
    m_nyquistEffectsRepository = std::make_shared<NyquistEffectsRepository>(iocContext());

    ioc()->registerExport<INyquistEffectsRepository>(moduleName(), m_nyquistEffectsRepository);
}

void au::effects::NyquistEffectsModule::resolveImports()
{
    auto scannerRegister = ioc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<NyquistPluginsScanner>());
    }

    auto metaReaderRegister = ioc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_nyquistMetaReader);
    }

    auto paramExtractorRegistry = ioc()->resolve<IParameterExtractorRegistry>(moduleName());
    if (paramExtractorRegistry) {
        paramExtractorRegistry->registerExtractor(std::make_shared<NyquistParameterExtractorService>());
    }

    auto launchRegister = ioc()->resolve<IEffectViewLaunchRegister>(moduleName());
    if (launchRegister) {
        launchRegister->regLauncher("Nyquist", std::make_shared<NyquistViewLauncher>(iocContext()));
    }
}

void au::effects::NyquistEffectsModule::registerResources()
{
}

void au::effects::NyquistEffectsModule::registerUiTypes()
{
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NyquistPromptViewModelFactory);
}

void au::effects::NyquistEffectsModule::onInit(const muse::IApplication::RunMode& runMode)
{
    m_nyquistMetaReader->init(runMode);
}

void au::effects::NyquistEffectsModule::onDeinit()
{
    m_nyquistMetaReader->deinit();
}
