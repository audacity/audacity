/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "internal/nyquisteffectsrepository.h"
#include "internal/nyquistparameterextractorservice.h"
#include "internal/nyquistviewlauncher.h"

#include "view/nyquistpromptviewmodel.h"

static void nyquist_init_qrc()
{
    Q_INIT_RESOURCE(nyquist);
}

std::string au::effects::NyquistEffectsModule::moduleName() const
{
    return "effects_nyquist";
}

void au::effects::NyquistEffectsModule::registerExports()
{
    m_nyquistEffectsRepository = std::make_shared<NyquistEffectsRepository>();

    ioc()->registerExport<INyquistEffectsRepository>(moduleName(), m_nyquistEffectsRepository);
}

void au::effects::NyquistEffectsModule::resolveImports()
{
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
    nyquist_init_qrc();
}

void au::effects::NyquistEffectsModule::registerUiTypes()
{
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NyquistPromptViewModelFactory);
}
