/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsmodule.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "internal/vsteffectsrepository.h"
#include "internal/vst3pluginsscanner.h"
#include "internal/vst3pluginsmetareader.h"
#include "internal/vst3viewlauncher.h"
#include "internal/vstparameterextractorservice.h"

#include "internal/musevstpluginsregister.h"
#include "internal/musevstmodulesrepository.h"

#include "view/vstviewmodel.h"

using namespace muse;
using namespace au::effects;

static const std::string mname("effects_vst");

static void vst_init_qrc()
{
    Q_INIT_RESOURCE(vst);
}

VstEffectsModule::VstEffectsModule()
    : m_vstMetaReader(std::make_shared<Vst3PluginsMetaReader>())
{
    vst_init_qrc();
}

std::string VstEffectsModule::moduleName() const
{
    return mname;
}

void VstEffectsModule::registerExports()
{
    m_museVstModulesRepository = std::make_shared<MuseVstModulesRepository>();
    m_vstEffectsRepository = std::make_shared<VstEffectsRepository>();

    // for muse
    globalIoc()->registerExport<muse::vst::IVstInstancesRegister>(mname, new MuseVstInstancesRegister());
    globalIoc()->registerExport<muse::vst::IVstModulesRepository>(mname, m_museVstModulesRepository);

    globalIoc()->registerExport<IVstEffectsRepository>(mname, m_vstEffectsRepository);
}

void VstEffectsModule::resolveImports()
{
    auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(mname);
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<Vst3PluginsScanner>());
    }

    auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(mname);
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_vstMetaReader);
    }

    auto paramExtractorRegistry = globalIoc()->resolve<IParameterExtractorRegistry>(mname);
    if (paramExtractorRegistry) {
        paramExtractorRegistry->registerExtractor(std::make_shared<VstParameterExtractorService>());
    }
}

void VstEffectsModule::registerResources()
{
}

void VstEffectsModule::registerUiTypes()
{
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(VstViewModelFactory);
}

void VstEffectsModule::onInit(const muse::IApplication::RunMode& runMode)
{
    m_museVstModulesRepository->init();
    m_vstMetaReader->init(runMode);
}

void VstEffectsModule::onDeinit()
{
    m_museVstModulesRepository->deinit();
    m_vstMetaReader->deinit();
}

muse::modularity::IContextSetup* VstEffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new VstEffectsContext(ctx);
}

// =====================================================
// VstEffectsContext
// =====================================================

void VstEffectsContext::registerExports()
{
}

void VstEffectsContext::resolveImports()
{
    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (lr) {
        lr->regLauncher("VST3", std::make_shared<Vst3ViewLauncher>(iocContext()));
    }
}

void VstEffectsContext::onDeinit()
{
}
