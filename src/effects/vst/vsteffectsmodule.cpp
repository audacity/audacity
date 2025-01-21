/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsmodule.h"

#include "ui/iinteractiveuriregister.h"

#include "internal/vsteffectsrepository.h"
#include "internal/vst3pluginsscanner.h"
#include "internal/vst3pluginsmetareader.h"
#include "internal/vst3viewlauncher.h"

#include "internal/musevstpluginsregister.h"
#include "internal/musevstmodulesrepository.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"

//! NOTE From Muse
#include "view/vstfxeditorview.h"

using namespace muse;
using namespace muse::ui;
using namespace au::effects;

std::string VstEffectsModule::moduleName() const
{
    return "effects_vst";
}

void VstEffectsModule::registerExports()
{
    m_vstEffectsRepository = std::make_shared<VstEffectsRepository>();
    m_museVstModulesRepository = std::make_shared<MuseVstModulesRepository>();

    ioc()->registerExport<IVstEffectsRepository>(moduleName(), m_vstEffectsRepository);

    // for muse
    ioc()->registerExport<muse::vst::IVstInstancesRegister>(moduleName(), new MuseVstInstancesRegister());
    ioc()->registerExport<muse::vst::IVstModulesRepository>(moduleName(), m_museVstModulesRepository);
}

void VstEffectsModule::resolveImports()
{
    auto scannerRegister = ioc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<Vst3PluginsScanner>());
    }

    auto metaReaderRegister = ioc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(std::make_shared<Vst3PluginsMetaReader>());
    }

    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(moduleName());
    if (lr) {
        lr->regLauncher("VST3", std::make_shared<Vst3ViewLauncher>());
    }

    auto ir = ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerUri(Uri("audacity://effects/vst_viewer"),
                        ContainerMeta(ContainerType::QWidgetDialog, qRegisterMetaType<muse::vst::VstFxEditorView>("VstFxEditorView")));
    }
}

void VstEffectsModule::onInit(const muse::IApplication::RunMode&)
{
    m_museVstModulesRepository->init();
}

void VstEffectsModule::onDeinit()
{
    m_museVstModulesRepository->deinit();
}
