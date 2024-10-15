/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsmodule.h"

#include "internal/vsteffectsrepository.h"
#include "internal/vst3pluginsscanner.h"
#include "internal/vst3pluginsmetareader.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

using namespace au::effects;

std::string VstEffectsModule::moduleName() const
{
    return "effects_vst";
}

void VstEffectsModule::registerExports()
{
    m_vstEffectsRepository = std::make_shared<VstEffectsRepository>();

    ioc()->registerExport<IVstEffectsRepository>(moduleName(), m_vstEffectsRepository);
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
}
