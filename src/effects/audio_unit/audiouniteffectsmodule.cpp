/*
* Audacity: A Digital Audio Editor
*/
#include "audiouniteffectsmodule.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "internal/audiouniteffectsrepository.h"
#include "internal/audiounitpluginsscanner.h"
#include "internal/audiounitpluginsmetareader.h"
#include "internal/audiounitviewlauncher.h"

#include "view/audiounitview.h"
#include "view/audiounitviewmodel.h"

static void AudioUnitInitQrc()
{
    Q_INIT_RESOURCE(audiounit);
}

au::effects::AudioUnitEffectsModule::AudioUnitEffectsModule()
    : m_metaReader(std::make_shared<AudioUnitPluginsMetaReader>())
{
    AudioUnitInitQrc();
}

std::string au::effects::AudioUnitEffectsModule::moduleName() const
{
    return "effects_audiounit";
}

void au::effects::AudioUnitEffectsModule::registerExports()
{
    m_effectsRepository = std::make_shared<AudioUnitEffectsRepository>();

    ioc()->registerExport<IAudioUnitEffectsRepository>(moduleName(), m_effectsRepository);
}

void au::effects::AudioUnitEffectsModule::resolveImports()
{
    auto scannerRegister = ioc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<AudioUnitPluginsScanner>());
    }

    auto metaReaderRegister = ioc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_metaReader);
    }

    auto launchRegister = ioc()->resolve<IEffectViewLaunchRegister>(moduleName());
    if (launchRegister) {
        launchRegister->regLauncher("AudioUnit", std::make_shared<AudioUnitViewLauncher>());
    }
}

void au::effects::AudioUnitEffectsModule::registerUiTypes()
{
    qmlRegisterType<au::effects::AudioUnitView>("Audacity.AudioUnit", 1, 0, "AudioUnitView");
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(AudioUnitViewModelFactory);
}

void au::effects::AudioUnitEffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_metaReader->init(mode);
}

void au::effects::AudioUnitEffectsModule::onDeinit()
{
    m_metaReader->deinit();
}
