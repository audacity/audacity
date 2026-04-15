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

static const std::string mname("effects_audiounit");

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
    return mname;
}

void au::effects::AudioUnitEffectsModule::registerExports()
{
    globalIoc()->registerExport<IAudioUnitEffectsRepository>(mname, std::make_shared<AudioUnitEffectsRepository>());
}

void au::effects::AudioUnitEffectsModule::resolveImports()
{
    auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(mname);
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<AudioUnitPluginsScanner>());
    }

    auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(mname);
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_metaReader);
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

muse::modularity::IContextSetup* au::effects::AudioUnitEffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new AudioUnitEffectsContext(ctx);
}

// =====================================================
// AudioUnitEffectsContext
// =====================================================

void au::effects::AudioUnitEffectsContext::registerExports()
{
}

void au::effects::AudioUnitEffectsContext::resolveImports()
{
    auto launchRegister = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (launchRegister) {
        launchRegister->regLauncher("AudioUnit", std::make_shared<AudioUnitViewLauncher>(iocContext()));
    }
}

void au::effects::AudioUnitEffectsContext::onDeinit()
{
}
