/*
* Audacity: A Digital Audio Editor
*/
#include "musevstmodulesrepository.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-effects/EffectManager.h"
#include "au3-vst3/VST3EffectBase.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace au::effects;

void MuseVstModulesRepository::init()
{
    muse::vst::PluginContextFactory::instance().setPluginContext(&m_pluginContext);
}

void MuseVstModulesRepository::deinit()
{
    muse::vst::PluginContextFactory::instance().setPluginContext(nullptr);
}

bool MuseVstModulesRepository::exists(const muse::audio::AudioResourceId& resourceId) const
{
    PluginID pluginId = resourceId;
    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(pluginId);
    return plug != nullptr;
}

muse::vst::PluginModulePtr MuseVstModulesRepository::pluginModule(const muse::audio::AudioResourceId& resourceId) const
{
    PluginID pluginId = resourceId;
    EffectPlugin* plug = EffectManager::Get().GetEffect(pluginId);
    IF_ASSERT_FAILED(plug) {
        LOGE() << "plug not available, pluginId: " << resourceId;
        return nullptr;
    }

    VST3EffectBase* vstPlug = dynamic_cast<VST3EffectBase*>(plug);
    IF_ASSERT_FAILED(vstPlug) {
        LOGE() << "plug not vst, pluginId: " << resourceId;
        return nullptr;
    }

    return vstPlug->vstModule();
}

void MuseVstModulesRepository::addPluginModule(const muse::audio::AudioResourceId&)
{
}

void MuseVstModulesRepository::removePluginModule(const muse::audio::AudioResourceId& resourceId)
{
    UNUSED(resourceId);
}

muse::audio::AudioResourceMetaList MuseVstModulesRepository::instrumentModulesMeta() const
{
    return muse::audio::AudioResourceMetaList();
}

muse::audio::AudioResourceMetaList MuseVstModulesRepository::fxModulesMeta() const
{
    return muse::audio::AudioResourceMetaList();
}

void MuseVstModulesRepository::refresh()
{
}
