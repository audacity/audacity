/*
* Audacity: A Digital Audio Editor
*/
#pragma once

// from muse
#include "vst/ivstplugininstance.h"

#include "effects/effects_base/effectstypes.h"

class VST3Instance;
namespace au::effects {
class MuseVstPluginInstance : public muse::vst::IVstPluginInstance
{
public:
    MuseVstPluginInstance(const EffectId& effectId, const EffectInstanceId& instanceId, std::shared_ptr<VST3Instance> auVstInstance);

    const muse::audio::AudioResourceId& resourceId() const override;
    const std::string& name() const override;
    muse::vst::VstPluginInstanceId id() const override;

    bool isLoaded() const override;
    muse::async::Notification loadingCompleted() const override;

    muse::vst::PluginViewPtr createView() const override;
    muse::vst::PluginControllerPtr controller() const override;
    muse::vst::PluginComponentPtr component() const override;
    muse::vst::PluginMidiMappingPtr midiMapping() const override;

    void updatePluginConfig(const muse::audio::AudioUnitConfig& config) override;
    void refreshConfig() override;
    muse::async::Channel<muse::audio::AudioUnitConfig> pluginSettingsChanged() const override;

private:

    EffectId m_effectId;
    EffectInstanceId m_instanceId;
    std::shared_ptr<VST3Instance> m_auVstInstance;
};
}
