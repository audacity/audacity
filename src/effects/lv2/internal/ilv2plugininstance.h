/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/async/notification.h"

#include "vsttypes.h"
#include "audio/audiotypes.h"

namespace muse::vst {
class IVstPluginInstance
{
public:

    virtual ~IVstPluginInstance() = default;

    virtual const muse::audio::AudioResourceId& resourceId() const = 0;
    virtual const std::string& name() const = 0;
    virtual VstPluginInstanceId id() const = 0;

    virtual bool isLoaded() const = 0;
    virtual async::Notification loadingCompleted() const = 0;

    virtual PluginViewPtr createView() const = 0;
    virtual PluginControllerPtr controller() const = 0;
    virtual PluginComponentPtr component() const = 0;
    virtual PluginMidiMappingPtr midiMapping() const = 0;

    virtual void updatePluginConfig(const muse::audio::AudioUnitConfig& config) = 0;
    virtual void refreshConfig() = 0;
    virtual async::Channel<muse::audio::AudioUnitConfig> pluginSettingsChanged() const = 0;
};
}
