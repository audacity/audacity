/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <limits>
#include <optional>
#include <memory>
#include <vector>

#include "audio/iaudiometer.h"

#include "au3-realtime-effects/RealtimeEffectManager.h"

#include "captureinput.h"
#include "juceincludes.h"
#include "playheadmapper.h"
#include "stereo.h"

class AudacityProject;
class ISequenceProcessingNode;
class PlayableSequence;

namespace au::au4juce {
class BlockContext;
class MonitoredInput;

//! Monitors the input of a device: meters it, and plays it through if asked, as it comes or through the effects
class JuceMonitoringCallback final : public juce::AudioIODeviceCallback
{
public:
    //! What the input is played through, if through the effects
    struct Effects {
        //! Whose effects, mute/solo and volume/pan, then the master's effects; only the master's if null
        std::shared_ptr<const PlayableSequence> track;
    };

    //! @param device opened, with input channels, and output channels for `playThrough`
    //! @param effects if played through the effects
    JuceMonitoringCallback(AudacityProject& project, std::unique_ptr<juce::AudioIODevice> device, bool playThrough,
                           audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume, std::optional<Effects> effects);
    ~JuceMonitoringCallback() override;

    AudacityProject& project;
    const bool playThrough;
    //! The track played through, if through the effects
    const std::optional<Effects> effects;

    double sampleRate() const { return m_sampleRate; }
    //! If played through the effects
    RealtimeEffects::InitializationScope* effectsInitialization();

    void start();
    void stop();

    // juce::AudioIODeviceCallback
    void audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                          int numOutputChannels, int numSamples,
                                          const juce::AudioIODeviceCallbackContext& context) override;
    void audioDeviceAboutToStart(juce::AudioIODevice*) override {}
    void audioDeviceStopped() override {}
    void audioDeviceError(const juce::String& errorMessage) override;

private:
    std::unique_ptr<juce::AudioIODevice> m_device;
    const int m_blockSize;
    const double m_sampleRate;
    const audio::IAudioMeterPtr m_meter;
    const std::atomic<float>& m_inputVolume;
    CaptureInput m_input;
    std::vector<const float*> m_inputPointers;

    // If played through the effects: the input's chain, then the master's effects, rendered as a block by stream index
    std::optional<RealtimeEffects::InitializationScope> m_effectsInitialization;
    PlayheadMapper m_mapper;
    std::atomic<int64_t> m_playEnd { std::numeric_limits<int64_t>::max() };
    std::unique_ptr<BlockContext> m_blockContext;
    std::shared_ptr<MonitoredInput> m_monitoredInput;
    std::shared_ptr<ISequenceProcessingNode> m_chain;
    int64_t m_index = 0;
    std::vector<float> m_stereo[numChannels];
};
}
