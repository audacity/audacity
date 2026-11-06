/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <map>
#include <memory>
#include <string>

#include "framework/global/modularity/ioc.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "audio/iaudioengine.h"
#include "record/irecordconfiguration.h"

#include "au3-utility/LockFreeQueue.h"
#include "au3-utility/Observer.h"

#include "../au3audioiolistener.h"
#include "playedrange.h"

class PlayableSequence;

namespace juce {
class AudioIODevice;
}

namespace RealtimeEffects {
class InitializationScope;
}

namespace au::au4juce {
class JuceAudioDevices;
class JuceIoCallback;
class JuceMonitoringCallback;

//! An audio engine over JUCE audio devices, which plays safe or fast: see SafeIoCallback and FastIoCallback
class JuceAudioEngine final : public audio::IAudioEngine
{
    muse::GlobalInject<record::IRecordConfiguration> recordConfiguration;

public:
    enum class Mode {
        Safe,
        Fast,
    };

    //! Safe until told otherwise
    explicit JuceAudioEngine(std::shared_ptr<JuceAudioDevices> devices);
    ~JuceAudioEngine() override;

    //! The controller whose configuration selects the device; must outlive this.
    void setDriverController(const audio::IAudioDriverController* driverController);

    //! How the next streams play: the engine is two backends, which set it when activated
    void setMode(Mode mode);

    bool isBusy() const override;
    bool isCapturing() const override;
    bool isMonitoring() const override;

    std::optional<audio::AudioStreamDescriptor> currentStream() const override;
    bool isStreamActive(AudacityProject& project) const override;
    double transportTime() const override;

    int startTransport(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, AudacityProject& project,
                       const StartTransportOptions& options) override;
    void stopTransport() override;
    void pauseTransport(bool pause) override;
    void seek(double time) override;

    std::shared_ptr<RealtimeEffectState> addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                const std::string& effectId) override;
    void removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                   const std::shared_ptr<RealtimeEffectState>& state) override;
    std::shared_ptr<RealtimeEffectState> replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group, size_t effectListIndex,
                                                                    const std::string& newEffectId) override;

    void setPlaybackMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter) override;
    void setCaptureMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter) override;
    void stopMeters(AudacityProject& project) override;

    void startMonitoring(AudacityProject& project) override;
    void stopMonitoring() override;
    //! Playing fast, the input is monitored through its effects, then the master's; playing safe, as it comes
    void setMonitoredTrack(std::optional<int64_t> track) override;

    void setInputVolume(float newInputVolume) override;
    float getInputVolume() const override;
    void setPlaybackVolume(float newPlaybackVolume) override;
    float getPlaybackVolume() const override;

    bool canStopTransport(AudacityProject& project) const override;

    void handleDeviceChange() override;
    muse::String lastErrorString() const override;
    double getPlaybackSampleRate() const override;
    void updateTimePosition(unsigned long newlyConsumedSamples) override;
    std::optional<audio::AudioCallbackInfo> consumeNextCallbackInfo() override;

    muse::async::Notification recordingUpdateRequested() const override;
    muse::async::Notification recordingNewBlocks() const override;
    muse::async::Notification recordingCommitRequested() const override;
    muse::async::Notification recordingFinished() const override;
    muse::async::Notification streamStopped() const override;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const override;

private:
    void onPlayRegionChanged();
    //! Opened with as many of the requested channels as it has, or null, with `m_lastError` set
    std::unique_ptr<juce::AudioIODevice> openDevice(int numInputChannels, int numOutputChannels, double sampleRate);
    //! Where realtime effects added to `project` while streaming are initialized, if anywhere
    RealtimeEffects::InitializationScope* effectsInitialization(AudacityProject& project) const;
    //! The track of `project` the input is monitored through, if any
    std::shared_ptr<const PlayableSequence> monitoredTrack(AudacityProject& project) const;

    const std::shared_ptr<JuceAudioDevices> m_devices;
    const audio::IAudioDriverController* m_driverController = nullptr;

    Mode m_mode = Mode::Safe;
    std::optional<int64_t> m_monitoredTrack;
    std::unique_ptr<JuceIoCallback> m_stream;
    std::unique_ptr<JuceMonitoringCallback> m_monitoring;
    int m_nextStreamToken = 1;
    double m_lastSampleRate = 0.0;
    muse::String m_lastError;

    std::atomic<float> m_playbackVolume { 1.f };
    std::atomic<float> m_inputVolume { 1.f };
    std::map<const AudacityProject*, audio::IAudioMeterPtr> m_playbackMeters;
    std::map<const AudacityProject*, audio::IAudioMeterPtr> m_captureMeters;

    // Filled by the device callback, consumed by the main thread
    LockFreeQueue<audio::AudioCallbackInfo> m_callbackInfoQueue { 512 };
    LockFreeQueue<PlayedRange> m_playedRanges { 1024 };

    // Main thread state of the consumption of m_playedRanges
    PlayedRange m_currentPlayedRange;
    unsigned long m_consumedInCurrentRange = 0;
    double m_transportTime = 0.0;

    Observer::Subscription m_playRegionSubscription;

    //! Notified of recordings, as AudioIO notifies its listener
    const std::shared_ptr<au3audio::Au3AudioIOListener> m_listener = std::make_shared<au3audio::Au3AudioIOListener>();
    muse::async::Notification m_streamStopped;
};
}
