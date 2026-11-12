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

#include "au3wrap/au3audioiolistener.h"
#include "playedrange.h"


namespace RealtimeEffects {
class InitializationScope;
}

namespace au::audio {
class IDevice;
class IDeviceOpener;
}

namespace au::au4audio {
class IoCallback;
class MonitoringCallback;

//! An audio engine whose tracks and master are graphs of nodes: see IoCallback. Its devices are those its IDeviceOpener
//! opens.
class Au4AudioEngine final : public audio::IAudioEngine
{
    muse::GlobalInject<record::IRecordConfiguration> recordConfiguration;

public:
    explicit Au4AudioEngine(std::shared_ptr<audio::IDeviceOpener> deviceOpener);
    ~Au4AudioEngine() override;

    //! The controller whose configuration selects the device; must outlive this.
    void setDriverController(const audio::IAudioDriverController* driverController);

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
    std::unique_ptr<audio::IDevice> openDevice(int numInputChannels, int numOutputChannels, double sampleRate);
    //! What the device's hardware volume can't set of the input volume, the callbacks apply
    void updateInputGain();
    //! Where realtime effects added to `project` while streaming are initialized, if anywhere
    RealtimeEffects::InitializationScope* effectsInitialization(AudacityProject& project) const;

    const std::shared_ptr<audio::IDeviceOpener> m_deviceOpener;
    const audio::IAudioDriverController* m_driverController = nullptr;

    std::unique_ptr<IoCallback> m_stream;
    std::unique_ptr<MonitoringCallback> m_monitoring;
    int m_nextStreamToken = 1;
    double m_lastSampleRate = 0.0;
    muse::String m_lastError;

    std::atomic<float> m_playbackVolume { 1.f };
    //! As set, applied to the samples unless the device's hardware volume can set it
    float m_inputVolume = 1.f;
    //! What the callbacks apply to the input
    std::atomic<float> m_inputGain { 1.f };
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
    const std::shared_ptr<au3::Au3AudioIOListener> m_listener = std::make_shared<au3::Au3AudioIOListener>();
    muse::async::Notification m_streamStopped;
};
}
