/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometer.h"
#include "audio/iaudiodevicesprovider.h"
#include "audio/iaudioengine.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"
#include "record/iaudioinput.h"
#include "record/irecordconfiguration.h"
#include "record/irecordcontroller.h"
#include "record/irecordmetercontroller.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::record {
class Au3AudioInput : public IAudioInput, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<record::IRecordConfiguration> configuration;

    muse::Inject<au::audio::IAudioEngine> audioEngine{ this };
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider{ this };
    muse::Inject<playback::IPlaybackController> playbackController{ this };
    muse::Inject<record::IRecordController> controller{ this };
    muse::Inject<record::IRecordMeterController> meterController{ this };
    muse::Inject<trackedit::ISelectionController> selectionController{ this };
    muse::Inject<trackedit::ITrackNavigationController> trackNavigationController{ this };

public:
    Au3AudioInput(const muse::modularity::ContextPtr& ctx);

    muse::async::Promise<float> recordVolume() const override;
    void setRecordVolume(float volume) override;
    muse::async::Channel<float> recordVolumeChanged() const override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordSignalChanges() const override;
    muse::async::Channel<audio::audioch_t, au::audio::MeterSignal> recordTrackSignalChanges(int64_t key) const override;

private:
    au3::Au3Project* projectRef() const;

    void initMeter();
    bool isTrackMeterMonitoring() const;
    int getFocusedTrackChannels() const;

    void updateAudioEngineMonitoring() const;
    void startAudioEngineMonitoring() const;
    void stopAudioEngineMonitoring() const;
    bool canStartAudioEngineMonitoring() const;
    bool audioEngineShouldBeMonitoring() const;

    mutable muse::async::Channel<float> m_recordVolumeChanged;
    const std::shared_ptr<au::au3::Au3AudioMeter> m_inputMeter;
    int m_inputChannelsCount{};
    int m_focusedTrackChannels{};
};
}
