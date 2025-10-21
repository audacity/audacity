/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"
#include "playback/iaudiodevicesprovider.h"
#include "record/irecordconfiguration.h"
#include "record/irecordcontroller.h"
#include "record/irecordmetercontroller.h"
#include "trackedit/iselectioncontroller.h"
#include "au3audio/iaudioengine.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometer.h"
#include "record/iaudioinput.h"

namespace au::record {
class Au3AudioInput : public IAudioInput, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<record::IRecordConfiguration> configuration;
    muse::Inject<record::IRecordController> controller;
    muse::Inject<record::IRecordMeterController> meterController;
    muse::Inject<playback::IPlaybackController> playbackController;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<au::audio::IAudioEngine> audioEngine;

public:
    Au3AudioInput();

    muse::async::Promise<float> recordVolume() const override;
    void setRecordVolume(float volume) override;
    muse::async::Channel<float> recordVolumeChanged() const override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordSignalChanges() const override;
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> recordTrackSignalChanges(int64_t key) const override;

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
    std::shared_ptr<au::au3::Meter> m_inputMeter;
    int m_inputChannelsCount{};
    int m_focusedTrackChannels{};
};
}
