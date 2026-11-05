/*
* Audacity: A Digital Audio Editor
*/
#include "soundactivationlevelmodel.h"

#include <algorithm>
#include <cmath>

#include "framework/global/realfn.h"

#include "playback/playbacktypes.h"
#include "record/iaudioinput.h"

using namespace au::record;

SoundActivationLevelModel::SoundActivationLevelModel(QObject* parent)
    : MeterModel(parent)
{
    resetChannelsSignal();
}

SoundActivationLevelModel::~SoundActivationLevelModel()
{
    setListening(false);
}

void SoundActivationLevelModel::init()
{
    MeterModel::init();

    recordConfiguration()->soundActivationLevelChanged().onNotify(this, [this]() {
        setLevel(recordConfiguration()->soundActivationLevel());
    });

    audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta& delta) {
        if (delta.contains(audio::AudioConfigurationField::InputChannels)) {
            emit recordingChannelsCountChanged();
        }
    });

    setLevel(recordConfiguration()->soundActivationLevel());
}

void SoundActivationLevelModel::apply()
{
    recordConfiguration()->setSoundActivationLevel(m_level);
}

double SoundActivationLevelModel::level() const
{
    return m_level;
}

void SoundActivationLevelModel::setLevel(double level)
{
    level = std::round(std::clamp(level, static_cast<double>(dbRange()), 0.0) * 10.0) / 10.0;
    if (muse::RealIsEqual(m_level, level)) {
        return;
    }

    m_level = level;
    setVolume(static_cast<float>(level));
    emit levelChanged();
}

bool SoundActivationLevelModel::listening() const
{
    return m_listening;
}

void SoundActivationLevelModel::setListening(bool listening)
{
    if (m_listening == listening) {
        return;
    }

    m_listening = listening;
    recordMeterController()->setSoundActivationMeterVisible(listening);

    if (listening) {
        record()->audioInput()->recordSignalChanges().onReceive(this,
                                                                [this](const audio::audioch_t channel, const audio::MeterSignal& signal) {
            setChannelSignal(channel, signal);
        });
    } else {
        record()->audioInput()->recordSignalChanges().disconnect(this);
        resetChannelsSignal();
    }

    emit listeningChanged();
}

int SoundActivationLevelModel::recordingChannelsCount() const
{
    return audioDriverController()->configuration().inputChannels;
}

float SoundActivationLevelModel::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float SoundActivationLevelModel::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

float SoundActivationLevelModel::leftChannelRMS() const
{
    return m_leftChannelRMS;
}

float SoundActivationLevelModel::rightChannelRMS() const
{
    return m_rightChannelRMS;
}

void SoundActivationLevelModel::setChannelSignal(audio::audioch_t channel, const audio::MeterSignal& signal)
{
    const float pressure = std::clamp(signal.peak.pressure, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);
    const float rms = std::clamp(signal.rms.pressure, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);

    if (channel == 0) {
        m_leftChannelPressure = pressure;
        m_leftChannelRMS = rms;
    } else {
        m_rightChannelPressure = pressure;
        m_rightChannelRMS = rms;
    }

    emit channelsSignalChanged();
}

void SoundActivationLevelModel::resetChannelsSignal()
{
    m_leftChannelPressure = playback::MIN_DISPLAYED_DBFS;
    m_rightChannelPressure = playback::MIN_DISPLAYED_DBFS;
    m_leftChannelRMS = playback::MIN_DISPLAYED_DBFS;
    m_rightChannelRMS = playback::MIN_DISPLAYED_DBFS;

    emit channelsSignalChanged();
}
