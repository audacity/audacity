/*
* Audacity: A Digital Audio Editor
*/

#include "wavetrackitem.h"

#include <QMetaType>
#include <QString>

#include "framework/global/containers.h"

#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"

using namespace au::projectscene;
using namespace au::trackedit;
using namespace au::audio;

static constexpr float PAN_SCALING_FACTOR = 100.f;
static constexpr float MIN_ALLOWED_PRESSURE = -145.f;
static constexpr float MAX_ALLOWED_PRESSURE = 0.f;

WaveTrackItem::WaveTrackItem(QObject* parent)
    : TrackItem(parent),
    m_leftChannelPressure(playback::MIN_DISPLAYED_DBFS),
    m_rightChannelPressure(playback::MIN_DISPLAYED_DBFS)
{
    connect(this, &WaveTrackItem::mutedChanged, this, [this]() {
        if (muted()) {
            resetAudioChannelsVolumePressure();
        }
    });
}

void WaveTrackItem::init(const trackedit::Track& track)
{
    TrackItem::init(track);

    emit channelCountChanged();

    playback()->audioOutput()->playbackTrackSignalChanges(trackId())
    .onReceive(this, [this](audio::audioch_t channel, const au::audio::MeterSignal& meterSignal) {
        setAudioChannelVolumePressure(channel, meterSignal.peak.pressure);
        setAudioChannelRMS(channel, meterSignal.rms.pressure);
    }, muse::async::Asyncable::Mode::SetReplace);

    record()->audioInput()->recordTrackSignalChanges(trackId())
    .onReceive(this, [this](audio::audioch_t channel, const au::audio::MeterSignal& meterSignal) {
        setAudioChannelVolumePressure(channel, meterSignal.peak.pressure);
        setAudioChannelRMS(channel, meterSignal.rms.pressure);
    }, muse::async::Asyncable::Mode::SetReplace);

    record()->audioInput()->monitoredTracksChanged().onNotify(this, [this]() {
        checkMainAudioInput();
    }, muse::async::Asyncable::Mode::SetReplace);

    const auto ctrl = trackPlaybackControl();
    m_outParams.volume = ctrl->volume(trackId());
    m_outParams.pan = ctrl->pan(trackId());
    m_outParams.solo = ctrl->solo(trackId());
    m_outParams.muted = ctrl->muted(trackId());
    emit volumeLevelChanged(m_outParams.volume);
    emit panChanged(m_outParams.pan);
    emit soloChanged();
    emit mutedChanged();
    emit outputParamsChanged(m_outParams);

    ctrl->muteOrSoloChanged().onReceive(this, [this](long trackId) {
        if (trackId != this->trackId()) {
            return;
        }
        muteOrSoloChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    checkMainAudioInput();
}

void WaveTrackItem::muteOrSoloChanged()
{
    auto paramChanged = false;
    if (m_outParams.solo != trackPlaybackControl()->solo(trackId())) {
        m_outParams.solo = !m_outParams.solo;
        paramChanged = true;
        emit soloChanged();
    }
    if (m_outParams.muted != trackPlaybackControl()->muted(trackId())) {
        m_outParams.muted = !m_outParams.muted;
        paramChanged = true;
        emit mutedChanged();
    }
    if (paramChanged) {
        emit outputParamsChanged(m_outParams);
    }
}

bool WaveTrackItem::outputOnly() const
{
    return m_outputOnly;
}

int WaveTrackItem::channelCount() const
{
    switch (trackType()) {
    case trackedit::TrackType::Mono:
        return 1;
    case trackedit::TrackType::Stereo:
        return 2;
    default:
        return 0;
    }
}

float WaveTrackItem::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float WaveTrackItem::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

float WaveTrackItem::leftChannelRMS() const
{
    return m_leftChannelRMS;
}

float WaveTrackItem::rightChannelRMS() const
{
    return m_rightChannelRMS;
}

float WaveTrackItem::volumeLevel() const
{
    return m_outParams.volume;
}

int WaveTrackItem::pan() const
{
    return m_outParams.pan * PAN_SCALING_FACTOR;
}

bool WaveTrackItem::solo() const
{
    return m_outParams.solo;
}

bool WaveTrackItem::muted() const
{
    return m_outParams.muted;
}

void WaveTrackItem::loadOutputParams(const audio::AudioOutputParams& newParams)
{
    if (!muse::RealIsEqual(m_outParams.volume, newParams.volume)) {
        m_outParams.volume = newParams.volume;
        emit volumeLevelChanged(newParams.volume);
    }

    if (!muse::RealIsEqual(m_outParams.pan, newParams.pan)) {
        m_outParams.pan = newParams.pan;
        emit panChanged(newParams.pan);
    }

    if (m_outParams.solo != newParams.solo) {
        m_outParams.solo = newParams.solo;
        emit soloChanged();
    }

    if (m_outParams.muted != newParams.muted) {
        m_outParams.muted = newParams.muted;
        emit mutedChanged();
    }
}

void WaveTrackItem::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void WaveTrackItem::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void WaveTrackItem::setLeftChannelRMS(float leftChannelRMS)
{
    if (qFuzzyCompare(m_leftChannelRMS, leftChannelRMS)) {
        return;
    }

    m_leftChannelRMS = leftChannelRMS;
    emit leftChannelRMSChanged(m_leftChannelRMS);
}

void WaveTrackItem::setRightChannelRMS(float rightChannelRMS)
{
    if (qFuzzyCompare(m_rightChannelRMS, rightChannelRMS)) {
        return;
    }

    m_rightChannelRMS = rightChannelRMS;
    emit rightChannelRMSChanged(m_rightChannelRMS);
}

void WaveTrackItem::setVolumeLevel(float volumeLevel, bool completed)
{
    trackPlaybackControl()->setVolume(trackId(), volumeLevel, completed);

    if (m_outParams.volume == volumeLevel) {
        return;
    }
    m_outParams.volume = volumeLevel;
    emit volumeLevelChanged(m_outParams.volume);
    emit outputParamsChanged(m_outParams);
}

void WaveTrackItem::setPan(int pan, bool completed)
{
    const float scaled = pan / PAN_SCALING_FACTOR;
    trackPlaybackControl()->setPan(trackId(), scaled, completed);

    if (m_outParams.pan == scaled) {
        return;
    }
    m_outParams.pan = scaled;
    emit panChanged(pan);
    emit outputParamsChanged(m_outParams);
}

void WaveTrackItem::setSolo(bool solo)
{
    const bool exclusive = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    trackPlaybackControl()->setSolo(trackId(), solo, exclusive);
}

void WaveTrackItem::setMuted(bool mute)
{
    const bool exclusive = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    trackPlaybackControl()->setMuted(trackId(), mute, exclusive);
}

void WaveTrackItem::setAudioChannelVolumePressure(const trackedit::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, MIN_ALLOWED_PRESSURE, MAX_ALLOWED_PRESSURE);
    chNum == 0 ? setLeftChannelPressure(clampedValue) : setRightChannelPressure(clampedValue);
}

void WaveTrackItem::setAudioChannelRMS(const trackedit::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, MIN_ALLOWED_PRESSURE, MAX_ALLOWED_PRESSURE);
    chNum == 0 ? setLeftChannelRMS(clampedValue) : setRightChannelRMS(clampedValue);
}

void WaveTrackItem::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(playback::MIN_DISPLAYED_DBFS);
    setRightChannelPressure(playback::MIN_DISPLAYED_DBFS);
}

const AudioOutputParams& WaveTrackItem::outputParams() const
{
    return m_outParams;
}

void WaveTrackItem::checkMainAudioInput()
{
    const bool wasMonitored = !m_inputChannels.empty();
    m_inputChannels = record()->audioInput()->monitoredInputChannels(trackId());

    if (m_inputChannels.empty()) {
        record()->audioInput()->recordSignalChanges().disconnect(this);
        if (wasMonitored) {
            resetAudioChannelsVolumePressure();
        }
        return;
    }

    record()->audioInput()->recordSignalChanges().onReceive(this,
                                                            [this](const audioch_t inputChannel, const audio::MeterSignal& meterSignal) {
        showInputSignal(inputChannel, meterSignal);
    }, muse::async::Asyncable::Mode::SetReplace);
}

void WaveTrackItem::showInputSignal(const audioch_t inputChannel, const audio::MeterSignal& meterSignal)
{
    if (inputChannel < m_mixedInputPressure.size()) {
        m_mixedInputPressure[inputChannel] = meterSignal.peak.pressure;
        m_mixedInputRMS[inputChannel] = meterSignal.rms.pressure;
    }

    for (audioch_t channel = 0; channel < m_inputChannels.size(); ++channel) {
        const std::vector<audioch_t>& sources = m_inputChannels[channel];
        if (!muse::contains(sources, inputChannel)) {
            continue;
        }

        if (sources.size() == 1) {
            setAudioChannelVolumePressure(channel, meterSignal.peak.pressure);
            setAudioChannelRMS(channel, meterSignal.rms.pressure);
            continue;
        }

        // A stereo input mixed into a mono track shows the louder input channel
        float pressure = playback::MIN_DISPLAYED_DBFS;
        float rms = playback::MIN_DISPLAYED_DBFS;
        for (const audioch_t source : sources) {
            if (source < m_mixedInputPressure.size()) {
                pressure = std::max(pressure, m_mixedInputPressure[source]);
                rms = std::max(rms, m_mixedInputRMS[source]);
            }
        }
        setAudioChannelVolumePressure(channel, pressure);
        setAudioChannelRMS(channel, rms);
    }
}

bool WaveTrackItem::isAudible() const
{
    return trackType() != TrackType::Label;
}
