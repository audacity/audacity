/*
* Audacity: A Digital Audio Editor
*/

#include "wavetrackitem.h"

#include <QMetaType>
#include <QVariantMap>

#include <algorithm>
#include <QString>

#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"
#include "framework/global/translation.h"

using namespace au::projectscene;
using namespace au::trackedit;
using namespace au::audio;

namespace {
//! What `outputMenu` calls the master
const QString MASTER_OUTPUT_ID = QStringLiteral("master");
}

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

    connect(this, &WaveTrackItem::isFocusedChanged, this, [this]() {
        checkMainAudioInput();
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

    audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta& delta) {
        if (!delta.contains(audio::AudioConfigurationField::InputChannels)) {
            return;
        }
        const int inputChannelsCount = audioDriverController()->configuration().inputChannels;
        m_recordStreamChannelsMatch = (trackType() == trackedit::TrackType::Mono && inputChannelsCount == 1)
                                      || (trackType() == trackedit::TrackType::Stereo && inputChannelsCount == 2);
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

    const int inputChannelsCount = audioDriverController()->configuration().inputChannels;
    m_recordStreamChannelsMatch = (trackType() == trackedit::TrackType::Mono && inputChannelsCount == 1)
                                  || (trackType() == trackedit::TrackType::Stereo && inputChannelsCount == 2);

    checkMainAudioInput();

    // Busses come and go and are renamed, and it changes output
    if (const auto project = globalContext()->currentTrackeditProject()) {
        const auto onTracks = [this](const auto&...) { emit outputChanged(); };
        project->trackAdded().onReceive(this, onTracks, muse::async::Asyncable::Mode::SetReplace);
        project->trackChanged().onReceive(this, onTracks, muse::async::Asyncable::Mode::SetReplace);
        project->trackRemoved().onReceive(this, onTracks, muse::async::Asyncable::Mode::SetReplace);
        project->tracksChanged().onReceive(this, onTracks, muse::async::Asyncable::Mode::SetReplace);
    }
    emit outputChanged();
}

bool WaveTrackItem::hasOutputChoice() const
{
    const auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return false;
    }
    const auto tracks = project->trackList();
    return std::any_of(tracks.begin(), tracks.end(), [this](const Track& track) {
        return track.type == TrackType::Bus && track.id != trackId();
    });
}

QString WaveTrackItem::outputTitle() const
{
    const auto project = globalContext()->currentTrackeditProject();
    const TrackId bus = trackeditInteraction()->trackOutput(trackId());
    const auto track = project && bus != INVALID_TRACK ? project->track(bus) : std::nullopt;
    //: Where a track goes when it goes to no bus
    return track ? track->title.toQString() : muse::qtrc("projectscene", "Master");
}

QVariantList WaveTrackItem::outputMenu() const
{
    QVariantList result;
    const auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return result;
    }
    const TrackId output = trackeditInteraction()->trackOutput(trackId());
    const auto item = [](const QString& id, const QString& title, bool checked, bool enabled) {
        QVariantMap map;
        map["id"] = id;
        map["title"] = title;
        map["checkable"] = true;
        map["checked"] = checked;
        map["enabled"] = enabled;
        return map;
    };
    result << item(MASTER_OUTPUT_ID, muse::qtrc("projectscene", "Master"), output == INVALID_TRACK, true);
    for (const Track& track : project->trackList()) {
        if (track.type != TrackType::Bus || track.id == trackId()) {
            continue;
        }
        result << item(QString::number(track.id), track.title.toQString(), track.id == output,
                       trackeditInteraction()->canOutputTo(trackId(), track.id));
    }
    return result;
}

void WaveTrackItem::setOutput(const QString& id)
{
    const TrackId bus = id == MASTER_OUTPUT_ID ? INVALID_TRACK : id.toLongLong();
    trackeditInteraction()->setTrackOutput(trackId(), bus);
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
    // A bus mixes in stereo
    case trackedit::TrackType::Bus:
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
    if (isFocused() && m_recordStreamChannelsMatch) {
        record()->audioInput()->recordSignalChanges().onReceive(this,
                                                                [this](const audioch_t audioChNum, const audio::MeterSignal& meterSignal) {
            setAudioChannelVolumePressure(audioChNum,
                                          meterSignal.peak.pressure);
            setAudioChannelRMS(audioChNum, meterSignal.rms.pressure);
        }, muse::async::Asyncable::Mode::SetReplace);
    } else {
        record()->audioInput()->recordSignalChanges().disconnect(this);
    }
}

bool WaveTrackItem::isAudible() const
{
    return trackType() != TrackType::Label;
}
