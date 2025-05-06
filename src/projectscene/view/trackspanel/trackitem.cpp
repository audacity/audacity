/*
* Audacity: A Digital Audio Editor
*/

#include "trackitem.h"

#include <QString>

#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;
using namespace au::audio;

static constexpr float BALANCE_SCALING_FACTOR = 100.f;
static constexpr float MIN_ALLOWED_PRESSURE = -60.f;
static constexpr float MAX_ALLOWED_PRESSURE = 0.f;

static const std::string TRACK_ID_KEY("trackId");
static const std::string RESOURCE_ID_KEY("resourceId");
static const std::string CHAIN_ORDER_KEY("chainOrder");

TrackItem::TrackItem(QObject* parent)
    : QObject(parent),
    m_leftChannelPressure(playback::MIN_DISPLAYED_DBFS),
    m_rightChannelPressure(playback::MIN_DISPLAYED_DBFS)
{
    connect(this, &TrackItem::mutedChanged, this, [this]() {
        if (muted()) {
            resetAudioChannelsVolumePressure();
        }
    });
}

TrackItem::~TrackItem()
{
    m_playbackTrackSignalChanged.close();
}

void TrackItem::init(const trackedit::Track& track)
{
    m_trackId = track.id;

    m_playbackTrackSignalChanged = playback()->audioOutput()->playbackTrackSignalChanges(m_trackId);
    m_playbackTrackSignalChanged.onReceive(this, [this](au::audio::audioch_t channel, const au::audio::AudioSignalVal& newValue) {
        const float newPressure = std::clamp(newValue.pressure, MIN_ALLOWED_PRESSURE, MAX_ALLOWED_PRESSURE);
        if (channel == 0) {
            if (m_leftChannelPressure == newPressure) {
                return;
            }
            m_leftChannelPressure = newPressure;
            emit leftChannelPressureChanged(newPressure);
        } else {
            if (m_rightChannelPressure == newPressure) {
                return;
            }
            m_rightChannelPressure = newPressure;
            emit rightChannelPressureChanged(newPressure);
        }
    });

    if (m_title != track.title) {
        m_title = track.title;
        emit titleChanged(m_title);
    }

    if (m_trackType != track.type) {
        m_trackType = track.type;
        emit channelCountChanged();
    }

    const auto ctrl = trackPlaybackControl();
    if (m_trackType != trackedit::TrackType::Label) {
        m_outParams.volume = ctrl->volume(m_trackId);
        m_outParams.balance = ctrl->balance(m_trackId);
        m_outParams.solo = ctrl->solo(m_trackId);
        m_outParams.muted = ctrl->muted(m_trackId);
    }
    ctrl->muteOrSoloChanged().onReceive(this, [this](long trackId) {
        if (trackId != m_trackId) {
            return;
        }
        auto paramChanged = false;
        if (m_outParams.solo != trackPlaybackControl()->solo(m_trackId)) {
            m_outParams.solo = !m_outParams.solo;
            paramChanged = true;
            emit soloChanged();
        }
        if (m_outParams.muted != trackPlaybackControl()->muted(m_trackId)) {
            m_outParams.muted = !m_outParams.muted;
            paramChanged = true;
            emit mutedChanged();
        }
        if (paramChanged) {
            emit outputParamsChanged(m_outParams);
        }
    });
}

au::trackedit::TrackId TrackItem::trackId() const
{
    return m_trackId;
}

QVariant TrackItem::trackId_property() const
{
    return QVariant::fromValue(m_trackId);
}

QString TrackItem::title() const
{
    return m_title;
}

int TrackItem::channelCount() const
{
    switch (m_trackType) {
    case trackedit::TrackType::Mono:
        return 1;
    case trackedit::TrackType::Stereo:
        return 2;
    default:
        return 0;
    }
}

float TrackItem::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float TrackItem::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

float TrackItem::volumeLevel() const
{
    return m_outParams.volume;
}

int TrackItem::balance() const
{
    return m_outParams.balance * BALANCE_SCALING_FACTOR;
}

bool TrackItem::solo() const
{
    return m_outParams.solo;
}

bool TrackItem::muted() const
{
    return m_outParams.muted;
}

void TrackItem::loadOutputParams(const audio::AudioOutputParams& newParams)
{
    if (!muse::RealIsEqual(m_outParams.volume, newParams.volume)) {
        m_outParams.volume = newParams.volume;
        emit volumeLevelChanged(newParams.volume);
    }

    if (!muse::RealIsEqual(m_outParams.balance, newParams.balance)) {
        m_outParams.balance = newParams.balance;
        emit balanceChanged(newParams.balance);
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

// void TrackItem::subscribeOnAudioSignalChanges(AudioSignalChanges&& audioSignalChanges)
// {
//     m_audioSignalChanges = audioSignalChanges;

//     m_audioSignalChanges.onReceive(this, [this](const audioch_t audioChNum, const AudioSignalVal& newValue) {
//         //!Note There should be no signal changes when the mixer channel is muted.
//         //!     But some audio signal changes still might be "on the way" from the times when the mixer channel wasn't muted
//         //!     So that we have to just ignore them
//         if (muted()) {
//             return;
//         }

//         if (newValue.pressure < MIN_DISPLAYED_DBFS) {
//             setAudioChannelVolumePressure(audioChNum, MIN_DISPLAYED_DBFS);
//         } else if (newValue.pressure > MAX_DISPLAYED_DBFS) {
//             setAudioChannelVolumePressure(audioChNum, MAX_DISPLAYED_DBFS);
//         } else {
//             setAudioChannelVolumePressure(audioChNum, newValue.pressure);
//         }
//     });
// }

void TrackItem::setTitle(QString title)
{
    if (m_title == title) {
        return;
    }

    m_title = title;
    trackeditInteraction()->changeTrackTitle(m_trackId, title);
    emit titleChanged(m_title);
}

void TrackItem::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void TrackItem::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void TrackItem::setVolumeLevel(float volumeLevel, bool completed)
{
    trackPlaybackControl()->setVolume(trackId(), volumeLevel, completed);

    if (m_outParams.volume == volumeLevel) {
        return;
    }
    m_outParams.volume = volumeLevel;
    emit volumeLevelChanged(m_outParams.volume);
    emit outputParamsChanged(m_outParams);
}

void TrackItem::setBalance(int balance, bool completed)
{
    const float scaled = balance / BALANCE_SCALING_FACTOR;
    trackPlaybackControl()->setBalance(trackId(), scaled, completed);

    if (m_outParams.balance == scaled) {
        return;
    }
    m_outParams.balance = scaled;
    emit balanceChanged(balance);
    emit outputParamsChanged(m_outParams);
}

void TrackItem::setSolo(bool solo)
{
    trackPlaybackControl()->setSolo(trackId(), solo);
}

void TrackItem::setMuted(bool mute)
{
    trackPlaybackControl()->setMuted(trackId(), mute);
}

void TrackItem::setAudioChannelVolumePressure(const trackedit::audioch_t chNum, const float newValue)
{
    if (chNum == 0) {
        setLeftChannelPressure(newValue);
    } else {
        setRightChannelPressure(newValue);
    }
}

void TrackItem::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(playback::MIN_DISPLAYED_DBFS);
    setRightChannelPressure(playback::MIN_DISPLAYED_DBFS);
}

bool TrackItem::outputOnly() const
{
    return m_outputOnly;
}

const AudioOutputParams& TrackItem::outputParams() const
{
    return m_outParams;
}

bool TrackItem::isSelected() const
{
    return m_isSelected;
}

void TrackItem::setIsSelected(bool selected)
{
    if (m_isSelected == selected) {
        return;
    }

    m_isSelected = selected;
    emit isSelectedChanged();
}
