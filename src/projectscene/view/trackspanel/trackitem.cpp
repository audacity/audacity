/*
* Audacity: A Digital Audio Editor
*/

#include "trackitem.h"

#include <QString>

#include "playback/playbacktypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::trackedit;
using namespace au::audio;

static constexpr float BALANCE_SCALING_FACTOR = 100.f;

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
    // m_audioSignalChanges.resetOnReceive(this);
}

void TrackItem::init(const trackedit::Track& track)
{
    m_trackId = track.id;
    m_trackType = track.type;
    m_title = track.title;
    if (m_trackType != trackedit::TrackType::Label) {
        m_outParams.volume = trackPlaybackControl()->volume(m_trackId);
        m_outParams.balance = trackPlaybackControl()->balance(m_trackId);
    }
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

bool TrackItem::forceMute() const
{
    return m_outParams.forceMute;
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

    if (m_outParams.forceMute != newParams.forceMute) {
        m_outParams.forceMute = newParams.forceMute;
        emit forceMuteChanged();
    }
}

// void TrackItem::loadSoloMuteState(const project::IProjectSoloMuteState::SoloMuteState& newState)
// {
//     if (m_outParams.muted != newState.mute) {
//         m_outParams.muted = newState.mute;
//         emit mutedChanged();
//     }

//     if (m_outParams.solo != newState.solo) {
//         m_outParams.solo = newState.solo;
//         emit soloChanged();
//     }
// }

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

void TrackItem::setVolumeLevel(float volumeLevel)
{
    if (qFuzzyCompare(m_outParams.volume, volumeLevel)) {
        return;
    }

    trackPlaybackControl()->setVolume(trackId(), volumeLevel);

    m_outParams.volume = volumeLevel;
    emit volumeLevelChanged(m_outParams.volume);
    emit outputParamsChanged(m_outParams);
}

void TrackItem::setBalance(int balance)
{
    if (m_outParams.balance * BALANCE_SCALING_FACTOR == balance) {
        return;
    }

    trackPlaybackControl()->setBalance(trackId(), balance / BALANCE_SCALING_FACTOR);

    m_outParams.balance = balance / BALANCE_SCALING_FACTOR;
    emit balanceChanged(balance);
    emit outputParamsChanged(m_outParams);
}

void TrackItem::setSolo(bool solo)
{
    if (m_outParams.solo == solo) {
        return;
    }

    m_outParams.solo = solo;

    // project::IProjectSoloMuteState::SoloMuteState soloMuteState;
    // soloMuteState.mute = m_outParams.muted;
    // soloMuteState.solo = m_outParams.solo;

    // emit soloMuteStateChanged(soloMuteState);
    emit soloChanged();

    if (solo && m_outParams.muted) {
        setMuted(false);
    }
}

void TrackItem::setMuted(bool mute)
{
    if (m_outParams.muted == mute) {
        return;
    }

    m_outParams.muted = mute;

    // project::IProjectSoloMuteState::SoloMuteState soloMuteState;
    // soloMuteState.mute = m_outParams.muted;
    // soloMuteState.solo = m_outParams.solo;

    // emit soloMuteStateChanged(soloMuteState);
    emit mutedChanged();

    if (mute && m_outParams.solo) {
        setSolo(false);
    }
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
