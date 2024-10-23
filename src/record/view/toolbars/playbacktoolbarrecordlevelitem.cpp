/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarrecordlevelitem.h"

#include "playback/playbacktypes.h"

#include <QVariantMap>

using namespace au::record;
using namespace au::audio;
using namespace au::playback;

PlaybackToolBarRecordLevelItem::PlaybackToolBarRecordLevelItem(const muse::ui::UiAction& action,
                                                               muse::uicomponents::ToolBarItemType::Type type, QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_active = recordController()->isRecording();
    });

    record()->audioInput()->recordVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume){
        m_level = volume;
        emit levelChanged();
    });

    record()->audioInput()->recordSignalChanges()
    .onResolve(this, [this](muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> signalVal) {
        signalVal.onReceive(this, [this](const audioch_t audioChNum, const audio::AudioSignalVal& newValue) {
            if (!m_active) {
                return;
            }

            if (newValue.pressure < MIN_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum, MIN_DISPLAYED_DBFS);
            } else if (newValue.pressure > MAX_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum, MAX_DISPLAYED_DBFS);
            } else {
                setAudioChannelVolumePressure(audioChNum, newValue.pressure);
            }
        });
    });

    resetAudioChannelsVolumePressure();
}

int PlaybackToolBarRecordLevelItem::level() const
{
    return m_level;
}

void PlaybackToolBarRecordLevelItem::setLevel(int newLevel)
{
    if (m_level == newLevel) {
        return;
    }

    record()->audioInput()->setRecordVolume(newLevel);
}

float PlaybackToolBarRecordLevelItem::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float PlaybackToolBarRecordLevelItem::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

void PlaybackToolBarRecordLevelItem::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void PlaybackToolBarRecordLevelItem::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void PlaybackToolBarRecordLevelItem::setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue)
{
    if (chNum == 0) {
        setLeftChannelPressure(newValue);
    } else {
        setRightChannelPressure(newValue);
    }
}

void PlaybackToolBarRecordLevelItem::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(MIN_DISPLAYED_DBFS);
    setRightChannelPressure(MIN_DISPLAYED_DBFS);
}

float PlaybackToolBarRecordLevelItem::leftRecentPeak() const
{
    return m_leftRecentPeak;
}

void PlaybackToolBarRecordLevelItem::setLeftRecentPeak(float newLeftRecentPeak)
{
    if (qFuzzyCompare(m_leftRecentPeak, newLeftRecentPeak)) {
        return;
    }

    m_leftRecentPeak = newLeftRecentPeak;
    emit leftRecentPeakChanged();
}

float PlaybackToolBarRecordLevelItem::leftMaxPeak() const
{
    return m_leftMaxPeak;
}

void PlaybackToolBarRecordLevelItem::setLeftMaxPeak(float newLeftMaxPeak)
{
    if (qFuzzyCompare(m_leftMaxPeak, newLeftMaxPeak)) {
        return;
    }

    m_leftMaxPeak = newLeftMaxPeak;
    emit leftMaxPeakChanged();
}

float PlaybackToolBarRecordLevelItem::rightRecentPeak() const
{
    return m_rightRecentPeak;
}

void PlaybackToolBarRecordLevelItem::setRightRecentPeak(float newRightRecentPeak)
{
    if (qFuzzyCompare(m_rightRecentPeak, newRightRecentPeak)) {
        return;
    }

    m_rightRecentPeak = newRightRecentPeak;
    emit rightRecentPeakChanged();
}

float PlaybackToolBarRecordLevelItem::rightMaxPeak() const
{
    return m_rightMaxPeak;
}

void PlaybackToolBarRecordLevelItem::setRightMaxPeak(float newRightMaxPeak)
{
    if (qFuzzyCompare(m_rightMaxPeak, newRightMaxPeak)) {
        return;
    }

    m_rightMaxPeak = newRightMaxPeak;
    emit rightMaxPeakChanged();
}
