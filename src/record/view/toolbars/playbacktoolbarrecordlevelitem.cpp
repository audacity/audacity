/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarrecordlevelitem.h"

#include "playback/playbacktypes.h"

#include <QVariantMap>

using namespace au::record;
using namespace au::auaudio;
using namespace au::playback;

PlaybackToolBarRecordLevelItem::PlaybackToolBarRecordLevelItem(const muse::ui::UiAction& action,
                                                               const muse::uicomponents::ToolBarItemType::Type type, QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    record()->audioInput()->recordVolumeChanged().onReceive(this, [this](const auaudio::volume_dbfs_t volume){
        m_level = volume;
        emit levelChanged();
    });

    playbackConfiguration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this]() {
        recordingChannelsCountChanged();
    });

    recordConfiguration()->isMicMeteringOnChanged().onNotify(this, [this]() {
        emit isMicMeteringOnChanged();
    });

    recordConfiguration()->isInputMonitoringOnChanged().onNotify(this, [this]() {
        emit isInputMonitoringOnChanged();
    });

    resetAudioChannelsVolumePressure();
}

float PlaybackToolBarRecordLevelItem::level() const
{
    return m_level;
}

void PlaybackToolBarRecordLevelItem::setLevel(const float newLevel)
{
    if (qFuzzyCompare(m_level, newLevel)) {
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

void PlaybackToolBarRecordLevelItem::setLeftChannelPressure(const float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void PlaybackToolBarRecordLevelItem::setRightChannelPressure(const float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void PlaybackToolBarRecordLevelItem::setAudioChannelVolumePressure(const auaudio::audioch_t chNum, const float newValue)
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

void PlaybackToolBarRecordLevelItem::setLeftRecentPeak(const float newLeftRecentPeak)
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

void PlaybackToolBarRecordLevelItem::setLeftMaxPeak(const float newLeftMaxPeak)
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

void PlaybackToolBarRecordLevelItem::setRightRecentPeak(const float newRightRecentPeak)
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

void PlaybackToolBarRecordLevelItem::setRightMaxPeak(const float newRightMaxPeak)
{
    if (qFuzzyCompare(m_rightMaxPeak, newRightMaxPeak)) {
        return;
    }

    m_rightMaxPeak = newRightMaxPeak;
    emit rightMaxPeakChanged();
}

int PlaybackToolBarRecordLevelItem::recordingChannelsCount() const
{
    return audioDevicesProvider()->currentInputChannelsCount();
}

bool PlaybackToolBarRecordLevelItem::isInputMonitoringOn() const
{
    return recordConfiguration()->isInputMonitoringOn();
}

void PlaybackToolBarRecordLevelItem::setIsInputMonitoringOn(const bool enable)
{
    recordConfiguration()->setIsInputMonitoringOn(enable);
}

bool PlaybackToolBarRecordLevelItem::isMicMeteringOn() const
{
    return recordConfiguration()->isMicMeteringOn();
}

void PlaybackToolBarRecordLevelItem::setIsMicMeteringOn(const bool enable)
{
    recordConfiguration()->setIsMicMeteringOn(enable);
}

PlaybackMeterStyle::MeterStyle PlaybackToolBarRecordLevelItem::meterStyle() const
{
    return playbackConfiguration()->playbackMeterStyle();
}

void PlaybackToolBarRecordLevelItem::listenMainAudioInput(const bool listen)
{
    recordMeterController()->setRecordMeterVisible(listen);
    if (listen) {
        record()->audioInput()->recordSignalChanges().onReceive(this,
                                                                [this](const audioch_t audioChNum, const auaudio::MeterSignal& meterSignal) {
            if (meterSignal.peak.pressure < MIN_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum,
                                              MIN_DISPLAYED_DBFS);
            } else if (meterSignal.peak.pressure > MAX_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum, MAX_DISPLAYED_DBFS);
            } else {
                setAudioChannelVolumePressure(audioChNum, meterSignal.peak.pressure);
            }
        });
    } else {
        record()->audioInput()->recordSignalChanges().disconnect(this);
    }
}
