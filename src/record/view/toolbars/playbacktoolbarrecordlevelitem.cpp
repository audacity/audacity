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
    record()->audioInput()->recordVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume){
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
    emit recordingChannelsCountChanged();
    // TODO not sure this 'emit' is needed, should we then also emit isMicMeteringOnChanged(); ?
    // emit isInputMonitoringOnChanged();
}

float PlaybackToolBarRecordLevelItem::level() const
{
    return m_level;
}

void PlaybackToolBarRecordLevelItem::setLevel(float newLevel)
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

int PlaybackToolBarRecordLevelItem::recordingChannelsCount() const
{
    return audioDevicesProvider()->currentInputChannelsCount();
}

bool PlaybackToolBarRecordLevelItem::isInputMonitoringOn() const
{
    return recordConfiguration()->isInputMonitoringOn();
}

void PlaybackToolBarRecordLevelItem::setIsInputMonitoringOn(bool enable)
{
    recordConfiguration()->setIsInputMonitoringOn(enable);
}

bool PlaybackToolBarRecordLevelItem::isMicMeteringOn() const
{
    return recordConfiguration()->isMicMeteringOn();
}

void PlaybackToolBarRecordLevelItem::setIsMicMeteringOn(bool enable)
{
    recordConfiguration()->setIsMicMeteringOn(enable);
}

PlaybackMeterStyle::MeterStyle PlaybackToolBarRecordLevelItem::meterStyle() const
{
    return playbackConfiguration()->playbackMeterStyle();
}

void PlaybackToolBarRecordLevelItem::listenMainAudioInput(bool listen)
{
    recordMeterController()->setRecordMeterVisible(listen);
    if (listen) {
        record()->audioInput()->recordSignalChanges().onReceive(this,
                                                                [this](const audioch_t audioChNum, const audio::MeterSignal& meterSignal) {
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
        record()->audioInput()->recordSignalChanges().resetOnReceive(this);
    }
}
