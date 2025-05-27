#include "playbackmeterpanelmodel.h"

#include "playback/playbacktypes.h"

using namespace au::projectscene;

PlaybackMeterPanelModel::PlaybackMeterPanelModel(QObject* parent)
    : QObject(parent)
{
    playback()->audioOutput()->playbackSignalChanges().onReceive(this,
                                                                 [this](const trackedit::audioch_t audioChNum,
                                                                        const audio::MeterSignal& meterSignal) {
        setAudioChannelVolumePressure(audioChNum, meterSignal.peak.pressure);
        setAudioChannelRMS(audioChNum, meterSignal.rms.pressure);
    });

    playback()->audioOutput()->playbackVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume){
        m_level = volume;
        emit levelChanged();
    });

    configuration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        emit meterTypeChanged();
    });

    resetAudioChannelsVolumePressure();
}

float PlaybackMeterPanelModel::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float PlaybackMeterPanelModel::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

float PlaybackMeterPanelModel::level() const
{
    return m_level;
}

void PlaybackMeterPanelModel::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void PlaybackMeterPanelModel::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void PlaybackMeterPanelModel::setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);
    chNum == 0 ? setLeftChannelPressure(clampedValue) : setRightChannelPressure(clampedValue);
}

void PlaybackMeterPanelModel::setAudioChannelRMS(const audio::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);
    chNum == 0 ? setLeftChannelRMS(clampedValue) : setRightChannelRMS(clampedValue);
}

void PlaybackMeterPanelModel::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(playback::MIN_DISPLAYED_DBFS);
    setRightChannelPressure(playback::MIN_DISPLAYED_DBFS);
}

float PlaybackMeterPanelModel::leftChannelRMS() const
{
    return m_leftChannelRMS;
}

void PlaybackMeterPanelModel::setLeftChannelRMS(float leftChannelRMS)
{
    if (qFuzzyCompare(m_leftChannelRMS, leftChannelRMS)) {
        return;
    }

    m_leftChannelRMS = leftChannelRMS;
    emit leftChannelRMSChanged(m_leftChannelRMS);
}

float PlaybackMeterPanelModel::rightChannelRMS() const
{
    return m_rightChannelRMS;
}

void PlaybackMeterPanelModel::setRightChannelRMS(float rightChannelRMS)
{
    if (qFuzzyCompare(m_rightChannelRMS, rightChannelRMS)) {
        return;
    }

    m_rightChannelRMS = rightChannelRMS;
    emit rightChannelRMSChanged(m_rightChannelRMS);
}

au::playback::PlaybackMeterStyle::MeterStyle PlaybackMeterPanelModel::meterStyle() const
{
    return configuration()->playbackMeterStyle();
}

au::playback::PlaybackMeterType::MeterType PlaybackMeterPanelModel::meterType() const
{
    return configuration()->playbackMeterType();
}

au::playback::PlaybackMeterPosition::MeterPosition PlaybackMeterPanelModel::meterPosition() const
{
    return configuration()->playbackMeterPosition();
}

void PlaybackMeterPanelModel::positionChangeRequested(playback::PlaybackMeterPosition::MeterPosition position)
{
    if (configuration()->playbackMeterPosition() == position) {
        return;
    }

    configuration()->setPlaybackMeterPosition(position);
}

void PlaybackMeterPanelModel::styleChangeRequested(playback::PlaybackMeterStyle::MeterStyle style)
{
    if (configuration()->playbackMeterStyle() == style) {
        return;
    }

    configuration()->setPlaybackMeterStyle(style);
}

void PlaybackMeterPanelModel::typeChangeRequested(playback::PlaybackMeterType::MeterType type)
{
    if (configuration()->playbackMeterType() == type) {
        return;
    }

    configuration()->setPlaybackMeterType(type);
}

void PlaybackMeterPanelModel::volumeLevelChangeRequested(float level)
{
    if (qFuzzyCompare(m_level, level)) {
        return;
    }

    playback()->audioOutput()->setPlaybackVolume(level);
}
