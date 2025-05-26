#include "playbackmetermodel.h"

#include "playback/playbacktypes.h"
#include "projectscene/internal/projectsceneuiactions.h"

using namespace au::projectscene;

static const QString TOOLBAR_NAME("playbackToolBar");
static const muse::actions::ActionCode PLAYBACK_LEVEL_CODE("playback-level");

PlaybackMeterModel::PlaybackMeterModel(QObject* parent)
    : QObject(parent)
{
    playback()->audioOutput()->playbackSignalChanges().onReceive(this,
                                                                 [this](const trackedit::audioch_t audioChNum,
                                                                        const audio::MeterSignal& meterSignal) {
        setAudioChannelVolumePressure(audioChNum, meterSignal.peak.pressure);
        setAudioChannelRMS(audioChNum, meterSignal.rms.pressure);
    });

    configuration()->playbackMeterPositionChanged().onNotify(this, [this]() {
        emit meterPositionChanged();
    });

    configuration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        emit meterTypeChanged();
    });

    uiConfiguration()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
        muse::ui::ToolConfig playbackConfig
            = uiConfiguration()->toolConfig(TOOLBAR_NAME, ProjectSceneUiActions::defaultPlaybackToolBarConfig());

        const auto it = std::find_if(playbackConfig.items.begin(), playbackConfig.items.end(),
                                     [](const muse::ui::ToolConfig::Item& item) {
            return item.action == PLAYBACK_LEVEL_CODE;
        });

        if (it != playbackConfig.items.end()) {
            if ((*it).show != visible()) {
                m_visible = (*it).show;
                visibleChanged();
            }
        }
    });

    resetAudioChannelsVolumePressure();
}

float PlaybackMeterModel::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float PlaybackMeterModel::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

void PlaybackMeterModel::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void PlaybackMeterModel::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void PlaybackMeterModel::setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);
    chNum == 0 ? setLeftChannelPressure(clampedValue) : setRightChannelPressure(clampedValue);
}

void PlaybackMeterModel::setAudioChannelRMS(const audio::audioch_t chNum, const float newValue)
{
    float clampedValue = std::clamp(newValue, playback::MIN_DISPLAYED_DBFS, playback::MAX_DISPLAYED_DBFS);
    chNum == 0 ? setLeftChannelRMS(clampedValue) : setRightChannelRMS(clampedValue);
}

void PlaybackMeterModel::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(playback::MIN_DISPLAYED_DBFS);
    setRightChannelPressure(playback::MIN_DISPLAYED_DBFS);
}

float PlaybackMeterModel::leftChannelRMS() const
{
    return m_leftChannelRMS;
}

void PlaybackMeterModel::setLeftChannelRMS(float leftChannelRMS)
{
    if (qFuzzyCompare(m_leftChannelRMS, leftChannelRMS)) {
        return;
    }

    m_leftChannelRMS = leftChannelRMS;
    emit leftChannelRMSChanged(m_leftChannelRMS);
}

float PlaybackMeterModel::rightChannelRMS() const
{
    return m_rightChannelRMS;
}

void PlaybackMeterModel::setRightChannelRMS(float rightChannelRMS)
{
    if (qFuzzyCompare(m_rightChannelRMS, rightChannelRMS)) {
        return;
    }

    m_rightChannelRMS = rightChannelRMS;
    emit rightChannelRMSChanged(m_rightChannelRMS);
}

void PlaybackMeterModel::setMeterStyle(playback::PlaybackMeterStyle::MeterStyle style)
{
    if (meterStyle() == style) {
        return;
    }

    configuration()->setPlaybackMeterStyle(style);
    emit meterStyleChanged();
}

au::playback::PlaybackMeterStyle::MeterStyle PlaybackMeterModel::meterStyle() const
{
    return configuration()->playbackMeterStyle();
}

void PlaybackMeterModel::setMeterType(playback::PlaybackMeterType::MeterType type)
{
    if (meterType() == type) {
        return;
    }

    configuration()->setPlaybackMeterType(type);
    emit meterTypeChanged();
}

au::playback::PlaybackMeterType::MeterType PlaybackMeterModel::meterType() const
{
    return configuration()->playbackMeterType();
}

void PlaybackMeterModel::setMeterPosition(playback::PlaybackMeterPosition::MeterPosition position)
{
    if (meterPosition() == position) {
        return;
    }

    configuration()->setPlaybackMeterPosition(position);
    emit meterPositionChanged();
}

au::playback::PlaybackMeterPosition::MeterPosition PlaybackMeterModel::meterPosition() const
{
    return configuration()->playbackMeterPosition();
}

bool PlaybackMeterModel::visible() const
{
    return m_visible;
}
