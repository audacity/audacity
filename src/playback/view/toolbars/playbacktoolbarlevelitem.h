/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackcontroller.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarLevelItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(float level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)
    Q_PROPERTY(float leftRecentPeak READ leftRecentPeak NOTIFY leftRecentPeakChanged FINAL)
    Q_PROPERTY(float leftMaxPeak READ leftMaxPeak NOTIFY leftMaxPeakChanged FINAL)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)
    Q_PROPERTY(float rightRecentPeak READ rightRecentPeak NOTIFY rightRecentPeakChanged FINAL)
    Q_PROPERTY(float rightMaxPeak READ rightMaxPeak NOTIFY rightMaxPeakChanged FINAL)

    Q_PROPERTY(int meterSize READ meterSize WRITE setMeterSize NOTIFY meterSizeChanged FINAL)

    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)

    muse::Inject<IPlayback> playback;
    muse::Inject<IPlaybackConfiguration> configuration;
    muse::Inject<IPlaybackController> controller;

public:
    explicit PlaybackToolBarLevelItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                      QObject* parent = nullptr);

    float level() const;
    void setLevel(float newLevel);

    float leftChannelPressure() const;
    float leftChannelRMS() const;
    float leftRecentPeak() const;
    float leftMaxPeak() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;
    float rightRecentPeak() const;
    float rightMaxPeak() const;

    int meterSize() const;

    bool isPlaying() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);
    void setLeftRecentPeak(float newLeftRecentPeak);
    void setLeftMaxPeak(float newLeftMaxPeak);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightChannelRMS(float rightChannelRMS);
    void setRightRecentPeak(float newRightRecentPeak);
    void setRightMaxPeak(float newRightMaxPeak);

    void setMeterSize(int size);

signals:
    void levelChanged();

    void leftChannelPressureChanged(float leftChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);
    void leftRecentPeakChanged();
    void leftMaxPeakChanged();

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightChannelRMSChanged(float rightChannelRMS);
    void rightRecentPeakChanged();
    void rightMaxPeakChanged();

    void meterSizeChanged();

    void isPlayingChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    float m_level = 0;

    float m_leftChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_leftChannelRMS = playback::MIN_DISPLAYED_DBFS;
    float m_leftRecentPeak = playback::MIN_DISPLAYED_DBFS;
    float m_leftMaxPeak = playback::MIN_DISPLAYED_DBFS;

    float m_rightChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelRMS = playback::MIN_DISPLAYED_DBFS;
    float m_rightRecentPeak = playback::MIN_DISPLAYED_DBFS;
    float m_rightMaxPeak = playback::MIN_DISPLAYED_DBFS;
};
}
