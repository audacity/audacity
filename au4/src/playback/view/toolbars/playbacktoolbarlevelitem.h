/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "playback/iplayback.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarLevelItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(int level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftRecentPeak READ leftRecentPeak NOTIFY leftRecentPeakChanged FINAL)
    Q_PROPERTY(float leftMaxPeak READ leftMaxPeak NOTIFY leftMaxPeakChanged FINAL)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightRecentPeak READ rightRecentPeak NOTIFY rightRecentPeakChanged FINAL)
    Q_PROPERTY(float rightMaxPeak READ rightMaxPeak NOTIFY rightMaxPeakChanged FINAL)

    muse::Inject<IPlayback> playback;

public:
    explicit PlaybackToolBarLevelItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                      QObject* parent = nullptr);

    int level() const;
    void setLevel(int newLevel);

    float leftChannelPressure() const;
    float leftRecentPeak() const;
    float leftMaxPeak() const;

    float rightChannelPressure() const;
    float rightRecentPeak() const;
    float rightMaxPeak() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftRecentPeak(float newLeftRecentPeak);
    void setLeftMaxPeak(float newLeftMaxPeak);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightRecentPeak(float newRightRecentPeak);
    void setRightMaxPeak(float newRightMaxPeak);

signals:
    void levelChanged();

    void leftChannelPressureChanged(float leftChannelPressure);
    void leftRecentPeakChanged();
    void leftMaxPeakChanged();

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightRecentPeakChanged();
    void rightMaxPeakChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    int m_level = 0;

    float m_leftChannelPressure = 0.0;
    float m_leftRecentPeak = 0.0;
    float m_leftMaxPeak = 0.0;

    float m_rightChannelPressure = 0.0;
    float m_rightRecentPeak = 0.0;
    float m_rightMaxPeak = 0.0;
};
}
