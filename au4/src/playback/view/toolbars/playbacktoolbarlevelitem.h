/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKTOOLBARLEVELITEM_H
#define AU_PLAYBACK_PLAYBACKTOOLBARLEVELITEM_H

#include <QString>

#include "modularity/ioc.h"
#include "au3wrap/iau3playback.h"

#include "playbacktoolbarabstractitem.h"

namespace au::playback {
class PlaybackToolBarLevelItem : public PlaybackToolBarAbstractItem
{
    Q_OBJECT

    Q_PROPERTY(int level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)

    muse::Inject<au3::IAu3Playback> playback;

public:
    explicit PlaybackToolBarLevelItem(const muse::ui::UiAction& action, const ItemType& type, QObject* parent = nullptr);

    int level() const;
    void setLevel(int newLevel);

    float leftChannelPressure() const;
    float rightChannelPressure() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setRightChannelPressure(float rightChannelPressure);

signals:
    void levelChanged();
    void leftChannelPressureChanged(float leftChannelPressure);
    void rightChannelPressureChanged(float rightChannelPressure);

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    int m_level = 0;

    float m_leftChannelPressure = 0.0;
    float m_rightChannelPressure = 0.0;
};
}

#endif // AU_PLAYBACK_PLAYBACKTOOLBARLEVELITEM_H
