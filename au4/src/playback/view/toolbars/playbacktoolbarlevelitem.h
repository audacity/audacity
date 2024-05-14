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

    muse::Inject<au3::IAu3Playback> playback;

public:
    explicit PlaybackToolBarLevelItem(const muse::ui::UiAction& action, const ItemType& type, QObject* parent = nullptr);

    int level() const;
    void setLevel(int newLevel);

signals:
    void levelChanged();

private:
    int m_level = 0;
};
}

#endif // AU_PLAYBACK_PLAYBACKTOOLBARLEVELITEM_H
