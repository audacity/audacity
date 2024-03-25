#ifndef AU_PROJECTSCENE_PLAYBACKCONTROLLER_H
#define AU_PROJECTSCENE_PLAYBACKCONTROLLER_H

#include "../iplaybackcontroller.h"

namespace au::projectscene {
class PlaybackController : public IPlaybackController
{
public:
    PlaybackController() = default;

    void play() override;
    void stop() override;
    void rewind() override;
};
}

#endif // AU_PROJECTSCENE_PLAYBACKCONTROLLER_H
