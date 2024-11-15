/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_IPLAYBACKCONTROLLER_H
#define AU_PLAYBACK_IPLAYBACKCONTROLLER_H

#include "modularity/imoduleinterface.h"
#include "async/notification.h"
#include "async/channel.h"
#include "global/progress.h"
#include "actions/actiontypes.h"

#include "playbacktypes.h"

namespace au::playback {
class IPlaybackController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackController)

public:
    virtual ~IPlaybackController() = default;

    virtual bool isPlayAllowed() const = 0;
    virtual muse::async::Notification isPlayAllowedChanged() const = 0;

    virtual bool isPlaying() const = 0;
    virtual muse::async::Notification isPlayingChanged() const = 0;

    virtual bool isPaused() const = 0;

    virtual void reset() = 0;

    virtual muse::async::Channel<uint32_t> midiTickPlayed() const = 0;

    virtual muse::async::Channel<playback::TrackId> trackAdded() const = 0;
    virtual muse::async::Channel<playback::TrackId> trackRemoved() const = 0;

    // virtual notation::INotationSoloMuteState::SoloMuteState trackSoloMuteState(const engraving::InstrumentTrackId& trackId) const = 0;
    // virtual void setTrackSoloMuteState(const engraving::InstrumentTrackId& trackId,
    // const notation::INotationSoloMuteState::SoloMuteState& state) const = 0;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;

    virtual muse::secs_t totalPlayTime() const = 0;
    virtual muse::async::Notification totalPlayTimeChanged() const = 0;

    virtual muse::Progress loadingProgress() const = 0;
};
}

#endif // AU_PLAYBACK_IPLAYBACKCONTROLLER_H
