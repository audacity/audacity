/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "modularity/imoduleinterface.h"

#include "draw/types/color.h"
#include "playbacktypes.h"
#include "trackedit/trackedittypes.h"

namespace au::playback {
class IPlaybackConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackConfiguration)

public:
    virtual ~IPlaybackConfiguration() = default;

    virtual muse::draw::Color playColor() const = 0;

    virtual std::vector<std::string> playbackQualityList() const = 0;
    virtual std::string currentPlaybackQuality() const = 0;
    virtual void setPlaybackQuality(const std::string& quality) = 0;
    virtual muse::async::Notification playbackQualityChanged() const = 0;

    virtual std::vector<std::string> ditheringList() const = 0;
    virtual std::string currentDithering() const = 0;
    virtual void setDithering(const std::string& dithering) = 0;
    virtual muse::async::Notification ditheringChanged() const = 0;

    virtual TracksBehaviors::SoloBehavior currentSoloBehavior() const = 0;
    virtual void setSoloBehavior(TracksBehaviors::SoloBehavior behavior) = 0;
    virtual muse::async::Notification soloBehaviorChanged() const = 0;

    virtual trackedit::secs_t shortSkip() const = 0;
    virtual void setShortSkip(trackedit::secs_t seconds) = 0;
    virtual muse::async::Notification shortSkipChanged() const = 0;

    virtual trackedit::secs_t longSkip() const = 0;
    virtual void setLongSkip(trackedit::secs_t seconds) = 0;
    virtual muse::async::Notification longSkipChanged() const = 0;
};
}
