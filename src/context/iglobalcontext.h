/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "project/iaudacityproject.h"
#include "playback/iplayer.h"
#include "iplaybackstate.h"

namespace au::context {
class IGlobalContext : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::context::IGlobalContext)

public:
    virtual ~IGlobalContext() = default;

    virtual void setCurrentProject(const au::project::IAudacityProjectPtr& project) = 0;
    virtual au::project::IAudacityProjectPtr currentProject() const = 0;
    virtual muse::async::Notification currentProjectChanged() const = 0;

    virtual au::trackedit::ITrackeditProjectPtr currentTrackeditProject() const = 0;
    virtual muse::async::Notification currentTrackeditProjectChanged() const = 0;

    virtual void setPlayer(const au::playback::IPlayerPtr& player) = 0;
    virtual IPlaybackStatePtr playbackState() const = 0;

    virtual bool isRecording() const = 0;
    virtual muse::async::Notification isRecordingChanged() const = 0;

    virtual muse::secs_t recordPosition() const = 0;
    virtual muse::async::Channel<muse::secs_t> recordPositionChanged() const = 0;
};
}
