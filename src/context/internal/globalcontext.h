/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "../iglobalcontext.h"
#include "playbackstate.h"
#include "record/irecordcontroller.h"

namespace au::context {
class GlobalContext : public au::context::IGlobalContext, public muse::Injectable
{
    muse::Inject<au::record::IRecordController> recordController;

public:

    GlobalContext();

    void setCurrentProject(const au::project::IAudacityProjectPtr& project) override;
    au::project::IAudacityProjectPtr currentProject() const override;
    muse::async::Notification currentProjectChanged() const override;

    au::trackedit::ITrackeditProjectPtr currentTrackeditProject() const override;
    muse::async::Notification currentTrackeditProjectChanged() const override;

    void setPlayer(const au::playback::IPlayerPtr& player) override;
    IPlaybackStatePtr playbackState() const override;

    bool isRecording() const override;
    muse::async::Notification isRecordingChanged() const override;
    muse::secs_t recordPosition() const override;
    muse::async::Channel<muse::secs_t> recordPositionChanged() const override;

private:
    au::project::IAudacityProjectPtr m_currentProject;
    muse::async::Notification m_currentProjectChanged;

    std::shared_ptr<PlaybackState> m_playbackState;
};
}
