/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iglobalcontext.h"
#include "playbackstate.h"

namespace au::context {
class GlobalContext : public au::context::IGlobalContext
{
public:

    GlobalContext();

    void setCurrentProject(const au::project::IAudacityProjectPtr& project) override;
    au::project::IAudacityProjectPtr currentProject() const override;
    muse::async::Notification currentProjectChanged() const override;

    au::processing::ProcessingProjectPtr currentProcessingProject() const override;
    muse::async::Notification currentProcessingProjectChanged() const override;

    void setPlayer(const au::playback::IPlayerPtr& player) override;
    IPlaybackStatePtr playbackState() const override;

private:
    au::project::IAudacityProjectPtr m_currentProject;
    muse::async::Notification m_currentProjectChanged;

    std::shared_ptr<PlaybackState> m_playbackState;
};
}
