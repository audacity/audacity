/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iglobalcontext.h"

namespace au::context {
class GlobalContext : public au::context::IGlobalContext
{
public:

    void setCurrentProject(const au::project::IAudacityProjectPtr& project) override;
    au::project::IAudacityProjectPtr currentProject() const override;
    muse::async::Notification currentProjectChanged() const override;

    au::processing::ProcessingProjectPtr currentProcessingProject() const override;
    muse::async::Notification currentProcessingProjectChanged() const override;

    void setPlayer(const au::playback::IPlayerPtr& player) override;
    au::playback::IPlayerPtr player() const override;
    muse::async::Notification playerChanged() const override;

private:
    au::project::IAudacityProjectPtr m_currentProject;
    muse::async::Notification m_currentProjectChanged;

    au::playback::IPlayerPtr m_player;
    muse::async::Notification m_playerChanged;
};
}
