/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "globalcontext.h"

using namespace au::context;
using namespace au::project;

GlobalContext::GlobalContext()
{
    m_playbackState = std::make_shared<PlaybackState>();
}

void GlobalContext::setCurrentProject(const IAudacityProjectPtr& project)
{
    if (m_currentProject == project) {
        return;
    }

    m_currentProject = project;
    m_currentProjectChanged.notify();
}

IAudacityProjectPtr GlobalContext::currentProject() const
{
    return m_currentProject;
}

muse::async::Notification GlobalContext::currentProjectChanged() const
{
    return m_currentProjectChanged;
}

au::trackedit::ITrackeditProjectPtr GlobalContext::currentTrackeditProject() const
{
    return m_currentProject ? m_currentProject->trackeditProject() : nullptr;
}

muse::async::Notification GlobalContext::currentTrackeditProjectChanged() const
{
    //! NOTE Same as main project
    return m_currentProjectChanged;
}

void GlobalContext::setPlayer(const au::playback::IPlayerPtr& player)
{
    m_playbackState->setPlayer(player);
}

IPlaybackStatePtr GlobalContext::playbackState() const
{
    return m_playbackState;
}

bool GlobalContext::isRecording() const
{
    return recordController()->isRecording();
}

muse::async::Notification GlobalContext::isRecordingChanged() const
{
    return recordController()->isRecordingChanged();
}

muse::secs_t GlobalContext::recordPosition() const
{
    return recordController()->recordPosition();
}

muse::async::Channel<muse::secs_t> GlobalContext::recordPositionChanged() const
{
    return recordController()->recordPositionChanged();
}
