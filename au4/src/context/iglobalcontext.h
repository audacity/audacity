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
#ifndef AU_CONTEXT_IGLOBALCONTEXT_H
#define AU_CONTEXT_IGLOBALCONTEXT_H

#include "modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "project/iaudacityproject.h"
#include "playback/iplayer.h"

namespace au::context {
class IGlobalContext : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::context::IGlobalContext)

public:
    virtual ~IGlobalContext() = default;

    virtual void setCurrentProject(const au::project::IAudacityProjectPtr& project) = 0;
    virtual au::project::IAudacityProjectPtr currentProject() const = 0;
    virtual muse::async::Notification currentProjectChanged() const = 0;

    virtual au::processing::ProcessingProjectPtr currentProcessingProject() const = 0;
    virtual muse::async::Notification currentProcessingProjectChanged() const = 0;

    virtual void setPlayer(const au::playback::IPlayerPtr& player) = 0;
    virtual au::playback::IPlayerPtr player() const = 0;
    virtual muse::async::Notification playerChanged() const = 0;
};
}

#endif // AU_CONTEXT_IGLOBALCONTEXT_H
