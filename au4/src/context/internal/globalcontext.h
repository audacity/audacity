/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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
#ifndef MU_CONTEXT_GLOBALCONTEXT_H
#define MU_CONTEXT_GLOBALCONTEXT_H

#include "../iglobalcontext.h"

namespace mu::context {
class GlobalContext : public IGlobalContext
{
public:

    void setCurrentProject(const project::IAudacityProjectPtr& project) override;
    project::IAudacityProjectPtr currentProject() const override;
    async::Notification currentProjectChanged() const override;

private:
    project::IAudacityProjectPtr m_currentProject;
    async::Notification m_currentProjectChanged;
};
}

#endif // MU_CONTEXT_GLOBALCONTEXT_H
