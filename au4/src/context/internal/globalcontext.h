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

    void setCurrentProject(const project::INotationProjectPtr& project) override;
    project::INotationProjectPtr currentProject() const override;
    async::Notification currentProjectChanged() const override;

    notation::IMasterNotationPtr currentMasterNotation() const override;
    async::Notification currentMasterNotationChanged() const override;

    void setCurrentNotation(const notation::INotationPtr& notation) override;
    notation::INotationPtr currentNotation() const override;
    async::Notification currentNotationChanged() const override;

private:
    void doSetCurrentNotation(const notation::INotationPtr& notation);

    project::INotationProjectPtr m_currentProject;
    async::Notification m_currentProjectChanged;

    notation::INotationPtr m_currentNotation;
    async::Notification m_currentNotationChanged;
};
}

#endif // MU_CONTEXT_GLOBALCONTEXT_H
