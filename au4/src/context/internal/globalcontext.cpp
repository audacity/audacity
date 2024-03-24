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
#include "globalcontext.h"

#include "notation/imasternotation.h"

using namespace mu::context;
using namespace mu::project;
using namespace mu::notation;
using namespace mu::async;

void GlobalContext::setCurrentProject(const INotationProjectPtr& project)
{
    if (m_currentProject == project) {
        return;
    }

    m_currentProject = project;

    INotationPtr notation = project ? project->masterNotation()->notation() : nullptr;
    doSetCurrentNotation(notation);

    m_currentProjectChanged.notify();
    m_currentNotationChanged.notify();
}

INotationProjectPtr GlobalContext::currentProject() const
{
    return m_currentProject;
}

Notification GlobalContext::currentProjectChanged() const
{
    return m_currentProjectChanged;
}

IMasterNotationPtr GlobalContext::currentMasterNotation() const
{
    return m_currentProject ? m_currentProject->masterNotation() : nullptr;
}

Notification GlobalContext::currentMasterNotationChanged() const
{
    //! NOTE Same as project
    return m_currentProjectChanged;
}

void GlobalContext::setCurrentNotation(const INotationPtr& notation)
{
    if (m_currentNotation == notation) {
        return;
    }

    doSetCurrentNotation(notation);
    m_currentNotationChanged.notify();
}

INotationPtr GlobalContext::currentNotation() const
{
    return m_currentNotation;
}

Notification GlobalContext::currentNotationChanged() const
{
    return m_currentNotationChanged;
}

void GlobalContext::doSetCurrentNotation(const INotationPtr& notation)
{
    if (m_currentNotation == notation) {
        return;
    }

    m_currentNotation = notation;

    if (m_currentNotation) {
        m_currentNotation->setIsOpen(true);
    }
}
