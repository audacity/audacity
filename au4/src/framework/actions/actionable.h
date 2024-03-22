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
#ifndef MU_ACTIONS_ACTIONABLE_H
#define MU_ACTIONS_ACTIONABLE_H

#include "iactionsdispatcher.h"

namespace mu::actions {
class Actionable
{
public:
    virtual ~Actionable()
    {
        if (m_dispatcher) {
            m_dispatcher->unReg(this);
        }
    }

    virtual bool canReceiveAction(const ActionCode&) const { return true; }

    inline void setDispatcher(IActionsDispatcher* dispatcher)
    {
        m_dispatcher = dispatcher;
    }

private:

    IActionsDispatcher* m_dispatcher = nullptr;
};
}

#endif // MU_ACTIONS_ACTIONABLE_H
