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
#ifndef MU_ACTIONS_ACTIONSDISPATCHER_H
#define MU_ACTIONS_ACTIONSDISPATCHER_H

#include <map>

#include "../iactionsdispatcher.h"

namespace mu::actions {
class ActionsDispatcher : public IActionsDispatcher
{
public:
    ActionsDispatcher() = default;
    ~ActionsDispatcher() override;

    void dispatch(const ActionCode& actionCode) override;
    void dispatch(const ActionCode& actionCode, const ActionData& data) override;

    void unReg(Actionable* client) override;
    void reg(Actionable* client, const ActionCode& actionCode, const ActionCallBackWithNameAndData& call) override;

private:

    using CallBacks = std::map<ActionCode, ActionCallBackWithNameAndData>;
    using Clients = std::map<Actionable*, CallBacks>;

    std::map<ActionCode, Clients > m_clients;
};
}

#endif // MU_ACTIONS_ACTIONSDISPATCHER_H
