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
#include "actionsdispatcher.h"

#include "actionable.h"

#include "log.h"

using namespace mu::actions;

ActionsDispatcher::~ActionsDispatcher()
{
    for (auto it = m_clients.begin(); it != m_clients.end(); ++it) {
        Clients& clients = it->second;
        for (auto cit = clients.begin(); cit != clients.end(); ++cit) {
            Actionable* client = cit->first;
            client->setDispatcher(nullptr);
        }
    }
}

void ActionsDispatcher::dispatch(const ActionCode& actionCode)
{
    static ActionData dummy;
    dispatch(actionCode, dummy);
}

void ActionsDispatcher::dispatch(const ActionCode& actionCode, const ActionData& data)
{
    auto it = m_clients.find(actionCode);
    if (it == m_clients.end()) {
        LOGW() << "not a registered action: " << actionCode;
        return;
    }

    int canReceiveCount = 0;
    const Clients& clients = it->second;
    for (auto cit = clients.cbegin(); cit != clients.cend(); ++cit) {
        const Actionable* client = cit->first;
        if (client->canReceiveAction(actionCode)) {
            ++canReceiveCount;
            const CallBacks& callbacks = cit->second;
            auto cbit = callbacks.find(actionCode);
            IF_ASSERT_FAILED(cbit != callbacks.end()) {
                continue;
            }

            const ActionCallBackWithNameAndData& callback = cbit->second;
            LOGI() << "try call action: " << actionCode;
            callback(actionCode, data);
        }
    }

    if (canReceiveCount == 0) {
        LOGI() << "no one can handle the action: " << actionCode;
    } else if (canReceiveCount > 1) {
        LOGW() << "More than one client can handle the action, this is not a typical situation.";
    }
}

void ActionsDispatcher::unReg(Actionable* client)
{
    for (auto it = m_clients.begin(); it != m_clients.end(); ++it) {
        Clients& clients = it->second;
        clients.erase(client);
    }
    client->setDispatcher(nullptr);
}

void ActionsDispatcher::reg(Actionable* client, const ActionCode& actionCode, const ActionCallBackWithNameAndData& call)
{
    client->setDispatcher(this);

    Clients& clients = m_clients[actionCode];
    CallBacks& callbacks = clients[client];
    callbacks.insert({ actionCode, call });
}
