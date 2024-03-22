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
#include "apiregister.h"

#include "log.h"

using namespace mu::api;

struct SingletonApiCreator : public IApiRegister::ICreator
{
    ApiObject* obj = nullptr;
    SingletonApiCreator(ApiObject* o)
        : obj(o) {}

    ApiObject* create(IApiEngine*) { return obj; }
};

void ApiRegister::regApiCreator(const std::string& module, const std::string& api, ICreator* c)
{
    ApiCreator ac;
    auto it = m_creators.find(api);
    if (it != m_creators.end()) {
        ac = it->second;
    }

    IF_ASSERT_FAILED(!ac.c) {
        LOGE() << "already registered creator for api: " << api << ", before creator will deleted";
        delete ac.c;
    }

    ac.module = module;
    ac.c = c;
    m_creators[api] = ac;
}

void ApiRegister::regApiSingltone(const std::string& module, const std::string& api, ApiObject* o)
{
    regApiCreator(module, api, new SingletonApiCreator(o));
}

ApiObject* ApiRegister::createApi(const std::string& api, IApiEngine* e) const
{
    auto it = m_creators.find(api);
    if (it == m_creators.end()) {
        LOGE() << "not registered creator for api: " << api;
        return nullptr;
    }
    return it->second.c->create(e);
}
