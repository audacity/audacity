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
#ifndef MU_API_APIREGISTER_H
#define MU_API_APIREGISTER_H

#include <map>

#include "../iapiregister.h"

namespace mu::api {
class ApiRegister : public IApiRegister
{
public:
    ApiRegister() = default;

    void regApiCreator(const std::string& module, const std::string& api, ICreator* c) override;
    void regApiSingltone(const std::string& module, const std::string& api, ApiObject* o) override;
    ApiObject* createApi(const std::string& api, IApiEngine* e) const override;

private:
    struct ApiCreator {
        std::string module;
        ICreator* c = nullptr;
    };

    std::map<std::string, ApiCreator> m_creators;
};
}

#endif // MU_API_APIREGISTER_H
