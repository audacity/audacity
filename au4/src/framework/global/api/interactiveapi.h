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
#ifndef MU_API_INTERACTIVEAPI_H
#define MU_API_INTERACTIVEAPI_H

#include "api/apiobject.h"

#include "modularity/ioc.h"
#include "iinteractive.h"

namespace mu::api {
class InteractiveApi : public ApiObject
{
    Q_OBJECT

    INJECT(IInteractive, interactive)

public:
    explicit InteractiveApi(IApiEngine* e);

    Q_INVOKABLE void info(const QString& title, const QString& text);

    Q_INVOKABLE void openUrl(const QString& url);
};
}

#endif // MU_API_INTERACTIVEAPI_H
