/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#ifndef MU_API_PROCESSAPI_H
#define MU_API_PROCESSAPI_H

#include <QString>

#include "api/apiobject.h"

#include "modularity/ioc.h"
#include "global/iprocess.h"

namespace mu::api {
class ProcessApi : public ApiObject
{
    Q_OBJECT

    INJECT(IProcess, process)

public:
    explicit ProcessApi(IApiEngine* e);

    Q_INVOKABLE int execute(const QString& program, const QStringList& arguments = QStringList());
    Q_INVOKABLE bool startDetached(const QString& program, const QStringList& arguments = QStringList());
};
}

#endif // MU_API_PROCESSAPI_H
