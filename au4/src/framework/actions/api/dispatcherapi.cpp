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
#include "dispatcherapi.h"

#include "log.h"

using namespace mu::api;
using namespace mu::actions;

DispatcherApi::DispatcherApi(IApiEngine* e)
    : ApiObject(e)
{
}

void DispatcherApi::dispatch(const QString& action, const QVariantList& args)
{
    ActionData data;
    int index = 0;

#ifdef MU_QT5_COMPAT
    for (const QVariant& arg : args) {
        switch (arg.type()) {
        case QVariant::Int: data.setArg<int>(index, arg.value<int>());
            break;
        default: {
            LOGE() << "unknown type: " << arg.typeName();
            continue;
        } break;
        }

        ++index;
    }
#else
    for (const QVariant& arg : args) {
        switch (arg.typeId()) {
        case QMetaType::Int: data.setArg<int>(index, arg.value<int>());
            break;
        default: {
            LOGE() << "unknown type: " << arg.typeName();
            continue;
        } break;
        }

        ++index;
    }
#endif

    dispatcher()->dispatch(action.toStdString(), data);
}
