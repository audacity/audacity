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
#ifndef MU_UICOMPONENTS_MODELUTILS_H
#define MU_UICOMPONENTS_MODELUTILS_H

#include <QAbstractItemModel>

namespace mu::uicomponents {
class ModelUtils
{
public:
    template<typename Model, typename RowCountChangedSignal>
    inline static void connectRowCountChangedSignal(Model* model, RowCountChangedSignal&& signal)
    {
        QObject::connect(model, &QAbstractItemModel::rowsInserted, model, signal);
        QObject::connect(model, &QAbstractItemModel::rowsRemoved, model, signal);
        QObject::connect(model, &QAbstractItemModel::modelReset, model, signal);
    }
};
}

#endif // MU_UICOMPONENTS_MODELUTILS_H
