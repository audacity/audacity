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

#ifndef MU_UI_IUIENGINE_H
#define MU_UI_IUIENGINE_H

#include <QString>

#include "modularity/imoduleinterface.h"

class QQmlEngine;

namespace mu::ui {
class IUiEngine : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IUiEngine)

public:
    virtual ~IUiEngine() {}

    virtual void updateTheme() = 0;
    virtual QQmlEngine* qmlEngine() const = 0;
    virtual void clearComponentCache() = 0;

    virtual void addSourceImportPath(const QString& path) = 0;
};
}

#endif // MU_UI_UIENGINEMODULE_H
