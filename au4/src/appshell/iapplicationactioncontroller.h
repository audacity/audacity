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
#ifndef AU_APPSHELL_IAPPLICATIONACTIONCONTROLLER_H
#define AU_APPSHELL_IAPPLICATIONACTIONCONTROLLER_H

#include <QDragEnterEvent>
#include <QDropEvent>
#include <QDragMoveEvent>
#include <QEvent>

#include "modularity/imoduleinterface.h"

namespace au::appshell {
class IApplicationActionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IApplicationActionController)

public:
    virtual ~IApplicationActionController() = default;

    virtual void onDragEnterEvent(QDragEnterEvent* event) = 0;
    virtual void onDragMoveEvent(QDragMoveEvent* event) = 0;
    virtual void onDropEvent(QDropEvent* event) = 0;
};
}

#endif // AU_APPSHELL_IAPPLICATIONACTIONCONTROLLER_H
