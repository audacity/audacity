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
#ifndef MU_DOCK_IDOCKWINDOW_H
#define MU_DOCK_IDOCKWINDOW_H

#include "internal/dockbase.h"
#include "async/channel.h"

#include <QString>

class QPoint;

namespace mu::dock {
class DockPageView;
class IDockWindow
{
public:
    virtual ~IDockWindow() = default;

    virtual bool isDockOpen(const QString& dockName) const = 0;
    virtual void setDockOpen(const QString& dockName, bool open) = 0;
    virtual void toggleDock(const QString& dockName) = 0;

    virtual async::Channel<QStringList> docksOpenStatusChanged() const = 0;

    virtual bool isDockFloating(const QString& dockName) const = 0;
    virtual void toggleDockFloating(const QString& dockName) = 0;

    virtual DockPageView* currentPage() const = 0;
    virtual QQuickItem& asItem() const = 0;

    virtual void restoreDefaultLayout() = 0;
};
}

#endif // MU_DOCK_IDOCKWINDOW_H
