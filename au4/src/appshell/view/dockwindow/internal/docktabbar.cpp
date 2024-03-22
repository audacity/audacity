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

#include "docktabbar.h"

using namespace mu::dock;

DockTabBar::DockTabBar(KDDockWidgets::TabWidget* parent)
    : KDDockWidgets::TabBarQuick(parent)
{
}

bool DockTabBar::event(QEvent* event)
{
    switch (event->type()) {
    //! NOTE: see https://github.com/musescore/MuseScore/issues/8164
    case QEvent::MouseButtonDblClick:
        return true;
    case QEvent::MouseButtonPress: {
        QQuickItem* tabBar = tabBarQmlItem();
        if (tabBar) {
            QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
            QPoint localPos = mouseEvent->pos();

            int tabIndex = tabAt(localPos);
            if (tabIndex < 0) {
                return true;
            }

            tabBar->setProperty("currentIndex", tabIndex);
            TabBar::onMousePress(localPos);
        }

        break;
    }
    default:
        break;
    }

    return KDDockWidgets::TabBarQuick::event(event);
}

bool DockTabBar::isPositionDraggable(QPoint localPos) const
{
    if (!m_draggableMouseArea) {
        return false;
    }

    return m_draggableMouseArea->contains(localPos);
}

void DockTabBar::setDraggableMouseArea(QQuickItem* mouseArea)
{
    if (m_draggableMouseArea == mouseArea) {
        return;
    }

    m_draggableMouseArea = mouseArea;
    redirectMouseEvents(mouseArea);
}
