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

#include "docktoolbarview.h"

#include "thirdparty/KDDockWidgets/src/DockWidgetQuick.h"
#include "thirdparty/KDDockWidgets/src/private/TitleBar_p.h"
#include "thirdparty/KDDockWidgets/src/private/DragController_p.h"

#include "log.h"
#include "docktypes.h"

using namespace mu::dock;

class DockToolBarView::DraggableArea : public KDDockWidgets::QWidgetAdapter, public KDDockWidgets::Draggable
{
public:
    DraggableArea()
        : KDDockWidgets::QWidgetAdapter(),
        KDDockWidgets::Draggable(this)
    {
    }

    std::unique_ptr<KDDockWidgets::WindowBeingDragged> makeWindow() override
    {
        if (!m_dockWidget) {
            return {};
        }

        KDDockWidgets::FloatingWindow* floatingWindow = m_dockWidget->floatingWindow();
        if (floatingWindow) {
            return std::unique_ptr<KDDockWidgets::WindowBeingDragged>(new KDDockWidgets::WindowBeingDragged(floatingWindow, this));
        }

        m_dockWidget->setFloating(true);
        floatingWindow = m_dockWidget->floatingWindow();

        auto draggable = static_cast<KDDockWidgets::Draggable*>(this);
        return std::unique_ptr<KDDockWidgets::WindowBeingDragged>(new KDDockWidgets::WindowBeingDragged(floatingWindow, draggable));
    }

    KDDockWidgets::DockWidgetBase* singleDockWidget() const override
    {
        return m_dockWidget;
    }

    bool isMDI() const override
    {
        return false;
    }

    bool isWindow() const override
    {
        return false;
    }

    QPoint mapToWindow(QPoint pos) const override
    {
        if (!m_mouseArea) {
            return pos;
        }

        QPointF result = m_mouseArea->mapToItem(m_dockWidget, QPointF(pos));

        result.setX(result.x() + DOCK_WINDOW_SHADOW);
        result.setY(result.y() + DOCK_WINDOW_SHADOW);

        return QPoint(result.x(), result.y());
    }

    void setDockWidget(KDDockWidgets::DockWidgetBase* dockWidget)
    {
        IF_ASSERT_FAILED(dockWidget) {
            return;
        }

        m_dockWidget = dockWidget;
        setObjectName(dockWidget->objectName() + "_draggableArea");
    }

    void setMouseArea(QQuickItem* mouseArea)
    {
        m_mouseArea = mouseArea;
        redirectMouseEvents(mouseArea);
    }

private:
    KDDockWidgets::DockWidgetBase* m_dockWidget = nullptr;
    QQuickItem* m_mouseArea = nullptr;
};

DockToolBarView::DockToolBarView(QQuickItem* parent)
    : DockBase(DockType::ToolBar, parent),
    //! NOTE: parent (MouseArea) will be set later
    m_draggableArea(new DraggableArea())
{
    setLocation(Location::Top);
}

Qt::Orientation DockToolBarView::orientation() const
{
    return m_orientation;
}

int DockToolBarView::alignment() const
{
    return m_alignment;
}

void DockToolBarView::setOrientation(Qt::Orientation orientation)
{
    if (orientation == m_orientation) {
        return;
    }

    m_orientation = orientation;
    emit orientationChanged(orientation);
}

void DockToolBarView::setAlignment(int alignment)
{
    if (alignment == m_alignment) {
        return;
    }

    m_alignment = alignment;
    emit alignmentChanged(alignment);
}

void DockToolBarView::setDraggableMouseArea(QQuickItem* mouseArea)
{
    IF_ASSERT_FAILED(m_draggableArea) {
        return;
    }

    m_draggableArea->setParent(mouseArea);
    m_draggableArea->setMouseArea(mouseArea);
}

void DockToolBarView::componentComplete()
{
    DockBase::componentComplete();

    m_draggableArea->setDockWidget(dockWidget());

    connect(this, &DockBase::floatingChanged, this, [this]() {
        setResizable(!floating());
    });
}

void DockToolBarView::init()
{
    if (canChangeOrientation()) {
        if (height() > width()) {
            setOrientation(Qt::Vertical);
        } else {
            setOrientation(Qt::Horizontal);
        }
    }

    DockBase::init();
}

bool DockToolBarView::canChangeOrientation() const
{
    if (!floatable()) {
        return false;
    }

    for (const DropDestination& dest : dropDestinations()) {
        if (dest.dropLocation == Location::Left || dest.dropLocation == Location::Right) {
            return true;
        }
    }

    return false;
}

void DockToolBarView::resetToDefault()
{
    DockBase::resetToDefault();

    setOrientation(Qt::Horizontal);
}
