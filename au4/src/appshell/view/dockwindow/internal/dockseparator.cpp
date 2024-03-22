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

#include "dockseparator.h"

#include "log.h"
#include "../docktypes.h"

#include "thirdparty/KDDockWidgets/src/private/multisplitter/Rubberband_quick.h"
#include "thirdparty/KDDockWidgets/src/DockWidgetBase.h"
#include "thirdparty/KDDockWidgets/src/private/DockRegistry_p.h"

#include <QTimer>

using namespace mu::dock;

namespace mu::dock {
static const KDDockWidgets::DockWidgetBase* findNearestDock(const DockSeparator* separator)
{
    const Layouting::ItemBoxContainer* container = separator->parentContainer();
    if (!container) {
        return nullptr;
    }

    int separatorPos = separator->Layouting::Separator::position();
    Qt::Orientation orientation = separator->orientation();
    Layouting::Item::List children = container->visibleChildren();

    const Layouting::Item* nearestItem = nullptr;
    int minPosDiff = std::numeric_limits<int>::max();

    for (const Layouting::Item* child : children) {
        int childPos = child->pos(orientation);
        int diff = std::abs(childPos - separatorPos);

        if (diff < minPosDiff) {
            nearestItem = child;
            minPosDiff = diff;
        }
    }

    if (!nearestItem) {
        return nullptr;
    }

    auto frame = dynamic_cast<KDDockWidgets::Frame*>(nearestItem->guestAsQObject());
    return frame && !frame->isEmpty() ? frame->currentDockWidget() : nullptr;
}
}

DockSeparator::DockSeparator(Layouting::Widget* parent)
    : QQuickItem(qobject_cast<QQuickItem*>(parent->asQObject())),
    Layouting::Separator(parent),
    Layouting::Widget_quick(this), m_isSeparatorVisible(true)
{
    createQQuickItem("qrc:/qml/dockwindow/DockSeparator.qml", this);

    // Only set on Separator::init(), so single-shot
    QTimer::singleShot(0, this, &DockSeparator::isVerticalChanged);
    QTimer::singleShot(0, this, &DockSeparator::showResizeCursorChanged);
    QTimer::singleShot(0, this, [this]() {
        initAvailability();
    });
}

void DockSeparator::initAvailability()
{
    const QObject* dock = findNearestDock(this);
    DockProperties properties = readPropertiesFromObject(dock);

    if (properties.isValid()) {
        m_isSeparatorVisible = properties.separatorsVisible;
        emit isSeparatorVisibleChanged();
        emit showResizeCursorChanged();
    }
}

bool DockSeparator::isVertical() const
{
    return Layouting::Separator::isVertical();
}

bool DockSeparator::isSeparatorVisible() const
{
    return m_isSeparatorVisible;
}

bool DockSeparator::showResizeCursor() const
{
    return parentContainer()
           && (parentContainer()->minPosForSeparator_global(const_cast<DockSeparator*>(this))
               != parentContainer()->maxPosForSeparator_global(const_cast<DockSeparator*>(this)));
}

Layouting::Widget* DockSeparator::createRubberBand(Layouting::Widget* parent)
{
    if (!parent) {
        LOGE() << "Parent is required";
        return nullptr;
    }

    return new Layouting::Widget_quick(new Layouting::RubberBand(parent));
}

Layouting::Widget* DockSeparator::asWidget()
{
    return this;
}

void DockSeparator::onMousePressed()
{
    Layouting::Separator::onMousePress();
}

void DockSeparator::onMouseMoved(QPointF localPos)
{
    const QPointF pos = QQuickItem::mapToItem(parentItem(), localPos);
    Layouting::Separator::onMouseMove(pos.toPoint());
}

void DockSeparator::onMouseReleased()
{
    Layouting::Separator::onMouseReleased();
}

void DockSeparator::onMouseDoubleClicked()
{
    Layouting::Separator::onMouseDoubleClick();
}
