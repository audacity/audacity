/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "ItemFreeContainer_p.h"

using namespace Layouting;

ItemFreeContainer::ItemFreeContainer(Widget *hostWidget, ItemContainer *parent)
    : ItemContainer(hostWidget, parent)
{
}

ItemFreeContainer::ItemFreeContainer(Widget *hostWidget)
    : ItemContainer(hostWidget)
{
}

ItemFreeContainer::~ItemFreeContainer()
{
}

void ItemFreeContainer::addDockWidget(Item *item, QPoint localPt)
{
    Q_ASSERT(item != this);
    if (contains(item)) {
        qWarning() << Q_FUNC_INFO << "Item already exists";
        return;
    }
    item->setIsVisible(true); // TODO: Use OptionStartHidden here too

    m_children.append(item);
    item->setParentContainer(this);
    item->setPos(localPt);

    Q_EMIT itemsChanged();

    if (item->isVisible())
        Q_EMIT numVisibleItemsChanged(numVisibleChildren());

    Q_EMIT numItemsChanged();
}

void ItemFreeContainer::clear()
{
    qDeleteAll(m_children);
    m_children.clear();
}

void ItemFreeContainer::removeItem(Item *item, bool hardRemove)
{
    const bool wasVisible = item->isVisible();

    if (hardRemove) {
        m_children.removeOne(item);
        delete item;
    } else {
        item->setIsVisible(false);
        item->setGuestWidget(nullptr);
    }

    if (wasVisible)
        Q_EMIT numVisibleItemsChanged(numVisibleChildren());

    Q_EMIT itemsChanged();
}

void ItemFreeContainer::restore(Item *child)
{
    child->setIsVisible(true);
}

void ItemFreeContainer::onChildMinSizeChanged(Item *)
{
    // Nothing here either, although we could update the size constraints
}

void ItemFreeContainer::onChildVisibleChanged(Item *, bool)
{
    // Nothing needed to do in this layout type
}
