/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MDILayoutWidget_p.h"
#include "multisplitter/ItemFreeContainer_p.h"
#include "DockWidgetBase_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"

using namespace KDDockWidgets;

MDILayoutWidget::MDILayoutWidget(QWidgetOrQuick *parent)
    : LayoutWidget(parent)
    , m_rootItem(new Layouting::ItemFreeContainer(this))
{
    setRootItem(m_rootItem);
}

MDILayoutWidget::~MDILayoutWidget()
{
}

void MDILayoutWidget::addDockWidget(DockWidgetBase *dw, QPoint localPt, InitialOption addingOption)
{
    if (!dw) {
        qWarning() << Q_FUNC_INFO << "Refusing to add null dock widget";
        return;
    }

    auto frame = qobject_cast<Frame *>(dw->d->frame());
    if (itemForFrame(frame) != nullptr) {
        // Item already exists, remove it. See also comment in MultiSplitter::addWidget().
        frame->QWidgetAdapter::setParent(nullptr);
        frame->setLayoutItem(nullptr);
    }

    Layouting::Item *newItem = new Layouting::Item(this);
    if (frame) {
        newItem->setGuestWidget(frame);
    } else {
        frame = Config::self().frameworkWidgetFactory()->createFrame(nullptr, FrameOption_None);
        frame->addWidget(dw, addingOption);

        newItem->setGuestWidget(frame);
    }

    Q_ASSERT(!newItem->geometry().isEmpty());
    m_rootItem->addDockWidget(newItem, localPt);

    if (addingOption.startsHidden()) {
        delete frame;
    }
}

void MDILayoutWidget::setDockWidgetGeometry(Frame *frame, QRect geometry)
{
    if (!frame)
        return;

    Layouting::Item *item = itemForFrame(frame);
    if (!item) {
        qWarning() << Q_FUNC_INFO << "Frame not found in the layout" << frame;
        return;
    }

    item->setGeometry(geometry);
}

void MDILayoutWidget::moveDockWidget(DockWidgetBase *dw, QPoint pos)
{
    moveDockWidget(dw->d->frame(), pos);
}

void MDILayoutWidget::moveDockWidget(Frame *frame, QPoint pos)
{
    if (!frame)
        return;

    Layouting::Item *item = itemForFrame(frame);
    if (!item) {
        qWarning() << Q_FUNC_INFO << "Frame not found in the layout" << frame;
        return;
    }

    QRect geo = item->geometry();
    geo.moveTopLeft(pos);
    item->setGeometry(geo);
}

void MDILayoutWidget::resizeDockWidget(DockWidgetBase *dw, QSize size)
{
    resizeDockWidget(dw->d->frame(), size);
}

void MDILayoutWidget::resizeDockWidget(Frame *frame, QSize size)
{
    if (!frame)
        return;

    Layouting::Item *item = itemForFrame(frame);
    if (!item) {
        qWarning() << Q_FUNC_INFO << "Frame not found in the layout" << frame;
        return;
    }

    item->setSize(size.expandedTo(frame->minimumSize()));
}
