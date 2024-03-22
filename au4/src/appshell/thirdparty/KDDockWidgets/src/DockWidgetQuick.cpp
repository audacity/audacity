/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DockWidgetQuick.h"
#include "FrameworkWidgetFactory.h"

#include "private/TitleBar_p.h"
#include "private/DockWidgetBase_p.h"
#include "private/quick/FrameQuick_p.h"

#include <Config.h>
#include <QQuickItem>
#include <QCloseEvent>

/**
 * @file
 * @brief Represents a dock widget.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

using namespace KDDockWidgets;

class DockWidgetQuick::Private
{
public:
    Private(DockWidgetQuick *dw, QQmlEngine *qmlengine)
        : q(dw)
        , m_visualItem(q->createItem(qmlengine,
                                     Config::self().frameworkWidgetFactory()->dockwidgetFilename().toString()))
        , m_qmlEngine(qmlengine)
    {
        Q_ASSERT(m_visualItem);
        m_visualItem->setParent(q);
        m_visualItem->setParentItem(q);
    }

    DockWidgetBase *const q;
    QQuickItem *const m_visualItem;
    QQmlEngine *const m_qmlEngine;
};

DockWidgetQuick::DockWidgetQuick(const QString &name, Options options,
                                 LayoutSaverOptions layoutSaverOptions, QQmlEngine *engine)
    : DockWidgetBase(name, options, layoutSaverOptions)
    , d(new Private(this, engine ? engine : Config::self().qmlEngine()))
{
    // To mimic what QtWidgets does when creating a new QWidget.
    setVisible(false);
}

DockWidgetQuick::~DockWidgetQuick()
{
    delete d;
}

void DockWidgetQuick::setWidget(const QString &qmlFilename)
{
    QQuickItem *guest = createItem(d->m_qmlEngine, qmlFilename);
    if (!guest)
        return;

    setWidget(guest);
}

void DockWidgetQuick::setWidget(QWidgetAdapter *widget)
{
    widget->QWidgetAdapter::setParent(this);
    QWidgetAdapter::makeItemFillParent(widget);
    DockWidgetBase::setWidget(widget);
}

void DockWidgetQuick::setWidget(QQuickItem *guest)
{
    auto adapter = new QWidgetAdapter(this);
    adapter->setIsWrapper();

    // In case the user app needs to use them:
    adapter->setProperty("originalParent", QVariant::fromValue(guest->parent()));
    adapter->setProperty("originalParentItem", QVariant::fromValue(guest->parentItem()));

    guest->setParentItem(adapter);
    guest->setParent(adapter);
    QWidgetAdapter::makeItemFillParent(guest);

    setWidget(adapter);
}

bool DockWidgetQuick::event(QEvent *e)
{
    if (e->type() == QEvent::ParentChange) {
        onParentChanged();
        Q_EMIT actualTitleBarChanged();
    } else if (e->type() == QEvent::Show) {
        onShown(e->spontaneous());
    } else if (e->type() == QEvent::Hide) {
        onHidden(e->spontaneous());
    } else if (e->type() == QEvent::Close) {
        onCloseEvent(static_cast<QCloseEvent *>(e));
    }

    return DockWidgetBase::event(e);
}

QSize DockWidgetQuick::minimumSize() const
{
    if (QWidgetAdapter *guestWidget = widget()) {
        // The guests min-size is the same as the widget's, there's no spacing or margins.
        return guestWidget->minimumSize();
    }

    return DockWidgetBase::minimumSize();
}

QSize DockWidgetQuick::maximumSize() const
{
    if (QWidgetAdapter *guestWidget = widget()) {
        // The guests max-size is the same as the widget's, there's no spacing or margins.
        return guestWidget->maximumSize();
    }

    return DockWidgetBase::maximumSize();
}

TitleBar *DockWidgetQuick::actualTitleBar() const
{
    if (Frame *frame = DockWidgetBase::d->frame())
        return frame->actualTitleBar();
    return nullptr;
}

QObject *DockWidgetQuick::actualTitleBarObj() const
{
    return actualTitleBar();
}

QQuickItem *DockWidgetQuick::frameVisualItem() const
{
    if (auto frame = qobject_cast<FrameQuick *>(DockWidgetBase::d->frame()))
        return frame->visualItem();

    return nullptr;
}

void DockWidgetQuick::onGeometryUpdated()
{
    if (auto frame = qobject_cast<FrameQuick *>(DockWidgetBase::d->frame())) {
        frame->updateConstriants();
        frame->updateGeometry();
    }
}

Frame *DockWidgetQuick::frame() const
{
    return qobject_cast<FrameQuick *>(DockWidgetBase::d->frame());
}
