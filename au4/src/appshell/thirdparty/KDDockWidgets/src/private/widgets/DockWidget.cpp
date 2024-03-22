/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DockWidget.h"

#include <QCloseEvent>
#include <QVBoxLayout>

/**
 * @file
 * @brief Represents a dock widget.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

using namespace KDDockWidgets;

class DockWidget::Private
{
public:
    Private(DockWidget *q)
        : layout(new QVBoxLayout(q))
    {
        layout->setSpacing(0);
        layout->setContentsMargins(0, 0, 0, 0);

        // propagate the max-size constraints from the guest widget to the DockWidget
        layout->setSizeConstraint(QLayout::SetMinAndMaxSize);
    }

    QVBoxLayout *const layout;
};

DockWidget::DockWidget(const QString &name, Options options, LayoutSaverOptions layoutSaverOptions)
    : DockWidgetBase(name, options, layoutSaverOptions)
    , d(new Private(this))
{
    connect(this, &DockWidgetBase::widgetChanged, this, [this](QWidget *w) {
        d->layout->addWidget(w);
    });
}

DockWidget::~DockWidget()
{
    delete d;
}

bool DockWidget::event(QEvent *e)
{
    if (e->type() == QEvent::ParentChange) {
        onParentChanged();
    } else if (e->type() == QEvent::Show) {
        onShown(e->spontaneous());
    } else if (e->type() == QEvent::Hide) {
        onHidden(e->spontaneous());
    }

    return QWidget::event(e);
}

void DockWidget::closeEvent(QCloseEvent *e)
{
    onCloseEvent(e);
}
