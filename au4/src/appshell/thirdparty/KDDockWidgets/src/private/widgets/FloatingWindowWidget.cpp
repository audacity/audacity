/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "FloatingWindowWidget_p.h"
#include "../DockRegistry_p.h"
#include "../DropArea_p.h"
#include "../Logging_p.h"
#include "../TitleBar_p.h"
#include "../Utils_p.h"

#include <QApplication>
#include <QPainter>
#include <QVBoxLayout>
#include <QWindow>
#include <QWindowStateChangeEvent>

using namespace KDDockWidgets;

FloatingWindowWidget::FloatingWindowWidget(QRect suggestedGeometry, MainWindowBase *parent)
    : FloatingWindow(suggestedGeometry, parent)
    , m_vlayout(new QVBoxLayout(this))
{
    init();
}

FloatingWindowWidget::FloatingWindowWidget(Frame *frame, QRect suggestedGeometry, MainWindowBase *parent)
    : FloatingWindow(frame, suggestedGeometry, parent)
    , m_vlayout(new QVBoxLayout(this))
{
    init();
}

void FloatingWindowWidget::paintEvent(QPaintEvent *ev)
{
    if (Config::self().disabledPaintEvents() & Config::CustomizableWidget_FloatingWindow) {
        QWidget::paintEvent(ev);
        return;
    }

    QPainter p(this);
    QPen pen(0x666666);
    pen.setWidth(1);
    pen.setJoinStyle(Qt::MiterJoin);
    p.setPen(pen);
    const qreal halfPenWidth = p.pen().widthF() / 2;
    const QRectF rectf = rect();
    p.drawRect(rectf.adjusted(halfPenWidth, halfPenWidth, -halfPenWidth, -halfPenWidth));
}

bool FloatingWindowWidget::event(QEvent *ev)
{
    if (ev->type() == QEvent::WindowStateChange) {
        Q_EMIT windowStateChanged(static_cast<QWindowStateChangeEvent *>(ev));
    } else if (ev->type() == QEvent::NonClientAreaMouseButtonDblClick && (Config::self().flags() & Config::Flag_NativeTitleBar)) {
        if ((windowFlags() & Qt::Tool) == Qt::Tool) {
            if (Config::self().flags() & Config::Flag_DoubleClickMaximizes) {
                // Let's refuse to maximize Qt::Tool. It's not natural.
                // Just avoid this combination: Flag_NativeTitleBar + Qt::Tool + Flag_DoubleClickMaximizes
            } else {
                // Double clicking a Qt::Tool title-bar. Triggers a redocking.
                if (m_titleBar->isFloating()) { // redocking nested floating windows aren't supported
                    m_titleBar->onFloatClicked();
                    return true;
                }
            }
        } else {
            // A normal Qt::Window window. The OS handles the double click.
            // In general this will maximize the window, that's the native behaviour.
        }
    } else if (ev->type() == QEvent::Show && !m_screenChangedConnection) {
        // We connect after QEvent::Show, so we have a QWindow. Qt doesn't offer much API to
        // intercept screen events
        m_screenChangedConnection =
            connect(windowHandle(), &QWindow::screenChanged, DockRegistry::self(),
                    [this] { Q_EMIT DockRegistry::self()->windowChangedScreen(windowHandle()); });
    }

    return FloatingWindow::event(ev);
}

void FloatingWindowWidget::init()
{
    m_vlayout->setSpacing(0);
    updateMargins();
    m_vlayout->addWidget(m_titleBar);
    m_vlayout->addWidget(m_dropArea);

    connect(DockRegistry::self(), &DockRegistry::windowChangedScreen, this, [this](QWindow *w) {
        if (w == window()->windowHandle())
            updateMargins();
    });
}

void FloatingWindowWidget::updateMargins()
{
    m_vlayout->setContentsMargins(QMargins(4, 4, 4, 4) * logicalDpiFactor(this));
}
