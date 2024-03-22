/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "Widget_qwidget.h"
#include "Item_p.h"

#include <QDebug>
#include <QWidget>

using namespace Layouting;

Widget_qwidget::Widget_qwidget(QWidget *thisWidget)
    : Widget(thisWidget)
    , m_thisWidget(thisWidget)
{
}

Widget_qwidget::~Widget_qwidget()
{
}

QSize Widget_qwidget::sizeHint() const
{
    return m_thisWidget->sizeHint();
}

QSize Widget_qwidget::minSize() const
{
    return widgetMinSize(m_thisWidget);
}

QSize Widget_qwidget::maxSizeHint() const
{
    return widgetMaxSize(m_thisWidget);
}

QRect Widget_qwidget::geometry() const
{
    return m_thisWidget->geometry();
}

void Widget_qwidget::setGeometry(QRect rect)
{
    m_thisWidget->setGeometry(rect);
}

void Widget_qwidget::setParent(Widget *parent)
{
    if (!parent) {
        m_thisWidget->setParent(nullptr);
        return;
    }

    if (auto qwidget = qobject_cast<QWidget *>(parent->asQObject())) {
        m_thisWidget->setParent(qwidget);
    } else {
        qWarning() << Q_FUNC_INFO << "parent is not a widget, you have a bug" << parent->asQObject();
        Q_ASSERT(false);
    }
}

QDebug &Widget_qwidget::dumpDebug(QDebug &d) const
{
    d << " Dump Start: Host=" << m_thisWidget << rect()
      << "; dpr=" << m_thisWidget->devicePixelRatio() << ")";

    return d;
}

bool Widget_qwidget::isVisible() const
{
    return m_thisWidget->isVisible();
}

void Widget_qwidget::setVisible(bool is) const
{
    m_thisWidget->setVisible(is);
}

std::unique_ptr<Widget> Widget_qwidget::parentWidget() const
{
    if (auto pw = m_thisWidget->parentWidget()) {
        return std::unique_ptr<Widget>(new Widget_qwidget(pw));
    }

    return {};
}

void Widget_qwidget::show()
{
    m_thisWidget->show();
}

void Widget_qwidget::hide()
{
    m_thisWidget->hide();
}

void Widget_qwidget::move(int x, int y)
{
    m_thisWidget->move(x, y);
}

void Widget_qwidget::setSize(int width, int height)
{
    m_thisWidget->resize(QSize(width, height));
}

void Widget_qwidget::setWidth(int width)
{
    setSize(width, m_thisWidget->height());
}

void Widget_qwidget::setHeight(int height)
{
    setSize(m_thisWidget->width(), height);
}

void Widget_qwidget::update()
{
    m_thisWidget->update();
}
