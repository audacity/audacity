/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "TitleBarQuick_p.h"

#include "../DragController_p.h"
#include "../Frame_p.h"
#include "../FloatingWindow_p.h"
#include "../Logging_p.h"
#include "../WindowBeingDragged_p.h"
#include "../Utils_p.h"


using namespace KDDockWidgets;


TitleBarQuick::TitleBarQuick(Frame *parent)
    : TitleBar(parent)
{
    setFixedHeight(30);
}

TitleBarQuick::TitleBarQuick(FloatingWindow *parent)
    : TitleBar(parent)
{
    setFixedHeight(30);
}

TitleBarQuick::~TitleBarQuick()
{
}

#ifdef DOCKS_DEVELOPER_MODE
bool TitleBarQuick::isCloseButtonEnabled() const
{
    if (QQuickItem *button = closeButton())
        return button->isEnabled();
    return false;
}

bool TitleBarQuick::isCloseButtonVisible() const
{
    if (QQuickItem *button = closeButton())
        return button->isVisible();

    return true;
}

bool TitleBarQuick::isFloatButtonEnabled() const
{
    if (QQuickItem *button = floatButton())
        return button->isEnabled();

    return true;
}

bool TitleBarQuick::isFloatButtonVisible() const
{
    if (QQuickItem *button = floatButton())
        return button->isVisible();

    return true;
}
#endif

QQuickItem *TitleBarQuick::titleBarQmlItem() const
{
    return m_titleBarQmlItem;
}

QQuickItem *TitleBarQuick::titleBarMouseArea() const
{
    if (m_titleBarQmlItem)
        return m_titleBarQmlItem->property("mouseAreaForTests").value<QQuickItem *>();

    return nullptr;
}

void TitleBarQuick::setTitleBarQmlItem(QQuickItem *item)
{
    if (item != m_titleBarQmlItem) {
        m_titleBarQmlItem = item;
        Q_EMIT titleBarQmlItemChanged();
    }
}

QQuickItem *TitleBarQuick::floatButton() const
{
    return m_titleBarQmlItem ? m_titleBarQmlItem->property("floatButton").value<QQuickItem *>()
                             : nullptr;
}

QQuickItem *TitleBarQuick::closeButton() const
{
    return m_titleBarQmlItem ? m_titleBarQmlItem->property("closeButton").value<QQuickItem *>()
                             : nullptr;
}
