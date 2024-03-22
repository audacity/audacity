/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Implements a QTabWidget derived class with support for docking and undocking
 * KDockWidget::DockWidget as tabs .
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_TABBAR_WIDGET_P_H
#define KD_TABBAR_WIDGET_P_H

#include "../TabWidget_p.h"

#include <QTabBar>

QT_BEGIN_NAMESPACE
class QMouseEvent;
QT_END_NAMESPACE

// clazy:excludeall=ctor-missing-parent-argument

namespace KDDockWidgets {

class DockWidget;
class TabWidget;

class DOCKS_EXPORT TabBarWidget
    : public QTabBar,
      public TabBar
{
    Q_OBJECT
public:
    explicit TabBarWidget(TabWidget *parent = nullptr);
    int tabAt(QPoint localPos) const override;

    DockWidgetBase *currentDockWidget() const;

    QString text(int index) const override;
    QRect rectForTab(int index) const override;
    void moveTabTo(int from, int to) override;

protected:
    bool dragCanStart(QPoint pressPos, QPoint pos) const override;
    void mousePressEvent(QMouseEvent *) override;
    void mouseMoveEvent(QMouseEvent *e) override;
    void mouseDoubleClickEvent(QMouseEvent *e) override;
    bool event(QEvent *) override;

private:
    TabWidget *const m_tabWidget;
};
}

#endif
