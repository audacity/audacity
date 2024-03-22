/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_SIDEBARWIDGET_P_H
#define KD_SIDEBARWIDGET_P_H

#include "kddockwidgets/docks_export.h"
#include "../SideBar_p.h"

#include <QToolButton>
#include <QPointer>

QT_BEGIN_NAMESPACE
class QBoxLayout;
class QAbstractButton;
QT_END_NAMESPACE

namespace KDDockWidgets {

class DockWidget;
class Frame;
class SideBarWidget;

class SideBarButton : public QToolButton
{
    Q_OBJECT
public:
    explicit SideBarButton(DockWidgetBase *dw, SideBarWidget *parent);
    bool isVertical() const;
    void paintEvent(QPaintEvent *) override;
    QSize sizeHint() const override;

private:
    SideBarWidget *const m_sideBar;
    const QPointer<DockWidgetBase> m_dockWidget;
};

class DOCKS_EXPORT SideBarWidget : public SideBar
{
    Q_OBJECT
public:
    explicit SideBarWidget(SideBarLocation, KDDockWidgets::MainWindowBase *parent);

protected:
    void addDockWidget_Impl(DockWidgetBase *dock) override;
    void removeDockWidget_Impl(DockWidgetBase *dock) override;

    // virtual so users can provide their own buttons
    virtual SideBarButton *createButton(DockWidgetBase *dw, SideBarWidget *parent) const;

private:
    QBoxLayout *const m_layout;
};

}

#endif
