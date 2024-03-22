/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#pragma once

#include <kddockwidgets/DockWidget.h>
#include <kddockwidgets/MainWindow.h>

class MyMainWindow : public KDDockWidgets::MainWindow
{
    Q_OBJECT
public:
    explicit MyMainWindow(const QString &uniqueName, KDDockWidgets::MainWindowOptions options,
                          bool dockWidget0IsNonClosable, bool nonDockableDockWidget9, bool restoreIsRelative,
                          bool maxSizeForDockWidget8, bool dockwidget5DoesntCloseBeforeRestore,
                          const QString &affinityName = {}, // Usually not needed. Just here to show the feature.
                          QWidget *parent = nullptr);
    ~MyMainWindow() override;

private:
    void createDockWidgets();
    KDDockWidgets::DockWidgetBase* newDockWidget();
    QMenu *m_toggleMenu = nullptr;
    const bool m_dockWidget0IsNonClosable;
    const bool m_dockWidget9IsNonDockable;
    const bool m_restoreIsRelative;
    const bool m_maxSizeForDockWidget8;
    const bool m_dockwidget5DoesntCloseBeforeRestore;
    KDDockWidgets::DockWidget::List m_dockwidgets;
};
