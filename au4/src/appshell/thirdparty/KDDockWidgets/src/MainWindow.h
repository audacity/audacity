/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief QMainWindow sub-class to enable KDDockWidgets support.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_MAINWINDOW_H
#define KD_MAINWINDOW_H

#include "MainWindowBase.h"

namespace KDDockWidgets {

class SideBar;

/**
 * @brief The QMainwindow sub-class that the application should use to be able
 * to dock KDDockWidget::DockWidget instances.
 */
class DOCKS_EXPORT MainWindow : public MainWindowBase
{
    Q_OBJECT
public:
    typedef QVector<MainWindow *> List;

    ///@brief Constructor. Use it as you would use QMainWindow.
    ///@param uniqueName Mandatory name that should be unique between all MainWindow instances.
    ///       This name won't be user visible and just used internally for the save/restore.
    ///@param options optional MainWindowOptions to use
    ///@param parent QObject *parent to pass to QMainWindow constructor.
    ///@param flags Window flags to  pass to QMainWindow constructor.
    explicit MainWindow(const QString &uniqueName, MainWindowOptions options = MainWindowOption_None,
                        QWidget *parent = nullptr, Qt::WindowFlags flags = Qt::WindowFlags());

    ///@brief Destructor
    ~MainWindow() override;

    ///@brief returns the sidebar for the specified location
    SideBar *sideBar(SideBarLocation) const override;

protected:
    void resizeEvent(QResizeEvent *) override;
    QMargins centerWidgetMargins() const override;
    QRect centralAreaGeometry() const override;

private:
    using QMainWindow::setCentralWidget;
    void setCentralWidget(QWidget *); // overridden just to make it private
    class Private;
    Private *const d;
};

}

#endif
