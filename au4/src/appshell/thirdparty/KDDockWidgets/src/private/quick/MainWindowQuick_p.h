/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_MAIN_WINDOW_QUICK_P_H
#define KD_MAIN_WINDOW_QUICK_P_H

#include "kddockwidgets/MainWindowBase.h"

namespace KDDockWidgets {

///@brief The MainWindow counterpart for QtQuick
///Provides the ability of acepting drops of dock widgets.
///It's not a real QWindow and not a main window in the sense of QMainWindow. Would be overkill
///to have tool bars, menu bar and footer in the QtQuick implementation. That's left for the user to do.
class DOCKS_EXPORT MainWindowQuick : public MainWindowBase
{
    Q_OBJECT
public:
    explicit MainWindowQuick(const QString &uniqueName,
                             MainWindowOptions options = MainWindowOption_HasCentralFrame,
                             QQuickItem *parent = nullptr, Qt::WindowFlags flags = {});

    ~MainWindowQuick() override;

    /// @reimp
    QSize minimumSize() const override;

    /// @reimp
    QSize maximumSize() const override;

protected:
    SideBar *sideBar(SideBarLocation) const override;
    QMargins centerWidgetMargins() const override;

private:
    void onMultiSplitterGeometryUpdated();
};

}

#endif
