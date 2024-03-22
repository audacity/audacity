/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Window to show debug information. Used for debugging only, for apps that don't support GammaRay.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */
#ifndef DEBUGWINDOW_H
#define DEBUGWINDOW_H

#include "ObjectViewer_p.h"
#include <QWidget>

QT_BEGIN_NAMESPACE
class QEventLoop;
QT_END_NAMESPACE

namespace KDDockWidgets {
namespace Debug {

class DebugWindow : public QWidget
{
    Q_OBJECT
public:
    explicit DebugWindow(QWidget *parent = nullptr);

private:
#ifdef Q_OS_WIN
    void dumpWindow(QWidget *);
    void dumpWindows();
#endif

    void repaintWidgetRecursive(QWidget *);

    void dumpDockWidgetInfo();
    ObjectViewer m_objectViewer;
    QEventLoop *m_isPickingWidget = nullptr;

protected:
    void mousePressEvent(QMouseEvent *event) override;
};
}
}

#endif
