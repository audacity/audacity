/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief A class that is QWidget when building for QtWidgets, and QObject when building for QtQuick.
 *
 * Allows to have the same code base supporting both stacks.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KDDOCKWIDGETS_QWIDGETADAPTERWIDGETS_P_H
#define KDDOCKWIDGETS_QWIDGETADAPTERWIDGETS_P_H

#include "kddockwidgets/docks_export.h"

#include <QWindow>
#include <QWidget>

namespace KDDockWidgets {
namespace Private {

/// @brief Helper since QQuickItem::parentItem() has a different name than QWidget::parentWidget()
inline QWidget *parentWidget(QWidget *widget)
{
    return widget ? widget->parentWidget() : nullptr;
}

bool isMinimized(QWindow *widget);

inline bool isMinimized(const QWidget *widget)
{
    QWindow *window = widget ? widget->window()->windowHandle() : nullptr;
    return KDDockWidgets::Private::isMinimized(window);
}

inline QRect geometry(const QWidget *w)
{
    return w ? w->geometry() : QRect();
}

inline QRect parentGeometry(const QWidget *w)
{
    if (!w || !w->parentWidget())
        return QRect();

    return w->parentWidget()->geometry();
}

inline QWindow *windowForWidget(const QWidget *w)
{
    return w ? w->window()->windowHandle() : nullptr;
}

DOCKS_EXPORT QWidget *widgetForWindow(QWindow *window);

/// @brief sets the geometry on the QWindow containing the specified item
inline void setTopLevelGeometry(QRect geometry, const QWidget *widget)
{
    if (!widget)
        return;

    if (QWidget *topLevel = widget->window())
        topLevel->setGeometry(geometry);
}

}

class FloatingWindow;

class DOCKS_EXPORT QWidgetAdapter : public QWidget
{
    Q_OBJECT
public:
    explicit QWidgetAdapter(QWidget *parent = nullptr, Qt::WindowFlags f = {});
    ~QWidgetAdapter() override;

    ///@brief returns the FloatingWindow this widget is in, otherwise nullptr
    FloatingWindow *floatingWindow() const;

    void setFlag(Qt::WindowType, bool on = true);
    void setSize(QSize);

    bool isTopLevel() const
    {
        return isWindow();
    }

protected:
    void raiseAndActivate();
    bool event(QEvent *e) override;
    void resizeEvent(QResizeEvent *) override;
    void mousePressEvent(QMouseEvent *) override;
    void mouseMoveEvent(QMouseEvent *) override;
    void mouseReleaseEvent(QMouseEvent *) override;
    void closeEvent(QCloseEvent *) override;
    virtual void setNormalGeometry(QRect);

    virtual bool onResize(QSize newSize);
    virtual void onLayoutRequest();
    virtual void onMousePress();
    virtual void onMouseMove(QPoint globalPos);
    virtual void onMouseRelease();
    virtual void onCloseEvent(QCloseEvent *);
};

inline qreal logicalDpiFactor(const QWidget *w)
{
#ifdef Q_OS_MACOS
    // It's always 72 on mac
    Q_UNUSED(w);
    return 1;
#else
    return w->logicalDpiX() / 96.0;
#endif
}
}

#endif
