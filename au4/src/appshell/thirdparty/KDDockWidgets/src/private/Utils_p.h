/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_UTILS_P_H
#define KD_UTILS_P_H

#include "Config.h"
#include "Frame_p.h"
#include "QWidgetAdapter.h"

#include <QScreen>
#include <QWindow>
#include <QMouseEvent>

#ifdef KDDOCKWIDGETS_QTQUICK
#include "private/quick/TitleBarQuick_p.h"

#include <QQuickItem>
#include <QQuickView>
#include <QGuiApplication>
#else
#include <QApplication>
#include <QAbstractButton>
#include <QLineEdit>
#endif

#ifdef QT_X11EXTRAS_LIB
#include <QtX11Extras/QX11Info>
#endif

QT_BEGIN_NAMESPACE
class QWidget;
class QWindow;
QT_END_NAMESPACE

namespace KDDockWidgets {

#ifdef KDDOCKWIDGETS_QTQUICK
inline QQuickItem *mouseAreaForPos(QQuickItem *item, QPointF globalPos);
#endif

inline bool isWayland()
{
    return qApp->platformName() == QLatin1String("wayland");
}

inline bool isOffscreen()
{
    return qApp->platformName() == QLatin1String("offscreen");
}

inline bool kddwUsesQtWidgets()
{
    // Returns whether KDDW is built for QtWidgets or QtQuick
#ifdef KDDOCKWIDGETS_QTWIDGETS
    return true;
#else
    return false;
#endif
}

inline bool isLeftButtonPressed()
{
    return qApp->mouseButtons() & Qt::LeftButton;
}

inline bool usesNativeTitleBar()
{
    return Config::self().flags() & Config::Flag_NativeTitleBar;
}

inline bool usesClientTitleBar()
{
    if (isWayland()) {
        // Wayland has both client and native title bars, due to limitations.
        return true;
    }

    // All other platforms have either the OS native title bar or a Qt title bar (aka client title bar).
    return !usesNativeTitleBar();
}

inline bool usesAeroSnapWithCustomDecos()
{
    //! NOTE: INTERNAL PATCH FOR MU4 ONLY
    //!
    //! The aero snap feature is not needed for panels/toolbars in MU4
    //! but its implementation causes a lot of problem on Windows
    //!
    //! See:
    //! https://github.com/musescore/MuseScore/issues/9821
    //! https://github.com/musescore/MuseScore/issues/9818
    return false;

    //return Config::self().flags() & Config::Flag_AeroSnapWithClientDecos;
}

inline bool usesNativeDraggingAndResizing()
{
    // a native title bar implies native resizing and dragging
    // Windows Aero-Snap also implies native dragging, and implies no native-title bar
    Q_ASSERT(!(usesNativeTitleBar() && usesAeroSnapWithCustomDecos()));
    return usesNativeTitleBar() || usesAeroSnapWithCustomDecos();
}

inline bool usesUtilityWindows()
{
    const auto flags = Config::self().internalFlags();

    const bool dontUse = (flags & Config::InternalFlag_DontUseParentForFloatingWindows) && (flags & Config::InternalFlag_DontUseQtToolWindowsForFloatingWindows);

    return !dontUse;
}

inline bool isNormalWindowState(Qt::WindowStates states)
{
    return !(states & Qt::WindowMaximized) && !(states & Qt::WindowFullScreen);
}

inline bool usesFallbackMouseGrabber()
{
#ifdef KDDOCKWIDGETS_QTWIDGETS
    // Will use QWidget::grabMouse()
    return false;
#else
    // For QtQuick we use the global event filter as mouse delivery is flaky
    // For example, the same QQuickItem that receives the press isn't receiving the mouse moves
    // when the top-level window moves.
    return true;
#endif
}

inline void activateWindow(QWindow *window)
{
    window->requestActivate();
}

inline bool windowManagerHasTranslucency()
{
    if (qEnvironmentVariableIsSet("KDDW_NO_TRANSLUCENCY") || (Config::self().internalFlags() & Config::InternalFlag_DisableTranslucency))
        return false;

#ifdef QT_X11EXTRAS_LIB
    if (qApp->platformName() == QLatin1String("xcb"))
        return QX11Info::isCompositingManagerRunning();
#endif

    // macOS and Windows are fine
    return true;
}

inline QSize screenSizeForWindow(const QWindow *window)
{
    if (window) {
        if (QScreen *screen = window->screen()) {
            return screen->size();
        }
    }

    return {};
}

inline int screenNumberForWindow(const QWindow *window)
{
    if (window) {
        if (QScreen *screen = window->screen()) {
            return qApp->screens().indexOf(screen);
        }
    }

    return -1;
}

inline QMouseEvent *mouseEvent(QEvent *e)
{
    switch (e->type()) {
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonDblClick:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseMove:
    case QEvent::NonClientAreaMouseButtonPress:
    case QEvent::NonClientAreaMouseButtonRelease:
    case QEvent::NonClientAreaMouseMove:
    case QEvent::NonClientAreaMouseButtonDblClick:
        return static_cast<QMouseEvent *>(e);
    default:
        break;
    }

    return nullptr;
}

inline bool isNonClientMouseEvent(const QEvent *e)
{
    switch (e->type()) {
    case QEvent::NonClientAreaMouseButtonPress:
    case QEvent::NonClientAreaMouseButtonRelease:
    case QEvent::NonClientAreaMouseMove:
        return true;
    default:
        break;
    }

    return false;
}

inline bool isWindow(const QWindow *w)
{
    return w != nullptr;
}

inline int startDragDistance()
{
#ifdef KDDOCKWIDGETS_QTWIDGETS
    return QApplication::startDragDistance();
#else
    //! NOTE: INTERNAL PATCH FOR MU4 ONLY
    //!
    //! Resolves the problem of unintended panel undocking
    //!
    //! See:
    //! https://github.com/musescore/MuseScore/pull/16013
    return 40;
#endif
}

/// @brief Returns the QWidget or QtQuickItem at the specified position
/// Basically QApplication::widgetAt() but with support for QtQuick
inline WidgetType *mouseReceiverAt(QPoint globalPos)
{
#ifdef KDDOCKWIDGETS_QTWIDGETS
    return qApp->widgetAt(globalPos);
#else
    auto window = qobject_cast<QQuickWindow *>(qApp->topLevelAt(globalPos));
    if (!window)
        return nullptr;

    return mouseAreaForPos(window->contentItem(), globalPos);
#endif
}

/// Not the entire TitleBar is draggable. For example, the close button won't allow to start a drag from there.
/// Returns true if we're over such controls where we shouldn't drag.
inline bool inDisallowDragWidget(QPoint globalPos)
{
    WidgetType *widget = mouseReceiverAt(globalPos);
    if (!widget)
        return false;

#ifdef KDDOCKWIDGETS_QTWIDGETS
    // User might have a line edit on the toolbar. TODO: Not so elegant fix, we should make the user's tabbar implement some virtual method...
    return qobject_cast<QAbstractButton *>(widget) || qobject_cast<QLineEdit *>(widget);
#else
    return widget->objectName() != QLatin1String("draggableMouseArea");
#endif
}

#ifdef KDDOCKWIDGETS_QTWIDGETS
inline int screenNumberForWidget(const QWidget *widget)
{
    return screenNumberForWindow(widget->window()->windowHandle());
}

inline QSize screenSizeForWidget(const QWidget *widget)
{
    return screenSizeForWindow(widget->window()->windowHandle());
}

inline QPoint mapToGlobal(QWidget *w, QPoint p)
{
    return w->mapToGlobal(p);
}

inline void activateWindow(QWidget *widget)
{
    widget->activateWindow();
}

inline bool isWindow(const QWidget *w)
{
    return w && w->isWindow();
}

#else

inline int screenNumberForWidget(const QQuickItem *w)
{
    return screenNumberForWindow(w->window());
}

inline QSize screenSizeForWidget(const QQuickItem *w)
{
    return screenSizeForWindow(w->window());
}

inline QPoint mapToGlobal(QQuickItem *item, QPoint p)
{
    Q_ASSERT(item);
    return item->mapToGlobal(p).toPoint();
}

inline QQuickItem *mouseAreaForPos(QQuickItem *item, QPointF globalPos)
{
    QRectF rect = item->boundingRect();
    rect.moveTopLeft(item->mapToGlobal(QPointF(0, 0)));

    // Assumes children are inside its parent. That's fine for KDDW's purposes.
    if (!rect.contains(globalPos)) {
        return nullptr;
    }

    const QList<QQuickItem *> children = item->childItems();

    for (auto it = children.rbegin(), end = children.rend(); it != end; ++it) {
        if (QQuickItem *receiver = mouseAreaForPos(*it, globalPos))
            return receiver;
    }

    if (QLatin1String(item->metaObject()->className()) == QLatin1String("QQuickMouseArea"))
        return item;

    return nullptr;
}

inline QRect globalGeometry(QQuickItem *item)
{
    QRect geo(QPoint(0, 0), item->size().toSize());
    geo.moveTopLeft(item->mapToGlobal(QPointF(0, 0)).toPoint());
    return geo;
}

#endif


/// @brief Returns the widget's geometry, but always in global space.
inline QRect globalGeometry(QWidgetOrQuick *w)
{
    QRect geo = w->geometry();
    if (!w->isWindow())
        geo.moveTopLeft(w->mapToGlobal(QPoint(0, 0)));
    return geo;
}

/// @brief Returns whether we support the specified scalling factor
/// This is a workaround against a bug in older Qt (QTBUG-86170).
/// Mostly affects Linux. Unless you're using Qt::HighDpiScaleFactorRoundingPolicy::PassThrough, in which case it will affect other OSes too.
inline bool scalingFactorIsSupported(qreal factor)
{
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 2)
    // We don't support fractional factors in older Qt.
    const bool isInteger = int(factor) == factor;
    return isInteger;
#else
    Q_UNUSED(factor);
    return true;
#endif
}

/// @brief Returns the parent frame which the specified object is in, if any
inline Frame *parentFrame(QObject *from)
{
    auto p = from;
    while (p) {
        if (auto frame = qobject_cast<Frame *>(p))
            return frame;

        p = p->parent();
    }

    return nullptr;
}

};

#endif
