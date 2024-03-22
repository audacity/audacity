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

#ifndef KDDOCKWIDGETS_QWIDGETADAPTERQUICK_P_H
#define KDDOCKWIDGETS_QWIDGETADAPTERQUICK_P_H

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/Qt5Qt6Compat_p.h"

#include <QQuickItem>
#include <QObject>
#include <QCloseEvent>
#include <QQuickWindow>
#include <QScreen>

QT_BEGIN_NAMESPACE
class QWindow;
class QQmlEngine;
class QQuickView;
QT_END_NAMESPACE

namespace KDDockWidgets {

class MouseEventRedirector;

namespace Private {

DOCKS_EXPORT QQuickItem *widgetForWindow(QWindow *window);

/// @brief Helper since QQuickItem::parentItem() has a different name than QWidget::parentWidget()
inline QQuickItem *parentWidget(QQuickItem *item)
{
    return item ? item->parentItem() : nullptr;
}

inline bool isMinimized(const QQuickItem *item)
{
    QWindow *window = item ? item->window() : nullptr;
    return KDDockWidgets::Private::isMinimized(window);
}

inline QRect geometry(const QQuickItem *item)
{
    QRect r(QPoint(0, 0), item->size().toSize());
    r.moveTopLeft(QPointF(item->x(), item->y()).toPoint());
    return r;
}

inline QRect parentGeometry(const QQuickItem *item)
{
    if (!item || !item->parentItem())
        return QRect();


    return geometry(item->parentItem());
}

inline QWindow *windowForWidget(const QQuickItem *item)
{
    return item ? item->window() : nullptr;
}

/// @brief sets the geometry on the QWindow containing the specified item
inline void setTopLevelGeometry(QRect geometry, const QQuickItem *item)
{
    if (!item)
        return;

    if (QWindow *window = item->window())
        window->setGeometry(geometry);
}

} // namespace Private

class FloatingWindow;
class DOCKS_EXPORT QWidgetAdapter : public QQuickItem
{
    Q_OBJECT
public:
    explicit QWidgetAdapter(QQuickItem *parent = nullptr, Qt::WindowFlags f = {});
    ~QWidgetAdapter() override;

    ///@brief returns the FloatingWindow this widget is in, otherwise nullptr
    FloatingWindow *floatingWindow() const;

    void setFlag(Qt::WindowType, bool on = true);

    int x() const
    {
        return int(QQuickItem::x());
    }
    int y() const
    {
        return int(QQuickItem::y());
    }
    int width() const
    {
        return int(QQuickItem::width());
    }
    int height() const
    {
        return int(QQuickItem::height());
    }

    virtual void setGeometry(QRect);
    QRect frameGeometry() const;
    QRect geometry() const;
    QRect normalGeometry() const;
    void setNormalGeometry(QRect);
    QRect rect() const;
    QPoint pos() const;
    void show();
    void setFixedHeight(int);
    void setFixedWidth(int);
    void raise();
    void update()
    {
    }

    QSize size() const
    {
        return QQuickItem::size().toSize();
    }
    virtual QSize minimumSizeHint() const
    {
        return minimumSize();
    }
    virtual QSize minimumSize() const;
    virtual QSize maximumSize() const;
    int minimumHeight() const
    {
        return minimumSize().height();
    }
    int minimumWidth() const
    {
        return minimumSize().width();
    }
    bool hasFixedWidth() const
    {
        return false;
    }
    bool hasFixedHeight() const
    {
        return false;
    }
    int maximumWidth() const
    {
        return maximumSize().width();
    }
    int maximumHeight() const
    {
        return maximumSize().height();
    }
    WId winId() const;

    void grabMouse();
    void releaseMouse();
    void releaseKeyboard();
    void setMinimumSize(QSize);
    void setMinimumSize(int w, int h);
    void setMaximumSize(QSize sz);
    void setMaximumSize(int w, int h);
    void updateGeometry();
    void resize(QSize);
    void resize(int w, int h);
    bool isWindow() const;
    bool isTopLevel() const
    {
        return isWindow();
    }
    bool isMaximized() const;
    bool isMinimized() const;
    bool isActiveWindow() const;
    Q_INVOKABLE void showMaximized();
    Q_INVOKABLE void showMinimized();
    Q_INVOKABLE void showNormal();
    Q_INVOKABLE void redirectMouseEvents(QObject *from);

    QScreen *screen() const;
    QQuickView *quickView() const;
    QWindow *windowHandle() const;
    QWidgetAdapter *window() const;
    QWidgetAdapter *parentWidget(bool includeTransient = true) const;
    QPoint mapToGlobal(QPoint pt) const;
    QPoint mapFromGlobal(QPoint) const;
    QPoint mapTo(const QQuickItem *parent, QPoint pos) const;
    bool testAttribute(Qt::WidgetAttribute) const;
    void setAttribute(Qt::WidgetAttribute, bool enabled = true);

    void setWindowTitle(const QString &);
    void setWindowIcon(const QIcon &);
    Q_INVOKABLE bool close();
    QQuickItem *childAt(QPoint) const;
    void move(int x, int y);
    void move(QPoint);
    void setSize(QSize);

    void setParent(QQuickItem *);
    void activateWindow();
    void setSizePolicy(QSizePolicy);
    QSizePolicy sizePolicy() const;
    Qt::FocusPolicy focusPolicy() const;
    void setFocusPolicy(Qt::FocusPolicy);
    void setFocus(Qt::FocusReason);
    virtual QSize sizeHint() const;
    bool hasFocus() const;

    void setWindowFlag(int flag, bool enable = true);
    Qt::WindowFlags windowFlags() const;
    void setWindowOpacity(qreal);

    void render(QPainter *);
    void setMouseTracking(bool);
    void setWindowIsBeingDestroyed(bool);
    void setIsWrapper();
    bool isWrapper() const;

    static QQuickItem *createItem(QQmlEngine *, const QString &filename);
    static void makeItemFillParent(QQuickItem *item);
Q_SIGNALS:
    void geometryUpdated(); // similar to QLayout stuff, when size constraints change
    void itemGeometryChanged(); // emitted when the geometry changes. QQuickItem::geometryChanged()
        // isn't a signal, so prefixed item

protected:
    void create();
    bool event(QEvent *) override;
    bool eventFilter(QObject *, QEvent *) override;
    void QQUICKITEMgeometryChanged(const QRectF &newGeometry, const QRectF &oldGeometry) override;
    void raiseAndActivate();
    virtual bool onResize(QSize newSize);
    virtual void onLayoutRequest();
    virtual void onMousePress();
    virtual void onMouseMove(QPoint globalPos);
    virtual void onMouseRelease();
    virtual void onCloseEvent(QCloseEvent *);
    virtual void onResizeEvent(QResizeEvent *);
    virtual void onMoveEvent(QMoveEvent *);
    void itemChange(QQuickItem::ItemChange, const QQuickItem::ItemChangeData &) override;

private:
    void updateNormalGeometry();

    QSize m_sizeHint;
    QSizePolicy m_sizePolicy = QSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
    ;
    Qt::WindowFlags m_windowFlags;
    int m_widgetAttributes = 0; // Qt::WidgetAttribute
    Qt::FocusPolicy m_focusPolicy = Qt::NoFocus;
    bool m_windowIsBeingDestroyed = false;
    bool m_mouseTrackingEnabled = false;
    bool m_isWrapper = false;
    bool m_inSetParent = false;
    MouseEventRedirector *m_mouseEventRedirector = nullptr;
    QRect m_normalGeometry;
};

inline qreal logicalDpiFactor(const QQuickItem *item)
{
#ifndef Q_OS_MACOS
    if (QQuickWindow *window = item->window()) {
        if (QScreen *s = window->screen()) {
            return s->logicalDotsPerInch() / 96.0;
        }
    }
#endif

    // It's always 72 on mac
    Q_UNUSED(item);
    return 1;
}

}

#endif
