/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

///@file
///@brief An abstraction/wrapper around QWidget, QtQuickItem or anything else

#pragma once

#include "kddockwidgets/docks_export.h"
#include "Item_p.h"

#include <QRect>
#include <QSize>
#include <QDebug>
#include <QObject>
#include <qglobal.h>

#include <memory>

QT_BEGIN_NAMESPACE
class QWidget;
QT_END_NAMESPACE

#ifdef QT_WIDGETS_LIB
#include <QSizePolicy>
#else
class QSizePolicy
{
public:
    enum Policy
    {
        Fixed,
        Preferred,
        Maximum
    };

    explicit QSizePolicy(Policy h, Policy v)
        : horizontal(h)
        , vertical(v)
    {
    }

    Policy verticalPolicy() const
    {
        return vertical;
    }

    Policy horizontalPolicy() const
    {
        return horizontal;
    }

    Policy horizontal = Fixed;
    Policy vertical = Fixed;
};
#endif

namespace Layouting {

class Item;

/**
 * @brief An abstraction/wrapper around QWidget, QtQuickItem or anything else
 *
 * So the layout can host QWidget, etc without depending on it.
 *
 * Inherit from it via multi-inheritance so this wrapper is deleted when the actual QWidget/QQuickItem
 * is deleted.
 */
class DOCKS_EXPORT Widget
{
public:
    explicit Widget(QObject *thisObj);
    virtual ~Widget();

    virtual void setLayoutItem(Item *) = 0;

    // Not strickly necessary, but it's nice convenience for kddw which is widget based.
    virtual QWidget *asQWidget() const
    {
        Q_ASSERT(false); // Only wanted for QtWidgets. All other should not call this.
        return nullptr;
    }

    virtual QSize sizeHint() const
    {
        return {};
    }
    virtual QSize minSize() const = 0;
    virtual QSize maxSizeHint() const = 0;
    virtual QRect geometry() const = 0;
    virtual void setGeometry(QRect) = 0;
    virtual void setParent(Widget *) = 0;
    virtual QDebug &dumpDebug(QDebug &) const = 0;
    virtual bool isVisible() const = 0;
    virtual void setVisible(bool) const = 0;
    virtual void move(int x, int y) = 0;
    virtual void setSize(int width, int height) = 0;
    virtual void setWidth(int width) = 0;
    virtual void setHeight(int height) = 0;
    virtual std::unique_ptr<Widget> parentWidget() const = 0;
    virtual void show() = 0;
    virtual void hide() = 0;
    virtual void update() = 0;

    QSize size() const
    {
        return geometry().size();
    }

    QRect rect() const
    {
        return QRect(QPoint(0, 0), size());
    }

    QObject *asQObject() const
    {
        return m_thisObj;
    }
    QObject *parent() const
    {
        return m_thisObj->parent();
    }

    int x() const
    {
        return geometry().x();
    }

    int y() const
    {
        return geometry().y();
    }

    bool operator==(const QObject *obj) const
    {
        return obj == m_thisObj;
    }

    ///@brief returns an id for corelation purposes for saving layouts
    QString id() const;

    template<typename T>
    static QSize widgetMinSize(const T *w)
    {
        const int minW = w->minimumWidth() > 0 ? w->minimumWidth()
                                               : w->minimumSizeHint().width();

        const int minH = w->minimumHeight() > 0 ? w->minimumHeight()
                                                : w->minimumSizeHint().height();

        return QSize(minW, minH).expandedTo(Item::hardcodedMinimumSize);
    }

    template<typename T>
    static QSize widgetMaxSize(const T *w)
    {
        // The max size is usually QWidget::maximumSize(), but we also honour the QSizePolicy::Fixed+sizeHint() case
        // as widgets don't need to have QWidget::maximumSize() to have a max size honoured

        const QSize min = widgetMinSize(w);
        QSize max = w->maximumSize();
        max = boundedMaxSize(min, max); // for safety against weird values

        const QSizePolicy policy = w->sizePolicy();

        if (policy.verticalPolicy() == QSizePolicy::Fixed || policy.verticalPolicy() == QSizePolicy::Maximum)
            max.setHeight(qMin(max.height(), w->sizeHint().height()));
        if (policy.horizontalPolicy() == QSizePolicy::Fixed || policy.horizontalPolicy() == QSizePolicy::Maximum)
            max.setWidth(qMin(max.width(), w->sizeHint().width()));

        max = boundedMaxSize(min, max); // for safety against weird values
        return max;
    }

protected:
    static QSize boundedMaxSize(QSize min, QSize max);

private:
    const QString m_id;
    QObject *const m_thisObj;
    Q_DISABLE_COPY(Widget)
};

inline bool operator==(const Widget *w, const QObject &obj)
{
    //if (!w && !obj)
    //return true;

    return w && w->operator==(&obj);
}

}
