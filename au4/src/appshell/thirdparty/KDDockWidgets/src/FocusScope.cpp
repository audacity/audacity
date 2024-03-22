/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief FocusScope
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "FocusScope.h"
#include "DockWidgetBase.h"

#include "private/TitleBar_p.h"
#include "private/Frame_p.h"
#include "private/DockRegistry_p.h"

#include <QObject>
#include <QGuiApplication>
#include <QPointer>

using namespace KDDockWidgets;

// Our Private inherits from QObject since FocusScope can't (Since Frame is already QObject)
class FocusScope::Private : public QObject //clazy:exclude=missing-qobject-macro (breaks unity build with earlier cmake due to including .moc here.)
{
public:
    Private(FocusScope *qq, QWidgetAdapter *thisWidget)
        : q(qq)
        , m_thisWidget(thisWidget)
    {
        connect(qApp, &QGuiApplication::focusObjectChanged,
                this, &Private::onFocusObjectChanged);

        onFocusObjectChanged(qApp->focusObject());
        m_inCtor = false;
    }

    /// @brief Returns whether the last focused widget is the tab widget itself
    bool lastFocusedIsTabWidget() const
    {
        if (!m_lastFocusedInScope)
            return false;

        if (auto metaObj = m_lastFocusedInScope->metaObject()) {
            const auto className = QLatin1String(metaObj->className());

            return className == QLatin1String("KDDockWidgets::TabBarWidget")
                || className == QLatin1String("KDDockWidgets::TabBarQuick");
        }

        return false;
    }

    ~Private() override;

    void setIsFocused(bool);
    void onFocusObjectChanged(QObject *);
    bool isInFocusScope(WidgetType *) const;

    FocusScope *const q;
    QWidgetAdapter *const m_thisWidget;
    bool m_isFocused = false;
    bool m_inCtor = true;
    QPointer<WidgetType> m_lastFocusedInScope;
};

FocusScope::Private::~Private()
{
}

FocusScope::FocusScope(QWidgetAdapter *thisWidget)
    : d(new Private(this, thisWidget))
{
}

FocusScope::~FocusScope()
{
    delete d;
}

bool FocusScope::isFocused() const
{
    return d->m_isFocused;
}

WidgetType *FocusScope::focusedWidget() const
{
    return d->m_lastFocusedInScope;
}

void FocusScope::focus(Qt::FocusReason reason)
{
    if (d->m_lastFocusedInScope && !d->lastFocusedIsTabWidget()) {
        // When we focus the FocusScope, we give focus to the last focused widget, but let's
        // do better than focusing a tab widget. The tab widget itself being focused isn't
        // very useful.
        d->m_lastFocusedInScope->setFocus(reason);
    } else {
        if (auto frame = qobject_cast<Frame *>(d->m_thisWidget)) {
            if (DockWidgetBase *dw = frame->currentDockWidget()) {
                if (auto guest = dw->widget()) {
                    if (guest->focusPolicy() != Qt::NoFocus)
                        guest->setFocus(reason);
                }
            }
        } else {
            // Not a use case right now
            d->m_thisWidget->setFocus(reason);
        }
    }
}

void FocusScope::Private::setIsFocused(bool is)
{
    if (is != m_isFocused) {
        m_isFocused = is;

        if (!m_inCtor) // Hack so we don't call pure-virtual
            /* Q_EMIT */ q->isFocusedChangedCallback();
    }
}

void FocusScope::Private::onFocusObjectChanged(QObject *obj)
{
    auto widget = qobject_cast<WidgetType *>(obj);
    if (!widget) {
        setIsFocused(false);
        return;
    }

    const bool is = isInFocusScope(widget);
    if (is && m_lastFocusedInScope != widget && !qobject_cast<TitleBar *>(obj)) {
        m_lastFocusedInScope = widget;
        setIsFocused(is);
        /* Q_EMIT */ q->focusedWidgetChangedCallback();
    } else {
        setIsFocused(is);
    }
}

bool FocusScope::Private::isInFocusScope(WidgetType *widget) const
{
    WidgetType *p = widget;
    while (p) {
        if (p == m_thisWidget)
            return true;

        p = KDDockWidgets::Private::parentWidget(p);
    }

    return false;
}
