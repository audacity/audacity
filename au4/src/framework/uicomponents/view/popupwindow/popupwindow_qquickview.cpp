/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "popupwindow_qquickview.h"

#include <QTimer>

#include "log.h"

using namespace mu::uicomponents;

PopupWindow_QQuickView::PopupWindow_QQuickView(QObject* parent)
    : IPopupWindow(parent)
{
    setObjectName("PopupWindow");
}

PopupWindow_QQuickView::~PopupWindow_QQuickView()
{
    delete m_view;
}

void PopupWindow_QQuickView::init(QQmlEngine* engine, bool isDialogMode, bool isFrameless)
{
    //! NOTE: do not set the window when constructing the view
    //! This causes different bugs on different OS (e.g., no transparency for popups on windows)
    m_view = new QQuickView(engine, nullptr);

    //! NOTE: We must set the parent
    //! Otherwise, the garbage collector may take ownership of the view and destroy it when we don't expect it
    m_view->QObject::setParent(this);

    m_view->setObjectName("PopupWindow_QQuickView");
    m_view->setResizeMode(QQuickView::SizeRootObjectToView);

    // dialog
    if (isDialogMode) {
        m_view->setFlags(Qt::Dialog);

        if (isFrameless) {
            m_view->setColor(QColor(Qt::transparent));
        } else {
            auto updateBackgroundColor = [this]() {
                if (!m_view) {
                    return;
                }

                QString bgColorStr = uiConfiguration()->currentTheme().values.value(ui::BACKGROUND_PRIMARY_COLOR).toString();
                m_view->setColor(QColor(bgColorStr));
            };

            uiConfiguration()->currentThemeChanged().onNotify(this, [updateBackgroundColor]() {
                updateBackgroundColor();
            });

            updateBackgroundColor();
        }
    }
    // popup
    else {
        Qt::WindowFlags flags(
            Qt::Tool
            | Qt::FramelessWindowHint            // Without border
            | Qt::NoDropShadowWindowHint         // Without system shadow
            | Qt::BypassWindowManagerHint        // Otherwise, it does not work correctly on Gnome (Linux) when resizing)
            );

        m_view->setFlags(flags);
        m_view->setColor(QColor(Qt::transparent));
    }

    // TODO: Can't use new `connect` syntax because the QQuickView::closing
    // has a parameter of type QQuickCloseEvent, which is not public, so we
    // can't include any header for it and it will always be an incomplete
    // type, which is not allowed for the new `connect` syntax.
    //connect(m_view, &QQuickWindow::closing, this, &PopupWindow_QQuickView::aboutToClose);
    connect(m_view, SIGNAL(closing(QQuickCloseEvent*)), this, SIGNAL(aboutToClose(QQuickCloseEvent*)));

    m_view->installEventFilter(this);
}

void PopupWindow_QQuickView::setContent(QQmlComponent* component, QQuickItem* item)
{
    m_view->setContent(QUrl(), component, item);
    m_view->setObjectName(item->objectName() + "_(PopupWindow_QQuickView)");

    connect(item, &QQuickItem::implicitWidthChanged, [this, item]() {
        if (!m_view->isVisible()) {
            return;
        }
        if (item->implicitWidth() != m_view->width()) {
            updateSize(QSize(item->implicitWidth(), item->implicitHeight()));
        }
    });

    connect(item, &QQuickItem::implicitHeightChanged, [this, item]() {
        if (!m_view->isVisible()) {
            return;
        }
        if (item->implicitHeight() != m_view->height()) {
            updateSize(QSize(item->implicitWidth(), item->implicitHeight()));
        }
    });
}

void PopupWindow_QQuickView::forceActiveFocus()
{
    if (!m_view) {
        return;
    }

    QQuickItem* rootObject = m_view->rootObject();
    if (!rootObject) {
        return;
    }

    if (!rootObject->hasActiveFocus()) {
        rootObject->forceActiveFocus();
    }
}

void PopupWindow_QQuickView::show(QScreen* screen, QRect geometry, bool activateFocus)
{
    m_view->setGeometry(geometry);
    m_view->setScreen(screen);

    m_activeFocusOnParentOnClose = activateFocus;

    if (!activateFocus) {
        m_view->setFlag(Qt::WindowDoesNotAcceptFocus);
    }

    QWindow* parent = m_parentWindow ? m_parentWindow : interactiveProvider()->topWindow();
    m_view->setTransientParent(parent);

    connect(parent, &QWindow::visibleChanged, this, [this](){
        if (!m_view->transientParent() || !m_view->transientParent()->isVisible()) {
            close();
        }
    });

    m_view->show();

    if (activateFocus) {
        m_view->requestActivate();
    }

    QQuickItem* item = m_view->rootObject();
    updateSize(QSize(item->implicitWidth(), item->implicitHeight()));

    if (activateFocus) {
        QTimer::singleShot(0, [this]() {
            forceActiveFocus();
        });
    }
}

void PopupWindow_QQuickView::close()
{
    m_view->close();
}

void PopupWindow_QQuickView::raise()
{
    m_view->raise();
}

void PopupWindow_QQuickView::setPosition(QPoint position)
{
    m_view->setPosition(position);
}

QWindow* PopupWindow_QQuickView::qWindow() const
{
    return m_view;
}

bool PopupWindow_QQuickView::isVisible() const
{
    return m_view ? m_view->isVisible() : false;
}

QRect PopupWindow_QQuickView::geometry() const
{
    return m_view ? m_view->geometry() : QRect();
}

QWindow* PopupWindow_QQuickView::parentWindow() const
{
    return m_parentWindow;
}

void PopupWindow_QQuickView::setParentWindow(QWindow* window)
{
    m_parentWindow = window;
}

bool PopupWindow_QQuickView::resizable() const
{
    return m_resizable;
}

void PopupWindow_QQuickView::setResizable(bool resizable)
{
    if (m_resizable == resizable) {
        return;
    }

    m_resizable = resizable;
    if (m_view && isVisible()) {
        updateSize(m_view->size());
    }
}

void PopupWindow_QQuickView::setPosition(const QPoint& position) const
{
    m_view->setPosition(position);
}

void PopupWindow_QQuickView::setOnHidden(const std::function<void()>& callback)
{
    m_onHidden = callback;
}

bool PopupWindow_QQuickView::eventFilter(QObject* watched, QEvent* event)
{
    if (watched == m_view) {
        if (event->type() == QEvent::Hide) {
            if (m_onHidden) {
                m_onHidden();
            }
        }

        if (event->type() == QEvent::FocusIn) {
            m_view->rootObject()->forceActiveFocus();
        }

        if (event->type() == QEvent::MouseButtonPress) {
            forceActiveFocus();
        }
    }

    return QObject::eventFilter(watched, event);
}

void PopupWindow_QQuickView::updateSize(const QSize& newSize)
{
    if (!m_view) {
        return;
    }

    if (m_resizable) {
        m_view->setMinimumSize(newSize);
        m_view->setMaximumSize(QSize(16777215, 16777215));
        m_view->resize(m_view->size().expandedTo(newSize));
    } else {
        m_view->setMinimumSize(newSize);
        m_view->setMaximumSize(newSize);
        m_view->resize(newSize);
    }
}
