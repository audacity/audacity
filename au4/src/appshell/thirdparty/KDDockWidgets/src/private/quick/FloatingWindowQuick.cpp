/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "FloatingWindowQuick_p.h"
#include "MainWindowBase.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"

#include "TitleBarQuick_p.h"

#include "../Logging_p.h"
#include "../Utils_p.h"
#include "../DropArea_p.h"
#include "../WidgetResizeHandler_p.h"

#include <QQuickView>
#include <QDebug>

using namespace KDDockWidgets;

namespace KDDockWidgets {

class QuickView : public QQuickView
{
    Q_OBJECT
public:
    explicit QuickView(QQmlEngine *qmlEngine, FloatingWindow *floatingWindow)
        : QQuickView(qmlEngine, nullptr)
        , m_floatingWindow(floatingWindow)
    {
        if (Config::self().internalFlags() & Config::InternalFlag_UseTransparentFloatingWindow)
            setColor(QColor(Qt::transparent));

        updateSize();

        connect(m_floatingWindow, &QQuickItem::widthChanged, this, &QuickView::onRootItemWidthChanged);
        connect(m_floatingWindow, &QQuickItem::heightChanged, this, &QuickView::onRootItemHeightChanged);
    }

    ~QuickView();

    bool event(QEvent *ev) override
    {
        if (ev->type() == QEvent::FocusAboutToChange) {
            // qquickwindow.cpp::event(FocusAboutToChange) removes the item grabber. Inibit that
            return true;
        } else if (ev->type() == QEvent::Resize) {
            updateRootItemSize();
        } else if (isNonClientMouseEvent(ev) || ev->type() == QEvent::Move) {
            // Mimic QWidget behaviour: The non-client mouse events go to the QWidget not the QWindow. In our case the QQuickItem.
            // I mean, they also go to QWindow, but for our QtWidgets impl we process them at the QWidget level, so use the same approach
            // so we maintain a single code path for processing mouse events
            qApp->sendEvent(m_floatingWindow, ev);
            return true;
        }

        return QQuickView::event(ev);
    }

    void onRootItemWidthChanged()
    {
        setWidth(int(m_floatingWindow->width()));
    }

    void onRootItemHeightChanged()
    {
        setHeight(int(m_floatingWindow->height()));
    }

    void updateSize()
    {
        resize(m_floatingWindow->size());
    }

    void updateRootItemSize()
    {
        m_floatingWindow->setSize(size());
    }

#ifdef Q_OS_WIN
    bool nativeEvent(const QByteArray &eventType, void *message, Qt5Qt6Compat::qintptr *result) override
    {
        // To enable aero snap we need to tell Windows where's our custom title bar
        if (!m_floatingWindow->beingDeleted() && WidgetResizeHandler::handleWindowsNativeEvent(m_floatingWindow, eventType, message, result))
            return true;

        return QWindow::nativeEvent(eventType, message, result);
    }
#endif
private:
    FloatingWindow *const m_floatingWindow;
};

QuickView::~QuickView() = default;

}


FloatingWindowQuick::FloatingWindowQuick(MainWindowBase *parent)
    : FloatingWindow(QRect(), parent)
    , m_quickWindow(new QuickView(Config::self().qmlEngine(), this))
{
    init();
}

FloatingWindowQuick::FloatingWindowQuick(Frame *frame, QRect suggestedGeometry, MainWindowBase *parent)
    : FloatingWindow(frame, QRect(), parent)
    , m_quickWindow(new QuickView(Config::self().qmlEngine(), this))
{
    init();
    // For QtQuick we need to set the suggested geometry only after we have the QWindow
    setGeometry(suggestedGeometry);
}

FloatingWindowQuick::~FloatingWindowQuick()
{
    // Avoid a bunch of QML warnings and constraints being violated at destruction.
    // Also simply avoiding unneeded work, as QML is destroying stuff 1 by 1
    if (m_dropArea)
        m_dropArea->setWindowIsBeingDestroyed(true);

    QWidgetAdapter::setParent(nullptr);
    if (qobject_cast<QQuickView *>(m_quickWindow)) // QObject cast just to make sure the QWindow is not in ~QObject already
        delete m_quickWindow;
}

QSize FloatingWindowQuick::minimumSize() const
{
    // Doesn't matter if it's not visible. We don't want the min-size to jump around. Also not so
    // easy to track as we don't have layouts
    const int margins = contentsMargins();
    return multiSplitter()->minimumSize() + QSize(0, titleBarHeight()) + QSize(margins * 2, margins * 2);
}

void FloatingWindowQuick::setGeometry(QRect geo)
{
    // Not needed with QtWidgets, but needed with QtQuick as we don't have layouts
    geo.setSize(geo.size().expandedTo(minimumSize()));

    parentItem()->setSize(geo.size());
    m_quickWindow->setGeometry(geo);
}

int FloatingWindowQuick::contentsMargins() const
{
    return m_visualItem->property("margins").toInt();
}

int FloatingWindowQuick::titleBarHeight() const
{
    return m_visualItem->property("titleBarHeight").toInt();
}

QWindow *FloatingWindowQuick::candidateParentWindow() const
{
    if (auto mainWindow = qobject_cast<MainWindowBase *>(QObject::parent())) {
        return mainWindow->QQuickItem::window();
    }

    return nullptr;
}

void FloatingWindowQuick::init()
{
    connect(this, &QQuickItem::visibleChanged, this, [this] {
        if (!isVisible() && !beingDeleted()) {
            scheduleDeleteLater();
        }
    });

    /* for debug:
      connect(m_quickWindow, &QQuickView::focusObjectChanged, this, [this] (QObject *object) {
        qDebug() << "Focus object changed to " << object << this << m_quickWindow;
    });*/


    if (QWindow *transientParent = candidateParentWindow()) {
        m_quickWindow->setTransientParent(candidateParentWindow());
        // This mimics the QWidget behaviour, where we not only have a transient parent but also
        // a parent for cleanup. Calling QWindow::setParent() here would clip it to the parent
        m_quickWindow->QObject::setParent(transientParent);
        m_quickWindow->setObjectName(QStringLiteral("Floating QWindow with parent")); // for debug
    } else {
        m_quickWindow->setObjectName(QStringLiteral("Floating QWindow"));
    }

    QWidgetAdapter::setParent(m_quickWindow->contentItem());
    WidgetResizeHandler::setupWindow(m_quickWindow);
    m_quickWindow->installEventFilter(this); // for window resizing
    maybeCreateResizeHandler();

    m_visualItem = createItem(m_quickWindow->engine(),
                              Config::self().frameworkWidgetFactory()->floatingWindowFilename().toString());
    Q_ASSERT(m_visualItem);

    // Ensure our window size is never smaller than our min-size
    setSize(size().expandedTo(minimumSize()));

    m_visualItem->setParent(this);
    m_visualItem->setParentItem(this);

    m_quickWindow->setFlags(windowFlags());

    updateTitleAndIcon();

    m_quickWindow->show();
}

#include "FloatingWindowQuick.moc"
