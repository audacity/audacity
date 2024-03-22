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

#include "QWidgetAdapter.h"
#include "MainWindowBase.h"

#include "../DockRegistry_p.h"
#include "../Utils_p.h"
#include "../FloatingWindow_p.h"

#include <QResizeEvent>
#include <QMouseEvent>
#include <QQmlComponent>
#include <QQuickItem>
#include <QQmlEngine>
#include <QQuickView>
#include <QScopedValueRollback>

#include <qpa/qplatformwindow.h>
#include <QtGui/private/qhighdpiscaling_p.h>

using namespace KDDockWidgets;

namespace KDDockWidgets {

/**
 * @brief Event filter which redirects mouse events from one QObject to another.
 * Needed for QtQuick to redirect the events from MouseArea to our KDDW classes which derive from Draggable.
 * For QtWidgets it's not needed, as the Draggables are QWidgets themselves.
 */
class MouseEventRedirector : public QObject
{
    Q_OBJECT
public:
    explicit MouseEventRedirector(QObject *eventSource, QObject *eventTarget)
        : QObject(eventTarget)
        , m_eventSource(eventSource)
        , m_eventTarget(eventTarget)
    {
        eventSource->installEventFilter(this);

        // Each source can only have one MouseEventRedirector
        auto oldRedirector = s_mouseEventRedirectors.take(eventSource);
        if (oldRedirector) {
            eventSource->removeEventFilter(oldRedirector);
            oldRedirector->deleteLater();
        }

        s_mouseEventRedirectors.insert(eventSource, this);
    }

    static MouseEventRedirector *redirectorForSource(QObject *eventSource)
    {
        return s_mouseEventRedirectors.value(eventSource);
    }

    ~MouseEventRedirector() override;

    bool eventFilter(QObject *source, QEvent *ev) override
    {
        QMouseEvent *me = mouseEvent(ev);
        if (!me)
            return false;

        // MouseArea.enable is different from Item.enabled. The former still lets the events
        // go through event loops. So query MouseArea.enable here and bail out if false.
        const QVariant v = source->property("enabled");
        if (v.isValid() && !v.toBool())
            return false;

        // Finally send the event
        m_eventTarget->setProperty("cursorPosition", m_eventSource->property("cursorPosition"));
        qApp->sendEvent(m_eventTarget, me);
        m_eventTarget->setProperty("cursorPosition", CursorPosition_Undefined);

        return false;
    }

    QObject *const m_eventSource;
    QObject *const m_eventTarget;
    static QHash<QObject *, MouseEventRedirector *> s_mouseEventRedirectors;
};

QHash<QObject *, MouseEventRedirector *> MouseEventRedirector::s_mouseEventRedirectors = {};

MouseEventRedirector::~MouseEventRedirector()
{
    s_mouseEventRedirectors.remove(m_eventSource);
}

}

static bool flagsAreTopLevelFlags(Qt::WindowFlags flags)
{
    return flags & (Qt::Window | Qt::Tool);
}

static QQuickItem *actualParentItem(QQuickItem *candidateParentItem, Qt::WindowFlags flags)
{
    // When we have a top-level, such as FloatingWindow, we only want to set QObject parentship
    // and not parentItem.
    return flagsAreTopLevelFlags(flags) ? nullptr
                                        : candidateParentItem;
}

QWidgetAdapter::QWidgetAdapter(QQuickItem *parent, Qt::WindowFlags flags)
    : QQuickItem(actualParentItem(parent, flags))
    , m_windowFlags(flags)
{
    if (parent && flagsAreTopLevelFlags(flags)) {
        // See comment in actualParentItem(). We set only the QObject parent. Mimics QWidget behaviour
        QObject::setParent(parent);
    }

    connect(this, &QQuickItem::widthChanged, this, [this] {
        onResize(size());
        updateGeometry();
    });

    connect(this, &QQuickItem::heightChanged, this, [this] {
        if (!m_windowIsBeingDestroyed) { // If Window is being destroyed we don't bother
            onResize(size());
            updateGeometry();
        }
    });

    qApp->installEventFilter(this);

    setSize(QSize(800, 800));
}

QWidgetAdapter::~QWidgetAdapter()
{
}

void QWidgetAdapter::raiseAndActivate()
{
    if (QWindow *w = windowHandle()) {
        w->raise();
        w->requestActivate();
    }
}

void QWidgetAdapter::setWindowOpacity(qreal level)
{
    if (QWindow *w = windowHandle())
        w->setOpacity(level);
}

bool QWidgetAdapter::onResize(QSize)
{
    return false;
}
void QWidgetAdapter::onLayoutRequest()
{
}
void QWidgetAdapter::onMousePress()
{
}
void QWidgetAdapter::onMouseMove(QPoint)
{
}
void QWidgetAdapter::onMouseRelease()
{
}
void QWidgetAdapter::onCloseEvent(QCloseEvent *)
{
}

void QWidgetAdapter::onResizeEvent(QResizeEvent *)
{
    updateNormalGeometry();
}

void QWidgetAdapter::onMoveEvent(QMoveEvent *)
{
    updateNormalGeometry();
}

void QWidgetAdapter::itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &data)
{
    QQuickItem::itemChange(change, data);

    // Emulate the QWidget behaviour as QQuickItem doesn't receive some QEvents.
    switch (change) {
    case QQuickItem::ItemParentHasChanged: {
        QEvent ev(QEvent::ParentChange);
        qApp->sendEvent(this, &ev); // Not calling event() directly, otherwise it would skip event filters
        Q_EMIT parentChanged(this);
        break;
    }
    case QQuickItem::ItemVisibleHasChanged: {
        if (m_inSetParent) {
            // Setting parent to nullptr will emit visible true in QtQuick
            // which we don't want, as we're going to hide it (as we do with QtWidgets)
            break;
        }

        QEvent ev(isVisible() ? QEvent::Show : QEvent::Hide);
        event(&ev);
        break;
    }
    default:
        break;
    }
}

void QWidgetAdapter::updateNormalGeometry()
{
    QWindow* window = windowHandle();
    if (!window) {
        return;
    }

    QRect normalGeometry;
    if (const QPlatformWindow *pw = window->handle()) {
        normalGeometry = QHighDpi::fromNativePixels(pw->normalGeometry(), pw->window());
    }

    if (!normalGeometry.isValid() && isNormalWindowState(window->windowState())) {
        normalGeometry = window->geometry();
    }

    if (normalGeometry.isValid()) {
        setNormalGeometry(normalGeometry);
    }
}

void QWidgetAdapter::QQUICKITEMgeometryChanged(const QRectF &newGeometry, const QRectF &oldGeometry)
{
    // Send a few events manually, since QQuickItem doesn't do it for us.
    QQuickItem::QQUICKITEMgeometryChanged(newGeometry, oldGeometry);

    // Not calling event() directly, otherwise it would skip event filters

    if (newGeometry.size() != oldGeometry.size()) {
        QEvent ev(QEvent::Resize);
        qApp->sendEvent(this, &ev);
    }

    if (newGeometry.topLeft() != oldGeometry.topLeft()) {
        QEvent ev(QEvent::Move);
        qApp->sendEvent(this, &ev);
    }

    Q_EMIT itemGeometryChanged();
}

void QWidgetAdapter::raise()
{
    if (isTopLevel()) {
        if (QWindow *w = windowHandle())
            w->raise();
    } else if (auto p = QQuickItem::parentItem()) {
        // It's not a top-level, so just increase its Z-order
        const auto siblings = p->childItems();
        QQuickItem *last = siblings.last();
        if (last != this)
            stackAfter(last);
    }
}

QSize QWidgetAdapter::minimumSize() const
{
    if (m_isWrapper) {
        const auto children = childItems();
        if (!children.isEmpty()) {
            const QSize min = children.constFirst()->property("kddockwidgets_min_size").toSize();
            return min.expandedTo(Layouting::Item::hardcodedMinimumSize);
        }
    }

    const QSize min = property("kddockwidgets_min_size").toSize();
    return min.expandedTo(Layouting::Item::hardcodedMinimumSize);
}

QSize QWidgetAdapter::maximumSize() const
{
    if (m_isWrapper) {
        const auto children = childItems();
        if (!children.isEmpty()) {
            const QSize max = children.constFirst()->property("kddockwidgets_max_size").toSize();
            return max.isEmpty() ? Layouting::Item::hardcodedMaximumSize
                                 : max.boundedTo(Layouting::Item::hardcodedMaximumSize);
        }
    }

    const QSize max = property("kddockwidgets_max_size").toSize();
    return max.isEmpty() ? Layouting::Item::hardcodedMaximumSize
                         : max.boundedTo(Layouting::Item::hardcodedMaximumSize);
}

WId QWidgetAdapter::winId() const
{
    if (QWindow *w = windowHandle())
        return w->winId();

    return WId(-1);
}

FloatingWindow *QWidgetAdapter::floatingWindow() const
{
    if (auto fw = qobject_cast<FloatingWindow *>(window()))
        return fw;

    return nullptr;
}

QRect QWidgetAdapter::geometry() const
{
    if (isTopLevel()) {
        if (QWindow *w = windowHandle()) {
            return w->geometry();
        }
    }

    return KDDockWidgets::Private::geometry(this);
}

QRect QWidgetAdapter::normalGeometry() const
{
    return m_normalGeometry;
}

void QWidgetAdapter::setNormalGeometry(QRect geo)
{
    m_normalGeometry = geo;
}

QRect QWidgetAdapter::rect() const
{
    return QRectF(0, 0, width(), height()).toRect();
}

QPoint QWidgetAdapter::pos() const
{
    return geometry().topLeft();
}

void QWidgetAdapter::show()
{
    setVisible(true);
}

void QWidgetAdapter::setFixedHeight(int height)
{
    setHeight(height);
}

void QWidgetAdapter::setFixedWidth(int width)
{
    setWidth(width);
}

void QWidgetAdapter::setGeometry(QRect rect)
{
    setWidth(rect.width());
    setHeight(rect.height());
    move(rect.topLeft());
}

QRect QWidgetAdapter::frameGeometry() const
{
    if (QWindow *w = windowHandle())
        return w->frameGeometry();

    return geometry();
}

void QWidgetAdapter::grabMouse()
{
    QQuickItem::grabMouse();
}

void QWidgetAdapter::releaseMouse()
{
    QQuickItem::ungrabMouse();
}

void QWidgetAdapter::releaseKeyboard()
{
    // Not needed apparently
}

void QWidgetAdapter::setMinimumSize(QSize sz)
{
    if (minimumSize() != sz) {
        setProperty("kddockwidgets_min_size", sz);
        updateGeometry();
    }
}

void QWidgetAdapter::setMaximumSize(QSize sz)
{
    if (maximumSize() != sz) {
        setProperty("kddockwidgets_max_size", sz);
        updateGeometry();
    }
}

void QWidgetAdapter::setMaximumSize(int w, int h)
{
    QWidgetAdapter::setMaximumSize(QSize(w, h));
}

void QWidgetAdapter::setMinimumSize(int w, int h)
{
    QWidgetAdapter::setMinimumSize(QSize(w, h));
}

void QWidgetAdapter::updateGeometry()
{
    Q_EMIT geometryUpdated();
}

void QWidgetAdapter::resize(QSize sz)
{
    setWidth(sz.width());
    setHeight(sz.height());
}

void QWidgetAdapter::resize(int w, int h)
{
    resize({ w, h });
}

bool QWidgetAdapter::isWindow() const
{
    QQuickItem *parent = parentItem();
    if (!parent)
        return true;

    if (QQuickView *w = quickView()) {
        if (parent == w->contentItem() || parent == w->rootObject())
            return true;
    }

    return false;
}

bool QWidgetAdapter::isMaximized() const
{
    if (QWindow *w = windowHandle())
        return w->windowStates() & Qt::WindowMaximized;

    return false;
}

bool QWidgetAdapter::isMinimized() const
{
    if (QWindow *w = windowHandle())
        return w->windowStates() & Qt::WindowMinimized;

    return false;
}

bool KDDockWidgets::QWidgetAdapter::isActiveWindow() const
{
    if (QWindow *w = windowHandle())
        return w->isActive();

    return false;
}

void QWidgetAdapter::showMaximized()
{
    if (QWindow *w = windowHandle())
        w->showMaximized();
}

void QWidgetAdapter::showMinimized()
{
    if (QWindow *w = windowHandle())
        w->showMinimized();
}

void QWidgetAdapter::showNormal()
{
    if (QWindow *w = windowHandle())
        w->showNormal();
}

QQuickView *QWidgetAdapter::quickView() const
{
    return qobject_cast<QQuickView *>(QQuickItem::window());
}

QWindow *QWidgetAdapter::windowHandle() const
{
    return QQuickItem::window();
}

QWidgetAdapter *QWidgetAdapter::window() const
{
    // We return the top-most QWidgetAdapter

    if (QWidgetAdapter *w = parentWidget(/*includeTransient =*/false))
        return w->window();

    return const_cast<QWidgetAdapter *>(this);
}

QWidgetAdapter *QWidgetAdapter::parentWidget(bool includeTransient) const
{
    QQuickItem *p = parentItem();
    while (p) {
        if (auto qa = qobject_cast<QWidgetAdapter *>(p))
            return qa;

        p = p->parentItem();
    }

    if (includeTransient) {
        if (QQuickView *w = quickView()) {
            // Here we're mimicking QWidget::parentWidget(), which can return the transient parent of the QWindow.
            MainWindowBase *mw = DockRegistry::self()->mainWindowForHandle(w->transientParent());
            if (mw)
                return mw;
        }
    }


    return nullptr;
}

QPoint QWidgetAdapter::mapToGlobal(QPoint pt) const
{
    return QQuickItem::mapToGlobal(pt).toPoint();
}

QPoint QWidgetAdapter::mapFromGlobal(QPoint pt) const
{
    return QQuickItem::mapFromGlobal(pt).toPoint();
}

QPoint QWidgetAdapter::mapTo(const QQuickItem *parent, QPoint pos) const
{
    if (!parent)
        return {};

    return parent->mapFromGlobal(QQuickItem::mapToGlobal(pos)).toPoint();
}

bool QWidgetAdapter::testAttribute(Qt::WidgetAttribute attr) const
{
    return m_widgetAttributes & attr;
}

void QWidgetAdapter::setAttribute(Qt::WidgetAttribute attr, bool enable)
{
    if (enable)
        m_widgetAttributes |= attr;
    else
        m_widgetAttributes &= ~attr;
}

void QWidgetAdapter::setWindowTitle(const QString &title)
{
    if (QWindow *window = windowHandle())
        window->setTitle(title);
}

void QWidgetAdapter::setWindowIcon(const QIcon &icon)
{
    if (QWindow *window = windowHandle())
        window->setIcon(icon);
}

bool QWidgetAdapter::close()
{
    QCloseEvent ev;
    onCloseEvent(&ev);

    if (ev.isAccepted()) {
        setVisible(false);
        return true;
    }

    return false;
}

QQuickItem *QWidgetAdapter::childAt(QPoint p) const
{
    return QQuickItem::childAt(p.x(), p.y());
}

void QWidgetAdapter::move(QPoint pt)
{
    move(pt.x(), pt.y());
}

void QWidgetAdapter::move(int x, int y)
{
    if (isTopLevel()) {
        if (QWindow *w = windowHandle()) {
            w->setPosition(x, y);
            return;
        }
    }

    setX(x);
    setY(y);
    setAttribute(Qt::WA_Moved);
}

void QWidgetAdapter::setSize(QSize size)
{
    QQuickItem::setSize(QSizeF(size));
}

void QWidgetAdapter::setParent(QQuickItem *p)
{
    {
        QScopedValueRollback<bool> guard(m_inSetParent, true);

        QQuickItem::setParent(p);
        QQuickItem::setParentItem(p);
    }

    // Mimic QWidget::setParent(), hide widget when setting parent
    if (!p)
        setVisible(false);
}

void QWidgetAdapter::activateWindow()
{
    if (QWindow *w = windowHandle())
        w->requestActivate();
}

void QWidgetAdapter::setSizePolicy(QSizePolicy sp)
{
    m_sizePolicy = sp;
}

QSizePolicy QWidgetAdapter::sizePolicy() const
{
    return m_sizePolicy;
}

QSize QWidgetAdapter::sizeHint() const
{
    return m_sizeHint;
}

bool QWidgetAdapter::hasFocus() const
{
    return hasActiveFocus();
}

void QWidgetAdapter::setWindowFlag(int flag, bool enable)
{
    Q_UNUSED(flag);
    Q_UNUSED(enable);
}

Qt::WindowFlags QWidgetAdapter::windowFlags() const
{
    return m_windowFlags;
}

/** static */
QQuickItem *QWidgetAdapter::createItem(QQmlEngine *engine, const QString &filename)
{
    QQmlComponent component(engine, filename);
    QObject *obj = component.create();
    if (!obj) {
        qWarning() << Q_FUNC_INFO << component.errorString();
        return nullptr;
    }

    return qobject_cast<QQuickItem *>(obj);
}

void QWidgetAdapter::makeItemFillParent(QQuickItem *item)
{
    // This is equivalent to "anchors.fill: parent

    if (!item) {
        qWarning() << Q_FUNC_INFO << "Invalid item";
        return;
    }

    QQuickItem *parentItem = item->parentItem();
    if (!parentItem) {
        qWarning() << Q_FUNC_INFO << "Invalid parentItem for" << item;
        return;
    }

    QObject *anchors = item->property("anchors").value<QObject *>();
    if (!anchors) {
        qWarning() << Q_FUNC_INFO << "Invalid anchors for" << item;
        return;
    }

    anchors->setProperty("fill", QVariant::fromValue(parentItem));
}

void QWidgetAdapter::setFlag(Qt::WindowType f, bool on)
{
    if (on) {
        m_windowFlags |= f;
    } else {
        m_windowFlags &= ~f;
    }
}

Qt::FocusPolicy QWidgetAdapter::focusPolicy() const
{
    return m_focusPolicy;
}

void QWidgetAdapter::setFocusPolicy(Qt::FocusPolicy policy)
{
    m_focusPolicy = policy;
}

void QWidgetAdapter::setFocus(Qt::FocusReason reason)
{
    QQuickItem::setFocus(true, reason);
    forceActiveFocus(reason);
}

void QWidgetAdapter::render(QPainter *)
{
    qWarning() << Q_FUNC_INFO << "Implement me";
}

void QWidgetAdapter::setMouseTracking(bool enabled)
{
    m_mouseTrackingEnabled = enabled;
}

bool QWidgetAdapter::event(QEvent *ev)
{
    if (ev->type() == QEvent::Close)
        onCloseEvent(static_cast<QCloseEvent *>(ev));

    return QQuickItem::event(ev);
}

bool QWidgetAdapter::eventFilter(QObject *watched, QEvent *ev)
{
    if (qobject_cast<QWindow *>(watched)) {
        if (m_mouseTrackingEnabled) {
            switch (ev->type()) {
            case QEvent::MouseMove:
            case QEvent::MouseButtonPress:
            case QEvent::MouseButtonRelease:
                ev->ignore();
                qApp->sendEvent(this, ev);
                //qDebug() << "Mouse event" << ev;
                if (ev->isAccepted())
                    return true;
                break;
            default:
                break;
            }
        }

        if (ev->type() == QEvent::Resize) {
            onResizeEvent(static_cast<QResizeEvent *>(ev));
        } else if (ev->type() == QEvent::Move) {
            onMoveEvent(static_cast<QMoveEvent *>(ev));
        }
    }

    return QQuickItem::eventFilter(watched, ev);
}

QScreen *QWidgetAdapter::screen() const
{
    if (QQuickView *w = quickView()) {
        return w->screen();
    }

    return nullptr;
}

void QWidgetAdapter::setWindowIsBeingDestroyed(bool is)
{
    m_windowIsBeingDestroyed = is;
}

void QWidgetAdapter::create()
{
    // Nothing to do, for QtQuick ?
}

QQuickItem *KDDockWidgets::Private::widgetForWindow(QWindow *window)
{
    if (!window)
        return nullptr;

    return window->property("kddockwidgets_qwidget").value<QQuickItem *>();
}

void QWidgetAdapter::redirectMouseEvents(QObject *source)
{
    if (auto existingRedirector = MouseEventRedirector::redirectorForSource(source)) {
        if (existingRedirector->m_eventTarget == this) {
            // Nothing to do. The specified event source is already redirecting to this instance
            return;
        }
    }

    new MouseEventRedirector(source, this);
}

void QWidgetAdapter::setIsWrapper()
{
    m_isWrapper = true;
}

bool QWidgetAdapter::isWrapper() const
{
    return m_isWrapper;
}

LayoutGuestWidget::~LayoutGuestWidget() = default;

#include "QWidgetAdapter_quick.moc"
