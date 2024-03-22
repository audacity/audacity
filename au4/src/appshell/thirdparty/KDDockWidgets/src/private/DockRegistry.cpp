/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DockRegistry_p.h"
#include "Config.h"
#include "DockWidgetBase.h"
#include "DockWidgetBase_p.h"
#include "FloatingWindow_p.h"
#include "LayoutWidget_p.h"
#include "Logging_p.h"
#include "MainWindowMDI.h"
#include "Position_p.h"
#include "QWidgetAdapter.h"
#include "SideBar_p.h"
#include "Utils_p.h"
#include "WidgetResizeHandler_p.h"
#include "WindowBeingDragged_p.h"

#include <QPointer>
#include <QDebug>
#include <QGuiApplication>
#include <QWindow>

#ifdef KDDOCKWIDGETS_QTWIDGETS
#include "DebugWindow_p.h"
#else
#include "quick/QmlTypes.h"
#endif

using namespace KDDockWidgets;

static void initKDDockWidgetResources()
{
#if defined(KDDOCKWIDGETS_STATICLIB) || defined(QT_STATIC)
    Q_INIT_RESOURCE(kddockwidgets_resources);
#if defined(KDDOCKWIDGETS_QTQUICK)
    Q_INIT_RESOURCE(kddockwidgets_qtquick);
#endif
#endif
}

DockRegistry::DockRegistry(QObject *parent)
    : QObject(parent)
{
    qApp->installEventFilter(this);

#ifdef KDDOCKWIDGETS_QTWIDGETS

#ifdef DOCKS_DEVELOPER_MODE
    if (qEnvironmentVariableIntValue("KDDOCKWIDGETS_SHOW_DEBUG_WINDOW") == 1) {
        auto dv = new Debug::DebugWindow();
        dv->show();
    }
#endif

#else
    KDDockWidgets::registerQmlTypes();
    QQuickWindow::setDefaultAlphaBuffer(true);
#endif

    connect(qApp, &QGuiApplication::focusObjectChanged,
            this, &DockRegistry::onFocusObjectChanged);

    initKDDockWidgetResources();
}

DockRegistry::~DockRegistry()
{
}

void DockRegistry::maybeDelete()
{
    if (isEmpty())
        delete this;
}

void DockRegistry::onFocusObjectChanged(QObject *obj)
{
    auto p = qobject_cast<WidgetType *>(obj);
    while (p) {
        if (auto frame = qobject_cast<Frame *>(p)) {
            // Special case: The focused widget is inside the frame but not inside the dockwidget.
            // For example, it's a line edit in the QTabBar. We still need to send the signal for
            // the current dw in the tab group
            if (auto dw = frame->currentDockWidget()) {
                setFocusedDockWidget(dw);
            }

            return;
        }

        if (auto dw = qobject_cast<DockWidgetBase *>(p)) {
            DockRegistry::self()->setFocusedDockWidget(dw);
            return;
        }
        p = KDDockWidgets::Private::parentWidget(p);
    }

    setFocusedDockWidget(nullptr);
}

void DockRegistry::setFocusedDockWidget(DockWidgetBase *dw)
{
    if (m_focusedDockWidget.data() == dw)
        return;

    if (m_focusedDockWidget)
        Q_EMIT m_focusedDockWidget->isFocusedChanged(false);

    m_focusedDockWidget = dw;

    if (m_focusedDockWidget)
        Q_EMIT m_focusedDockWidget->isFocusedChanged(true);
}

bool DockRegistry::isEmpty(bool excludeBeingDeleted) const
{
    if (!m_dockWidgets.isEmpty() || !m_mainWindows.isEmpty())
        return false;

    return excludeBeingDeleted ? !hasFloatingWindows()
                               : m_floatingWindows.isEmpty();
}

void DockRegistry::checkSanityAll(bool dumpLayout)
{
    for (auto layout : std::as_const(m_layouts)) {
        layout->checkSanity();
        if (dumpLayout)
            layout->dumpLayout();
    }
}

bool DockRegistry::isProcessingAppQuitEvent() const
{
    return m_isProcessingAppQuitEvent;
}

bool DockRegistry::affinitiesMatch(const QStringList &affinities1, const QStringList &affinities2) const
{
    if (affinities1.isEmpty() && affinities2.isEmpty())
        return true;

    for (const QString &a1 : affinities1) {
        for (const QString &a2 : affinities2) {
            if (a1 == a2)
                return true;
        }
    }

    return false;
}

QStringList DockRegistry::mainWindowsNames() const
{
    QStringList names;
    names.reserve(m_mainWindows.size());
    for (auto mw : m_mainWindows)
        names.push_back(mw->uniqueName());

    return names;
}

QStringList DockRegistry::dockWidgetNames() const
{
    QStringList names;
    names.reserve(m_dockWidgets.size());
    for (auto dw : m_dockWidgets)
        names.push_back(dw->uniqueName());

    return names;
}

bool DockRegistry::isProbablyObscured(QWindow *window, FloatingWindow *exclude) const
{
    if (!window)
        return false;

    const QRect geo = window->geometry();
    for (FloatingWindow *fw : m_floatingWindows) {
        QWindow *fwWindow = fw->QWidgetAdapter::windowHandle();
        if (fw == exclude || fwWindow == window)
            continue;

        if (fwWindow->geometry().intersects(geo)) {
            // fw might be below, but we don't have a way to check. So be conservative and return true.
            return true;
        }
    }

    // Floating windows are Tool (keep above), unless we disabled it in Config
    const bool targetIsToolWindow = KDDockWidgets::usesUtilityWindows() && floatingWindowForHandle(window) != nullptr;

    for (MainWindowBase *mw : m_mainWindows) {
        QWindow *mwWindow = mw->window()->windowHandle();

        if (mwWindow && mwWindow != window && !targetIsToolWindow && mwWindow->geometry().intersects(geo)) {
            // Two main windows that intersect. Return true. If the target is a tool window it will be above, so we don't care.
            return true;
        }
    }

    return false;
}

bool DockRegistry::isProbablyObscured(QWindow *target, WindowBeingDragged *exclude) const
{
    FloatingWindow *fw = exclude ? exclude->floatingWindow()
                                 : nullptr; // It's null on Wayland. On wayland obscuring never happens anyway, so not a problem.

    return isProbablyObscured(target, fw);
}

SideBarLocation DockRegistry::sideBarLocationForDockWidget(const DockWidgetBase *dw) const
{
    if (SideBar *sb = sideBarForDockWidget(dw))
        return sb->location();

    return SideBarLocation::None;
}

SideBar *DockRegistry::sideBarForDockWidget(const DockWidgetBase *dw) const
{
    for (auto mw : m_mainWindows) {
        if (SideBar *sb = mw->sideBarForDockWidget(dw))
            return sb;
    }

    return nullptr;
}

Frame *DockRegistry::frameInMDIResize() const
{
    for (auto mw : m_mainWindows) {
        if (!mw->isMDI())
            continue;

        LayoutWidget *layout = mw->layoutWidget();
        const QList<Frame *> frames = layout->frames();
        for (Frame *frame : frames) {
            if (WidgetResizeHandler *wrh = frame->resizeHandler()) {
                if (wrh->isResizing())
                    return frame;
            }
        }
    }

    return nullptr;
}

MainWindowBase::List DockRegistry::mainWindowsWithAffinity(const QStringList &affinities) const
{
    MainWindowBase::List result;
    result.reserve(m_mainWindows.size());

    for (auto mw : m_mainWindows) {
        const QStringList mwAffinities = mw->affinities();
        if (affinitiesMatch(mwAffinities, affinities))
            result << mw;
    }

    return result;
}

LayoutWidget *DockRegistry::layoutForItem(const Layouting::Item *item) const
{
    if (!item->hostWidget())
        return nullptr;

    if (auto ms = qobject_cast<LayoutWidget *>(item->hostWidget()->asQObject()))
        return ms;

    return nullptr;
}

bool DockRegistry::itemIsInMainWindow(const Layouting::Item *item) const
{
    if (auto layout = layoutForItem(item))
        return layout->isInMainWindow();

    return false;
}

DockRegistry *DockRegistry::self()
{
    static QPointer<DockRegistry> s_dockRegistry;

    if (!s_dockRegistry) {
        s_dockRegistry = new DockRegistry();
    }

    return s_dockRegistry;
}

void DockRegistry::registerDockWidget(DockWidgetBase *dock)
{
    if (dock->uniqueName().isEmpty()) {
        qWarning() << Q_FUNC_INFO << "DockWidget" << dock << " doesn't have an ID";
    } else if (auto other = dockByName(dock->uniqueName())) {
        qWarning() << Q_FUNC_INFO << "Another DockWidget" << other << "with name" << dock->uniqueName() << " already exists." << dock;
    }

    m_dockWidgets << dock;
}

void DockRegistry::unregisterDockWidget(DockWidgetBase *dock)
{
    if (m_focusedDockWidget == dock)
        m_focusedDockWidget = nullptr;

    m_dockWidgets.removeOne(dock);
    maybeDelete();
}

void DockRegistry::registerMainWindow(MainWindowBase *mainWindow)
{
    if (mainWindow->uniqueName().isEmpty()) {
        qWarning() << Q_FUNC_INFO << "MainWindow" << mainWindow << " doesn't have an ID";
    } else if (auto other = mainWindowByName(mainWindow->uniqueName())) {
        qWarning() << Q_FUNC_INFO << "Another MainWindow" << other << "with name" << mainWindow->uniqueName() << " already exists." << mainWindow;
    }

    m_mainWindows << mainWindow;
}

void DockRegistry::unregisterMainWindow(MainWindowBase *mainWindow)
{
    m_mainWindows.removeOne(mainWindow);
    maybeDelete();
}

void DockRegistry::registerFloatingWindow(FloatingWindow *window)
{
    m_floatingWindows << window;
}

void DockRegistry::unregisterFloatingWindow(FloatingWindow *window)
{
    m_floatingWindows.removeOne(window);
    maybeDelete();
}

void DockRegistry::registerLayout(LayoutWidget *layout)
{
    m_layouts << layout;
}

void DockRegistry::unregisterLayout(LayoutWidget *layout)
{
    m_layouts.removeOne(layout);
}

void DockRegistry::registerFrame(Frame *frame)
{
    m_frames << frame;
}

void DockRegistry::unregisterFrame(Frame *frame)
{
    m_frames.removeOne(frame);
}

DockWidgetBase *DockRegistry::focusedDockWidget() const
{
    return m_focusedDockWidget;
}

bool DockRegistry::containsDockWidget(const QString &uniqueName) const
{
    return dockByName(uniqueName) != nullptr;
}

bool DockRegistry::containsMainWindow(const QString &uniqueName) const
{
    return mainWindowByName(uniqueName) != nullptr;
}

DockWidgetBase *DockRegistry::dockByName(const QString &name, DockByNameFlags flags) const
{
    for (auto dock : std::as_const(m_dockWidgets)) {
        if (dock->uniqueName() == name)
            return dock;
    }

    if (flags.testFlag(DockByNameFlag::ConsultRemapping)) {
        // Name doesn't exist, let's check if it was remapped during a layout restore.
        const QString newName = m_dockWidgetIdRemapping.value(name);
        if (!newName.isEmpty())
            return dockByName(newName);
    }

    if (flags.testFlag(DockByNameFlag::CreateIfNotFound)) {
        // DockWidget doesn't exist, ask to create it
        if (auto factoryFunc = Config::self().dockWidgetFactoryFunc()) {
            auto dw = factoryFunc(name);
            if (dw && dw->uniqueName() != name) {
                // Very special case
                // The user's factory function returned a dock widget with a different ID.
                // We support it. Save the mapping though.
                m_dockWidgetIdRemapping.insert(name, dw->uniqueName());
            }
            return dw;
        } else {
            qWarning() << Q_FUNC_INFO << "Couldn't find dock widget" << name;
        }
    }

    return nullptr;
}

MainWindowBase *DockRegistry::mainWindowByName(const QString &name) const
{
    for (auto mainWindow : std::as_const(m_mainWindows)) {
        if (mainWindow->uniqueName() == name)
            return mainWindow;
    }

    return nullptr;
}

MainWindowMDI *DockRegistry::mdiMainWindowByName(const QString &name) const
{
    return qobject_cast<MainWindowMDI *>(mainWindowByName(name));
}

DockWidgetBase *DockRegistry::dockWidgetForGuest(QWidgetOrQuick *guest) const
{
    if (!guest)
        return nullptr;

    for (DockWidgetBase *dw : m_dockWidgets) {
        if (dw->widget() == guest)
            return dw;
    }

    return nullptr;
}

bool DockRegistry::isSane() const
{
    QSet<QString> names;
    for (auto dock : std::as_const(m_dockWidgets)) {
        const QString name = dock->uniqueName();
        if (name.isEmpty()) {
            qWarning() << "DockRegistry::isSane: DockWidget" << dock << "is missing a name";
            return false;
        } else if (names.contains(name)) {
            qWarning() << "DockRegistry::isSane: dockWidgets with duplicate names:" << name;
            return false;
        } else {
            names.insert(name);
        }
    }

    names.clear();
    for (auto mainwindow : std::as_const(m_mainWindows)) {
        const QString name = mainwindow->uniqueName();
        if (name.isEmpty()) {
            qWarning() << "DockRegistry::isSane: MainWindow" << mainwindow << "is missing a name";
            return false;
        } else if (names.contains(name)) {
            qWarning() << "DockRegistry::isSane: mainWindow with duplicate names:" << name;
            return false;
        } else {
            names.insert(name);
        }

        if (!mainwindow->multiSplitter()->checkSanity())
            return false;
    }

    return true;
}

const DockWidgetBase::List DockRegistry::dockwidgets() const
{
    return m_dockWidgets;
}

const DockWidgetBase::List DockRegistry::dockWidgets(const QStringList &names)
{
    DockWidgetBase::List result;
    result.reserve(names.size());

    for (auto dw : std::as_const(m_dockWidgets)) {
        if (names.contains(dw->uniqueName()))
            result.push_back(dw);
    }

    return result;
}

const MainWindowBase::List DockRegistry::mainWindows(const QStringList &names)
{
    MainWindowBase::List result;
    result.reserve(names.size());

    for (auto mw : std::as_const(m_mainWindows)) {
        if (names.contains(mw->uniqueName()))
            result.push_back(mw);
    }

    return result;
}

const DockWidgetBase::List DockRegistry::closedDockwidgets() const
{
    DockWidgetBase::List result;
    result.reserve(m_dockWidgets.size());

    for (DockWidgetBase *dw : m_dockWidgets) {
        if (dw->parent() == nullptr && !dw->isVisible())
            result.push_back(dw);
    }

    return result;
}

const MainWindowBase::List DockRegistry::mainwindows() const
{
    return m_mainWindows;
}

const QVector<LayoutWidget *> DockRegistry::layouts() const
{
    return m_layouts;
}

const Frame::List DockRegistry::frames() const
{
    return m_frames;
}

const QVector<FloatingWindow *> DockRegistry::floatingWindows(bool includeBeingDeleted) const
{
    // Returns all the FloatingWindow which aren't being deleted
    QVector<FloatingWindow *> result;
    result.reserve(m_floatingWindows.size());
    for (FloatingWindow *fw : m_floatingWindows) {
        if (includeBeingDeleted || !fw->beingDeleted())
            result.push_back(fw);
    }

    return result;
}

const QVector<QWindow *> DockRegistry::floatingQWindows() const
{
    QVector<QWindow *> windows;
    windows.reserve(m_floatingWindows.size());
    for (FloatingWindow *fw : m_floatingWindows) {
        if (!fw->beingDeleted()) {
            if (QWindow *window = fw->windowHandle()) {
                window->setProperty("kddockwidgets_qwidget", QVariant::fromValue<QWidgetOrQuick *>(fw)); // Since QWidgetWindow is private API
                windows.push_back(window);
            } else {
                qWarning() << Q_FUNC_INFO << "FloatingWindow doesn't have QWindow";
            }
        }
    }

    return windows;
}

bool DockRegistry::hasFloatingWindows() const
{
    return std::any_of(m_floatingWindows.begin(), m_floatingWindows.end(), [](FloatingWindow *fw) {
        return !fw->beingDeleted();
    });
}

QWindow *DockRegistry::windowForHandle(WId id) const
{
    const QWindowList windows = qApp->topLevelWindows();
    for (QWindow *w : windows) {
        if (w->isVisible() && w->handle()) {
            if (w->winId() == id)
                return w;
        }
    }
    return nullptr;
}

FloatingWindow *DockRegistry::floatingWindowForHandle(QWindow *windowHandle) const
{
    for (FloatingWindow *fw : m_floatingWindows) {
        if (fw->windowHandle() == windowHandle)
            return fw;
    }

    return nullptr;
}

FloatingWindow *DockRegistry::floatingWindowForHandle(WId hwnd) const
{
    for (FloatingWindow *fw : m_floatingWindows) {
        if (fw->windowHandle() && fw->windowHandle()->winId() == hwnd)
            return fw;
    }

    return nullptr;
}

MainWindowBase *DockRegistry::mainWindowForHandle(QWindow *windowHandle) const
{
    for (MainWindowBase *mw : m_mainWindows) {
        if (mw->windowHandle() == windowHandle)
            return mw;
    }

    return nullptr;
}

QWidgetOrQuick *DockRegistry::topLevelForHandle(QWindow *windowHandle) const
{
    if (auto fw = floatingWindowForHandle(windowHandle))
        return fw;

    if (auto mw = mainWindowForHandle(windowHandle))
        return mw;

    return nullptr;
}

QVector<QWindow *> DockRegistry::topLevels(bool excludeFloatingDocks) const
{
    QVector<QWindow *> windows;
    windows.reserve(m_floatingWindows.size() + m_mainWindows.size());

    if (!excludeFloatingDocks) {
        for (FloatingWindow *fw : m_floatingWindows) {
            if (fw->isVisible()) {
                if (QWindow *window = fw->windowHandle()) {
                    window->setProperty("kddockwidgets_qwidget", QVariant::fromValue<QWidgetOrQuick *>(fw)); // Since QWidgetWindow is private API
                    windows << window;
                } else {
                    qWarning() << Q_FUNC_INFO << "FloatingWindow doesn't have QWindow";
                }
            }
        }
    }

    for (MainWindowBase *m : m_mainWindows) {
        if (m->isVisible()) {
            if (QWindow *window = m->window()->windowHandle()) {
                window->setProperty("kddockwidgets_qwidget", QVariant::fromValue<QWidgetOrQuick *>(m));
                windows << window;
            } else {
                qWarning() << Q_FUNC_INFO << "MainWindow doesn't have QWindow";
            }
        }
    }

    return windows;
}

void DockRegistry::clear(const QStringList &affinities)
{
    // Clears everything
    clear(m_dockWidgets, m_mainWindows, affinities);
}

void DockRegistry::clear(const DockWidgetBase::List &dockWidgets,
                         const MainWindowBase::List &mainWindows,
                         const QStringList &affinities)
{
    for (auto dw : std::as_const(dockWidgets)) {
        if (affinities.isEmpty() || affinitiesMatch(affinities, dw->affinities())) {
            dw->forceClose();
            dw->d->lastPositions().removePlaceholders();
        }
    }

    for (auto mw : std::as_const(mainWindows)) {
        if (affinities.isEmpty() || affinitiesMatch(affinities, mw->affinities())) {
            mw->multiSplitter()->clearLayout();
        }
    }
}

void DockRegistry::ensureAllFloatingWidgetsAreMorphed()
{
    for (DockWidgetBase *dw : std::as_const(m_dockWidgets)) {
        if (dw->window() == dw && dw->isVisible())
            dw->d->morphIntoFloatingWindow();
    }
}

bool DockRegistry::eventFilter(QObject *watched, QEvent *event)
{
    if (event->type() == QEvent::Quit && !m_isProcessingAppQuitEvent) {
        m_isProcessingAppQuitEvent = true;
        qApp->sendEvent(qApp, event);
        m_isProcessingAppQuitEvent = false;
        return true;
    } else if (event->type() == QEvent::Expose) {
        if (auto windowHandle = qobject_cast<QWindow *>(watched)) {
            if (FloatingWindow *fw = floatingWindowForHandle(windowHandle)) {
                // This floating window was exposed
                m_floatingWindows.removeOne(fw);
                m_floatingWindows.append(fw);
            }
        }
    } else if (event->type() == QEvent::MouseButtonPress) {
        // When clicking on a MDI Frame we raise the window
        if (Frame *f = parentFrame(watched)) {
            if (f->isMDI())
                f->raise();
        }

        // The following code is for hididng the overlay
        if (!(Config::self().flags() & Config::Flag_AutoHideSupport))
            return false;

        if (qobject_cast<Frame *>(watched)) {
            // break recursion
            return false;
        }

        auto p = watched;
        while (p) {
            if (auto dw = qobject_cast<DockWidgetBase *>(p))
                return onDockWidgetPressed(dw, static_cast<QMouseEvent *>(event));

            if (auto layoutWidget = qobject_cast<LayoutWidget *>(p)) {
                if (auto mw = layoutWidget->mainWindow()) {
                    // The user clicked somewhere in the main window's drop area, but outside of the
                    // overlayed dock widget
                    mw->clearSideBarOverlay();
                    return false;
                }
            }

            p = p->parent();
        }
    }

    return false;
}

bool DockRegistry::onDockWidgetPressed(DockWidgetBase *dw, QMouseEvent *ev)
{
    // Here we implement "auto-hide". If there's a overlayed dock widget, we hide it if some other
    // dock widget is clicked.

#ifdef KDDOCKWIDGETS_QTWIDGETS
    // Don't be sending mouse events around if a popup is open, they are sensitive
    if (qApp->activePopupWidget())
        return false;
#endif

    MainWindowBase *mainWindow = dw->mainWindow();
    if (!mainWindow) // Only docked widgets are interesting
        return false;

    if (DockWidgetBase *overlayedDockWidget = mainWindow->overlayedDockWidget()) {
        ev->ignore();
        qApp->sendEvent(overlayedDockWidget->d->frame(), ev);

        if (ev->isAccepted()) {
            // The Frame accepted it. It means the user is resizing it. We allow for 4px outside for better resize.
            return true; // don't propagate the event further
        }
        if (dw != overlayedDockWidget) {
            // User clicked outside if the overlay, then we close the overlay.
            mainWindow->clearSideBarOverlay();
            return false;
        }
    }

    return false;
}
