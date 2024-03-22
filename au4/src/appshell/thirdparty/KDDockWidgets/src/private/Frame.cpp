/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief A DockWidget wrapper that adds a QTabWidget and a TitleBar.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "Frame_p.h"
#include "Config.h"
#include "DockRegistry_p.h"
#include "DockWidgetBase_p.h"
#include "FloatingWindow_p.h"
#include "FrameworkWidgetFactory.h"
#include "LayoutSaver_p.h"
#include "LayoutWidget_p.h"
#include "Logging_p.h"
#include "Position_p.h"
#include "TabWidget_p.h"
#include "TitleBar_p.h"
#include "Utils_p.h"
#include "WidgetResizeHandler_p.h"
#include "MDILayoutWidget_p.h"
#include "DropAreaWithCentralFrame_p.h"

#include <QCloseEvent>
#include <QTimer>

#define MARGIN_THRESHOLD 100

static int s_dbg_numFrames = 0;

using namespace KDDockWidgets;

namespace KDDockWidgets {
static FrameOptions actualOptions(FrameOptions options)
{
    if (Config::self().flags() & Config::Flag_AlwaysShowTabs)
        options |= FrameOption_AlwaysShowsTabs;

    return options;
}
}

Frame::Frame(QWidgetOrQuick *parent, FrameOptions options, int userType)
    : LayoutGuestWidget(parent)
    , FocusScope(this)
    , m_tabWidget(Config::self().frameworkWidgetFactory()->createTabWidget(this))
    , m_titleBar(Config::self().frameworkWidgetFactory()->createTitleBar(this))
    , m_options(actualOptions(options))
    , m_userType(userType)
{
    s_dbg_numFrames++;
    DockRegistry::self()->registerFrame(this);

    connect(this, &Frame::currentDockWidgetChanged, this, &Frame::updateTitleAndIcon);

    connect(m_tabWidget->asWidget(), SIGNAL(currentTabChanged(int)), // clazy:exclude=old-style-connect
            this, SLOT(onCurrentTabChanged(int)));

    setLayoutWidget(qobject_cast<LayoutWidget *>(QWidgetAdapter::parentWidget()));
    m_inCtor = false;
}

Frame::~Frame()
{
    m_inDtor = true;
    s_dbg_numFrames--;
    if (m_layoutItem)
        m_layoutItem->unref();

    delete m_resizeHandler;
    m_resizeHandler = nullptr;

    DockRegistry::self()->unregisterFrame(this);

    // Run some disconnects() too, so we don't receive signals during destruction:
    setLayoutWidget(nullptr);
}

void Frame::updateTitleAndIcon()
{
    if (DockWidgetBase *dw = currentDockWidget()) {
        m_titleBar->setTitle(dw->title());
        m_titleBar->setIcon(dw->icon());

        if (auto fw = floatingWindow()) {
            if (fw->hasSingleFrame()) {
                fw->updateTitleAndIcon();
            }
        }

        setObjectName(dw->uniqueName());

    } else if (currentTabIndex() != -1) {
        qWarning() << Q_FUNC_INFO << "Invalid dock widget for frame." << currentTabIndex();
    }
}

void Frame::onDockWidgetTitleChanged()
{
    updateTitleAndIcon();

    if (!m_inCtor) { // don't call pure virtual in ctor
        if (auto dw = qobject_cast<DockWidgetBase *>(sender())) {
            int index = indexOfDockWidget(dw);
            renameTab(index, dw->title());
            changeTabIcon(index, dw->icon(DockWidgetBase::IconPlace::TabBar));
        }
    }
}

void Frame::addWidget(DockWidgetBase *dockWidget, InitialOption addingOption)
{
    insertWidget(dockWidget, dockWidgetCount(), addingOption); // append
}

void Frame::addWidget(Frame *frame, InitialOption addingOption)
{
    if (frame->isEmpty()) {
        qWarning() << "Frame::addWidget: frame is empty." << frame;
        return;
    }

    const auto &docks = frame->dockWidgets();
    for (DockWidgetBase *dockWidget : docks)
        addWidget(dockWidget, addingOption);
}

void Frame::addWidget(FloatingWindow *floatingWindow, InitialOption addingOption)
{
    Q_ASSERT(floatingWindow);
    for (Frame *f : floatingWindow->frames())
        addWidget(f, addingOption);
}

void Frame::insertWidget(DockWidgetBase *dockWidget, int index, InitialOption addingOption)
{
    Q_ASSERT(dockWidget);
    if (containsDockWidget(dockWidget)) {
        if (!dockWidget->isPersistentCentralDockWidget())
            qWarning() << "Frame::addWidget dockWidget already exists. this=" << this << "; dockWidget=" << dockWidget;
        return;
    }
    if (m_layoutItem)
        dockWidget->d->addPlaceholderItem(m_layoutItem);

    insertDockWidget(dockWidget, index);

    if (addingOption.startsHidden()) {
        dockWidget->close(); // Ensure closed
    } else {
        if (hasSingleDockWidget()) {
            Q_EMIT currentDockWidgetChanged(dockWidget);
            setObjectName(dockWidget->uniqueName());

            if (!m_layoutItem) {
                // When adding the 1st dock widget of a fresh frame, let's give the frame the size
                // of the dock widget, so that when adding it to the main window, the main window can
                // use that size as the initial suggested size.
                resize(dockWidget->size());
            }
        }
    }

    connect(dockWidget, &DockWidgetBase::titleChanged, this, &Frame::onDockWidgetTitleChanged);
    connect(dockWidget, &DockWidgetBase::iconChanged, this, &Frame::onDockWidgetTitleChanged);
}

void Frame::removeWidget(DockWidgetBase *dw)
{
    disconnect(dw, &DockWidgetBase::titleChanged, this, &Frame::onDockWidgetTitleChanged);
    disconnect(dw, &DockWidgetBase::iconChanged, this, &Frame::onDockWidgetTitleChanged);
    removeWidget_impl(dw);
}

FloatingWindow *Frame::detachTab(DockWidgetBase *dockWidget)
{
    if (m_inCtor || m_inDtor)
        return nullptr;

    dockWidget->d->saveTabIndex();

    QRect r = dockWidget->geometry();
    removeWidget(dockWidget);

    auto newFrame = Config::self().frameworkWidgetFactory()->createFrame();
    const QPoint globalPoint = mapToGlobal(QPoint(0, 0));
    newFrame->addWidget(dockWidget);

    // We're potentially already dead at this point, as frames with 0 tabs auto-destruct. Don't access members from this point.

    auto floatingWindow = Config::self().frameworkWidgetFactory()->createFloatingWindow(newFrame);
    r.moveTopLeft(globalPoint);
    floatingWindow->setSuggestedGeometry(r, SuggestedGeometryHint_GeometryIsFromDocked);
    floatingWindow->show();

    return floatingWindow;
}

int Frame::indexOfDockWidget(const DockWidgetBase *dw)
{
    if (m_inCtor || m_inDtor)
        return -1;

    return indexOfDockWidget_impl(dw);
}

int Frame::currentIndex() const
{
    if (m_inCtor || m_inDtor)
        return -1;

    return currentIndex_impl();
}

void Frame::setCurrentTabIndex(int index)
{
    if (m_inCtor || m_inDtor)
        return;

    setCurrentTabIndex_impl(index);
}

void Frame::setCurrentDockWidget(DockWidgetBase *dw)
{
    if (m_inCtor || m_inDtor)
        return;

    setCurrentDockWidget_impl(dw);
}

void Frame::insertDockWidget(DockWidgetBase *dw, int index)
{
    if (m_inCtor || m_inDtor)
        return;

    insertDockWidget_impl(dw, index);
}

DockWidgetBase *Frame::dockWidgetAt(int index) const
{
    if (m_inCtor || m_inDtor)
        return nullptr;

    return dockWidgetAt_impl(index);
}

DockWidgetBase *Frame::currentDockWidget() const
{
    if (m_inCtor || m_inDtor)
        return nullptr;

    return currentDockWidget_impl();
}

int Frame::dockWidgetCount() const
{
    if (m_inCtor || m_inDtor)
        return 0;

    return m_tabWidget->numDockWidgets();
}

void Frame::onDockWidgetCountChanged()
{
    qCDebug(docking) << "Frame::onDockWidgetCountChanged:" << this << "; widgetCount=" << dockWidgetCount();
    if (isEmpty() && !isCentralFrame()) {
        scheduleDeleteLater();
    } else {
        updateTitleBarVisibility();

        // We don't really keep track of the state, so emit even if the visibility didn't change. No biggie.
        if (!(m_options & FrameOption_AlwaysShowsTabs))
            Q_EMIT hasTabsVisibleChanged();

        const DockWidgetBase::List docks = dockWidgets();
        for (DockWidgetBase *dock : docks)
            dock->d->updateFloatAction();
    }

    Q_EMIT numDockWidgetsChanged();
}

void Frame::onCurrentTabChanged(int index)
{
    if (index != -1) {
        if (auto dock = dockWidgetAt(index)) {
            Q_EMIT currentDockWidgetChanged(dock);
        } else {
            qWarning() << "dockWidgetAt" << index << "returned nullptr" << this;
        }
    }
}

void Frame::isFocusedChangedCallback()
{
    Q_EMIT isFocusedChanged();
}

void Frame::focusedWidgetChangedCallback()
{
    Q_EMIT focusedWidgetChanged();
}

void Frame::updateTitleBarVisibility()
{
    if (m_updatingTitleBar || m_beingDeleted) {
        // To break a cyclic dependency
        return;
    }

    QScopedValueRollback<bool> guard(m_updatingTitleBar, true);

    bool visible = false;
    if (isCentralFrame()) {
        visible = false;
    } else if ((Config::self().flags() & Config::Flag_HideTitleBarWhenTabsVisible) && hasTabsVisible()) {
        visible = false;
    } else if (FloatingWindow *fw = floatingWindow()) {
        // If there's nested frames then show each Frame's title bar
        visible = !fw->hasSingleFrame();
    } else {
        visible = true;
    }

    const bool wasVisible = m_titleBar->isVisible();
    m_titleBar->setVisible(visible);

    if (wasVisible != visible) {
        Q_EMIT actualTitleBarChanged();
        for (auto dw : dockWidgets())
            Q_EMIT dw->actualTitleBarChanged();
    }

    if (auto fw = floatingWindow()) {
        // Update the floating window which might be using Flag_HideTitleBarWhenTabsVisible
        // In that case it might not show title bar depending on the number of tabs that the frame has
        fw->updateTitleBarVisibility();
    }
}

void Frame::updateFloatingActions()
{
    const QVector<DockWidgetBase *> widgets = dockWidgets();
    for (DockWidgetBase *dw : widgets)
        dw->d->updateFloatAction();
}

bool Frame::containsMouse(QPoint globalPos) const
{
    return QWidgetAdapter::rect().contains(KDDockWidgets::QWidgetAdapter::mapFromGlobal(globalPos));
}

TitleBar *Frame::titleBar() const
{
    return m_titleBar;
}

TitleBar *Frame::actualTitleBar() const
{
    if (FloatingWindow *fw = floatingWindow()) {
        // If there's nested frames then show each Frame's title bar
        if (fw->hasSingleFrame())
            return fw->titleBar();
    }

    return titleBar();
}

QString Frame::title() const
{
    return m_titleBar->title();
}

QIcon Frame::icon() const
{
    return m_titleBar->icon();
}

const DockWidgetBase::List Frame::dockWidgets() const
{
    if (m_inCtor || m_inDtor)
        return {};

    DockWidgetBase::List dockWidgets;
    const int count = dockWidgetCount();
    dockWidgets.reserve(count);
    for (int i = 0; i < count; ++i)
        dockWidgets << dockWidgetAt(i);

    return dockWidgets;
}

bool Frame::containsDockWidget(DockWidgetBase *dockWidget) const
{
    const int count = dockWidgetCount();
    for (int i = 0, e = count; i != e; ++i) {
        if (dockWidget == dockWidgetAt(i))
            return true;
    }
    return false;
}

FloatingWindow *Frame::floatingWindow() const
{
    // Returns the first FloatingWindow* parent in the hierarchy.
    // However, if there's a MainWindow in the hierarchy it stops, which can
    // happen with nested main windows.

    auto p = QWidgetAdapter::parentWidget();
    while (p) {
        if (qobject_cast<KDDockWidgets::MainWindowBase *>(p))
            return nullptr;

        if (auto fw = qobject_cast<FloatingWindow *>(p))
            return fw;

        if (p == window()) {
            // We stop at the window. (top-levels can have parent, but we're not interested)
            return nullptr;
        }

        p = p->parentWidget();
    }

    return nullptr;
}

void Frame::restoreToPreviousPosition()
{
    if (hasSingleDockWidget()) {
        qWarning() << Q_FUNC_INFO << "Invalid usage, there's no tabs";
        return;
    }

    if (!m_layoutItem) {
        qCDebug(placeholder) << Q_FUNC_INFO << "There's no previous position known";
        return;
    }

    if (!m_layoutItem->isPlaceholder()) {
        // Maybe in this case just fold the frame into the placeholder, which probably has other dockwidgets which were added meanwhile. TODO
        qCDebug(placeholder) << Q_FUNC_INFO << "Previous position isn't a placeholder";
        return;
    }

    m_layoutItem->restore(this);
}

int Frame::currentTabIndex() const
{
    return currentIndex();
}

void Frame::onCloseEvent(QCloseEvent *e)
{
    qCDebug(closing) << "Frame::closeEvent";
    e->accept(); // Accepted by default (will close unless ignored)
    const DockWidgetBase::List docks = dockWidgets();
    for (DockWidgetBase *dock : docks) {
        qApp->sendEvent(dock, e);
        if (!e->isAccepted())
            break; // Stop when the first dockwidget prevents closing
    }
}

bool Frame::anyNonClosable() const
{
    for (auto dw : dockWidgets()) {
        if ((dw->options() & DockWidgetBase::Option_NotClosable) && !DockRegistry::self()->isProcessingAppQuitEvent())
            return true;
    }

    return false;
}

bool Frame::anyNonDockable() const
{
    for (auto dw : dockWidgets()) {
        if (dw->options() & DockWidgetBase::Option_NotDockable)
            return true;
    }

    return false;
}

void Frame::onDockWidgetShown(DockWidgetBase *w)
{
    if (hasSingleDockWidget() && containsDockWidget(w)) { // We have to call contains because it might be being in process of being reparented
        if (!QWidgetAdapter::isVisible()) {
            qCDebug(hiding) << "Widget" << w << " was shown, we're="
                            << "; visible="
                            << QWidgetAdapter::isVisible();
            QWidgetAdapter::setVisible(true);
        }
    }
}

void Frame::onDockWidgetHidden(DockWidgetBase *w)
{
    if (!isCentralFrame() && hasSingleDockWidget() && containsDockWidget(w)) { // We have to call contains because it might be being in process of being reparented
        if (QWidgetAdapter::isVisible()) {
            qCDebug(hiding) << "Widget" << w << " was hidden, we're="
                            << "; visible=" << QWidgetAdapter::isVisible()
                            << "; dockWidgets=" << dockWidgets();
            QWidgetAdapter::setVisible(false);
        }
    }
}

void Frame::setLayoutItem(Layouting::Item *item)
{
    if (item == m_layoutItem)
        return;

    if (m_layoutItem)
        m_layoutItem->unref();

    if (item)
        item->ref();

    m_layoutItem = item;
    if (item) {
        for (DockWidgetBase *dw : dockWidgets())
            dw->d->addPlaceholderItem(item);
    } else {
        for (DockWidgetBase *dw : dockWidgets())
            dw->d->lastPositions().removePlaceholders();
    }
}

Layouting::Item *Frame::layoutItem() const
{
    return m_layoutItem;
}

int Frame::dbg_numFrames()
{
    return s_dbg_numFrames;
}

bool Frame::beingDeletedLater() const
{
    return m_beingDeleted;
}

bool Frame::hasTabsVisible() const
{
    if (m_beingDeleted)
        return false;

    return alwaysShowsTabs() || dockWidgetCount() > 1;
}

QStringList Frame::affinities() const
{
    if (isEmpty()) {
        return {};
    } else {
        return dockWidgetAt(0)->affinities();
    }
}

void Frame::setLayoutWidget(LayoutWidget *dt)
{
    if (dt == m_layoutWidget)
        return;

    const bool wasInMainWindow = dt && isInMainWindow();
    const bool wasMDI = isMDI();
    if (m_layoutWidget)
        disconnect(m_visibleWidgetCountChangedConnection);

    m_layoutWidget = dt;
    delete m_resizeHandler;
    m_resizeHandler = nullptr;

    if (m_layoutWidget) {
        if (isMDI())
            m_resizeHandler = new WidgetResizeHandler(/*topLevel=*/false, this);

        // We keep the connect result so we don't dereference m_layoutWidget at shutdown
        m_visibleWidgetCountChangedConnection =
            connect(m_layoutWidget, &LayoutWidget::visibleWidgetCountChanged, this,
                    &Frame::updateTitleBarVisibility);
        updateTitleBarVisibility();
        if (wasInMainWindow != isInMainWindow())
            Q_EMIT isInMainWindowChanged();
    }

    if (wasMDI != isMDI())
        Q_EMIT isMDIChanged();
}

bool Frame::isTheOnlyFrame() const
{
    return m_layoutWidget && m_layoutWidget->visibleCount() == 1;
}

bool Frame::isOverlayed() const
{
    return m_options & FrameOption_IsOverlayed;
}

void Frame::unoverlay()
{
    m_options &= ~FrameOption_IsOverlayed;
}

bool Frame::isFloating() const
{
    if (isInMainWindow())
        return false;

    return isTheOnlyFrame();
}

bool Frame::isInFloatingWindow() const
{
    return floatingWindow() != nullptr;
}

bool Frame::isInMainWindow() const
{
    return mainWindow() != nullptr;
}

bool Frame::event(QEvent *e)
{
    if (e->type() == QEvent::ParentChange) {
        if (auto layoutWidget = qobject_cast<LayoutWidget *>(QWidgetAdapter::parentWidget())) {
            setLayoutWidget(layoutWidget);
        } else {
            setLayoutWidget(nullptr);
        }
    }

    return QWidgetAdapter::event(e);
}

Frame *Frame::deserialize(const LayoutSaver::Frame &f)
{
    if (!f.isValid())
        return nullptr;

    const FrameOptions options = FrameOptions(f.options);
    Frame *frame = nullptr;
    const bool isPersistentCentralFrame = options & FrameOption::FrameOption_IsCentralFrame;
    auto widgetFactory = Config::self().frameworkWidgetFactory();

    if (isPersistentCentralFrame) {
        // Don't create a new Frame if we're restoring the Persistent Central frame (the one created
        // by MainWindowOption_HasCentralFrame). It already exists.

        if (f.mainWindowUniqueName.isEmpty()) {
            // Can happen with older serialization formats
            qWarning() << Q_FUNC_INFO << "Frame is the persistent central frame but doesn't have"
                       << "an associated window name";
        } else {
            if (MainWindowBase *mw = DockRegistry::self()->mainWindowByName(f.mainWindowUniqueName)) {
                frame = mw->dropArea()->m_centralFrame;
                if (!frame) {
                    // Doesn't happen...
                    qWarning() << "Main window" << f.mainWindowUniqueName << "doesn't have central frame";
                }
            } else {
                // Doesn't happen...
                qWarning() << Q_FUNC_INFO << "Couldn't find main window"
                           << f.mainWindowUniqueName;
            }
        }
    }

    if (!frame)
        frame = widgetFactory->createFrame(/*parent=*/nullptr, options);

    frame->setObjectName(f.objectName);

    for (const auto &savedDock : std::as_const(f.dockWidgets)) {
        if (DockWidgetBase *dw = DockWidgetBase::deserialize(savedDock)) {
            frame->addWidget(dw);
        }
    }

    frame->setCurrentTabIndex(f.currentTabIndex);
    frame->QWidgetAdapter::setGeometry(f.geometry);

    return frame;
}

LayoutSaver::Frame Frame::serialize() const
{
    LayoutSaver::Frame frame;
    frame.isNull = false;

    const DockWidgetBase::List docks = dockWidgets();

    frame.objectName = objectName();
    frame.geometry = QWidgetAdapter::geometry();
    frame.options = options();
    frame.currentTabIndex = currentTabIndex();
    frame.id = id(); // for coorelation purposes

    if (MainWindowBase *mw = mainWindow())
        frame.mainWindowUniqueName = mw->uniqueName();

    for (DockWidgetBase *dock : docks)
        frame.dockWidgets.push_back(dock->d->serialize());

    return frame;
}

void Frame::scheduleDeleteLater()
{
    qCDebug(creation) << Q_FUNC_INFO << this;
    m_beingDeleted = true;
    QTimer::singleShot(0, this, [this] {
        // Can't use deleteLater() here due to QTBUG-83030 (deleteLater() never delivered if triggered by a sendEvent() before event loop starts)
        delete this;
    });
}

QSize Frame::dockWidgetsMinSize() const
{
    QSize size = Layouting::Item::hardcodedMinimumSize;
    for (DockWidgetBase *dw : dockWidgets())
        size = size.expandedTo(Layouting::Widget::widgetMinSize(dw));

    return size;
}

QSize Frame::biggestDockWidgetMaxSize() const
{
    QSize size = Layouting::Item::hardcodedMaximumSize;
    for (DockWidgetBase *dw : dockWidgets()) {
        const QSize dwMax = widgetMaxSize(dw);
        if (size == Layouting::Item::hardcodedMaximumSize) {
            size = dwMax;
            continue;
        }

        const bool hasMaxSize = dwMax != Layouting::Item::hardcodedMaximumSize;
        if (hasMaxSize)
            size = dw->maximumSize().expandedTo(size);
    }

    // Interpret 0 max-size as not having one too.
    if (size.width() == 0)
        size.setWidth(Layouting::Item::hardcodedMaximumSize.width());
    if (size.height() == 0)
        size.setHeight(Layouting::Item::hardcodedMaximumSize.height());

    return size;
}

QRect Frame::dragRect() const
{
    QRect rect;
    if (m_titleBar->isVisible()) {
        rect = m_titleBar->rect();
        rect.moveTopLeft(m_titleBar->mapToGlobal(QPoint(0, 0)));
    }

    return rect;
}

MainWindowBase *Frame::mainWindow() const
{
    return m_layoutWidget ? m_layoutWidget->mainWindow() : nullptr;
}

TabWidget *Frame::tabWidget() const
{
    return m_tabWidget;
}

///@brief Returns whether all dock widgets have the specified option set
bool Frame::allDockWidgetsHave(DockWidgetBase::Option option) const
{
    const DockWidgetBase::List docks = dockWidgets();
    return std::all_of(docks.cbegin(), docks.cend(), [option](DockWidgetBase *dw) {
        return dw->options() & option;
    });
}

///@brief Returns whether at least one dock widget has the specified option set
bool Frame::anyDockWidgetsHas(DockWidgetBase::Option option) const
{
    const DockWidgetBase::List docks = dockWidgets();
    return std::any_of(docks.cbegin(), docks.cend(), [option](DockWidgetBase *dw) {
        return dw->options() & option;
    });
}

bool Frame::allDockWidgetsHave(DockWidgetBase::LayoutSaverOption option) const
{
    const DockWidgetBase::List docks = dockWidgets();
    return std::all_of(docks.cbegin(), docks.cend(), [option](DockWidgetBase *dw) {
        return dw->layoutSaverOptions() & option;
    });
}

bool Frame::anyDockWidgetsHas(DockWidgetBase::LayoutSaverOption option) const
{
    const DockWidgetBase::List docks = dockWidgets();
    return std::any_of(docks.cbegin(), docks.cend(), [option](DockWidgetBase *dw) {
        return dw->layoutSaverOptions() & option;
    });
}

void Frame::setAllowedResizeSides(CursorPositions sides)
{
    if (sides) {
        delete m_resizeHandler;
        m_resizeHandler = new WidgetResizeHandler(/*topLevel=*/false, this);
        m_resizeHandler->setAllowedResizeSides(sides);
    } else {
        delete m_resizeHandler;
        m_resizeHandler = nullptr;
    }
}

bool Frame::isMDI() const
{
    return mdiLayoutWidget() != nullptr;
}

MDILayoutWidget *Frame::mdiLayoutWidget() const
{
    return qobject_cast<MDILayoutWidget *>(m_layoutWidget);
}

int Frame::userType() const
{
    return m_userType;
}

WidgetResizeHandler *Frame::resizeHandler() const
{
    return m_resizeHandler;
}
