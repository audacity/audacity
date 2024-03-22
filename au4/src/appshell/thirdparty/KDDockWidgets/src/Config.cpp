/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Application wide config to tune certain beahviours of the framework.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "Config.h"
#include "private/multisplitter/MultiSplitterConfig.h"
#include "private/multisplitter/Widget.h"
#include "private/DockRegistry_p.h"
#include "private/Utils_p.h"
#include "private/DragController_p.h"
#include "FrameworkWidgetFactory.h"

#include <QDebug>
#include <QOperatingSystemVersion>

#ifdef KDDOCKWIDGETS_QTQUICK
#include "private/quick/Helpers_p.h"
#include <QQmlEngine>
#include <QQmlContext>
#endif

namespace KDDockWidgets {

class Config::Private
{
public:
    Private()
        : m_frameworkWidgetFactory(new DefaultWidgetFactory())
    {
    }

    ~Private()
    {
        delete m_frameworkWidgetFactory;
    }

    void fixFlags();

    QQmlEngine *m_qmlEngine = nullptr;
    DockWidgetFactoryFunc m_dockWidgetFactoryFunc = nullptr;
    MainWindowFactoryFunc m_mainWindowFactoryFunc = nullptr;
    TabbingAllowedFunc m_tabbingAllowedFunc = nullptr;
    FrameworkWidgetFactory *m_frameworkWidgetFactory = nullptr;
    Flags m_flags = Flag_Default;
    InternalFlags m_internalFlags = InternalFlag_None;
    CustomizableWidgets m_disabledPaintEvents = CustomizableWidget_None;
    qreal m_draggedWindowOpacity = Q_QNAN;
    int m_mdiPopupThreshold = 250;
    bool m_dropIndicatorsInhibited = false;
#ifdef KDDOCKWIDGETS_QTQUICK
    QtQuickHelpers m_qquickHelpers;
#endif
};

Config::Config()
    : d(new Private())
{
    d->fixFlags();

    // stuff in multisplitter/ can't include the framework widget factory, so set it here
    auto separatorCreator = [](Layouting::Widget *parent) {
        return Config::self().frameworkWidgetFactory()->createSeparator(parent);
    };

    Layouting::Config::self().setSeparatorFactoryFunc(separatorCreator);
}

Config &Config::self()
{
    static Config config;
    return config;
}

Config::~Config()
{
    delete d;
}

Config::Flags Config::flags() const
{
    return d->m_flags;
}

void Config::setFlags(Flags f)
{
    auto dr = DockRegistry::self();
    if (!dr->isEmpty(/*excludeBeingDeleted=*/true)) {
        qWarning() << Q_FUNC_INFO << "Only use this function at startup before creating any DockWidget or MainWindow"
                   << "; These are already created: " << dr->mainWindowsNames()
                   << dr->dockWidgetNames() << dr->floatingWindows();
        return;
    }

    d->m_flags = f;
    d->fixFlags();

    auto multisplitterFlags = Layouting::Config::self().flags();
    multisplitterFlags.setFlag(Layouting::Config::Flag::LazyResize, d->m_flags & Flag_LazyResize);
    Layouting::Config::self().setFlags(multisplitterFlags);
}

void Config::setDockWidgetFactoryFunc(DockWidgetFactoryFunc func)
{
    d->m_dockWidgetFactoryFunc = func;
}

DockWidgetFactoryFunc Config::dockWidgetFactoryFunc() const
{
    return d->m_dockWidgetFactoryFunc;
}

void Config::setMainWindowFactoryFunc(MainWindowFactoryFunc func)
{
    d->m_mainWindowFactoryFunc = func;
}

MainWindowFactoryFunc Config::mainWindowFactoryFunc() const
{
    return d->m_mainWindowFactoryFunc;
}

void Config::setFrameworkWidgetFactory(FrameworkWidgetFactory *wf)
{
    Q_ASSERT(wf);
    delete d->m_frameworkWidgetFactory;
    d->m_frameworkWidgetFactory = wf;
}

FrameworkWidgetFactory *Config::frameworkWidgetFactory() const
{
    return d->m_frameworkWidgetFactory;
}

int Config::separatorThickness() const
{
    return Layouting::Config::self().separatorThickness();
}

void Config::setSeparatorThickness(int value)
{
    if (!DockRegistry::self()->isEmpty(/*excludeBeingDeleted=*/true)) {
        qWarning() << Q_FUNC_INFO << "Only use this function at startup before creating any DockWidget or MainWindow";
        return;
    }

    Layouting::Config::self().setSeparatorThickness(value);
}

void Config::setDraggedWindowOpacity(qreal opacity)
{
    d->m_draggedWindowOpacity = opacity;
}

qreal Config::draggedWindowOpacity() const
{
    return d->m_draggedWindowOpacity;
}

void Config::setTabbingAllowedFunc(TabbingAllowedFunc func)
{
    d->m_tabbingAllowedFunc = func;
}

TabbingAllowedFunc Config::tabbingAllowedFunc() const
{
    return d->m_tabbingAllowedFunc;
}

void Config::setAbsoluteWidgetMinSize(QSize size)
{
    if (!DockRegistry::self()->isEmpty(/*excludeBeingDeleted=*/false)) {
        qWarning() << Q_FUNC_INFO << "Only use this function at startup before creating any DockWidget or MainWindow";
        return;
    }

    Layouting::Item::hardcodedMinimumSize = size;
}

QSize Config::absoluteWidgetMinSize() const
{
    return Layouting::Item::hardcodedMinimumSize;
}

void Config::setAbsoluteWidgetMaxSize(QSize size)
{
    if (!DockRegistry::self()->isEmpty(/*excludeBeingDeleted=*/false)) {
        qWarning() << Q_FUNC_INFO << "Only use this function at startup before creating any DockWidget or MainWindow";
        return;
    }

    Layouting::Item::hardcodedMaximumSize = size;
}

QSize Config::absoluteWidgetMaxSize() const
{
    return Layouting::Item::hardcodedMaximumSize;
}

Config::InternalFlags Config::internalFlags() const
{
    return d->m_internalFlags;
}

void Config::setInternalFlags(InternalFlags flags)
{
    d->m_internalFlags = flags;
}

#ifdef KDDOCKWIDGETS_QTQUICK
void Config::setQmlEngine(QQmlEngine *qmlEngine)
{
    if (d->m_qmlEngine) {
        qWarning() << Q_FUNC_INFO << "Already has QML engine";
        return;
    }

    if (!qmlEngine) {
        qWarning() << Q_FUNC_INFO << "Null QML engine";
        return;
    }

    auto dr = DockRegistry::self(); // make sure our QML types are registered
    QQmlContext *context = qmlEngine->rootContext();
    context->setContextProperty(QStringLiteral("_kddwHelpers"), &d->m_qquickHelpers);
    context->setContextProperty(QStringLiteral("_kddwDockRegistry"), dr);
    context->setContextProperty(QStringLiteral("_kddwDragController"), DragController::instance());
    context->setContextProperty(QStringLiteral("_kddw_widgetFactory"), d->m_frameworkWidgetFactory);

    d->m_qmlEngine = qmlEngine;
}

QQmlEngine *Config::qmlEngine() const
{
    if (!d->m_qmlEngine)
        qWarning() << "Please call KDDockWidgets::Config::self()->setQmlEngine(engine)";

    return d->m_qmlEngine;
}
#endif

void Config::Private::fixFlags()
{
#if defined(Q_OS_WIN)
    if (QOperatingSystemVersion::current().majorVersion() < 10) {
        // Aero-snap requires Windows 10
        m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
    } else {
        // Unconditional now
        m_flags |= Flag_AeroSnapWithClientDecos;
    }

    // These are mutually exclusive:
    if ((m_flags & Flag_AeroSnapWithClientDecos) && (m_flags & Flag_NativeTitleBar)) {
        // We're either using native or client decorations, let's use native.
        m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
    }
#elif defined(Q_OS_MACOS)
    // Not supported on macOS:
    m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
#else
    if (KDDockWidgets::isWayland()) {
        // Native title bar is forced on Wayland. Needed for moving the window.
        // The inner KDDW title bar is used for DnD.
        m_flags |= Flag_NativeTitleBar;
    } else {
        // Not supported on linux/X11
        // On Linux, dragging the title bar of a window doesn't generate NonClientMouseEvents
        // at least with KWin anyway. We can make this more granular and allow it for other
        // X11 window managers
        m_flags = m_flags & ~Flag_NativeTitleBar;
        m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
    }
#endif

#if (!defined(Q_OS_WIN) && !defined(Q_OS_MACOS))
    // QtQuick doesn't support AeroSnap yet. Some problem with the native events not being received...
    m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
#endif


#if defined(DOCKS_DEVELOPER_MODE)
    // We allow to disable aero-snap during development
    if (m_internalFlags & InternalFlag_NoAeroSnap) {
        // The only way to disable AeroSnap
        m_flags = m_flags & ~Flag_AeroSnapWithClientDecos;
    }
#endif

    if (m_flags & Flag_DontUseUtilityFloatingWindows) {
        m_internalFlags |= InternalFlag_DontUseParentForFloatingWindows;
        m_internalFlags |= InternalFlag_DontUseQtToolWindowsForFloatingWindows;
    }

    if (m_flags & Flag_ShowButtonsOnTabBarIfTitleBarHidden) {
        // Flag_ShowButtonsOnTabBarIfTitleBarHidden doesn't make sense if used alone
        m_flags |= Flag_HideTitleBarWhenTabsVisible;
    }
}

void Config::setDisabledPaintEvents(CustomizableWidgets widgets)
{
    d->m_disabledPaintEvents = widgets;
}

Config::CustomizableWidgets Config::disabledPaintEvents() const
{
    return d->m_disabledPaintEvents;
}

void Config::setMDIPopupThreshold(int threshold)
{
    d->m_mdiPopupThreshold = threshold;
}

int Config::mdiPopupThreshold() const
{
    return d->m_mdiPopupThreshold;
}

void Config::setDropIndicatorsInhibited(bool inhibit) const
{
    if (d->m_dropIndicatorsInhibited != inhibit) {
        d->m_dropIndicatorsInhibited = inhibit;
        Q_EMIT DockRegistry::self()->dropIndicatorsInhibitedChanged(inhibit);
    }
}

bool Config::dropIndicatorsInhibited() const
{
    return d->m_dropIndicatorsInhibited;
}

}
