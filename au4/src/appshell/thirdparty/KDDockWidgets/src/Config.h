/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Application-wide config to tune certain behaviours of the framework.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_DOCKWIDGETS_CONFIG_H
#define KD_DOCKWIDGETS_CONFIG_H

#include "docks_export.h"

#include <qglobal.h>

QT_BEGIN_NAMESPACE
class QQmlEngine;
class QSize;
QT_END_NAMESPACE

namespace KDDockWidgets {

class DockWidgetBase;
class MainWindowBase;
class FrameworkWidgetFactory;

typedef KDDockWidgets::DockWidgetBase *(*DockWidgetFactoryFunc)(const QString &name);
typedef KDDockWidgets::MainWindowBase *(*MainWindowFactoryFunc)(const QString &name);

/// @brief Function to allow the user more granularity to disallow dock widgets to tab together
/// @param source The dock widgets being dragged
/// @param target The dock widgets within an existing docked tab group
/// @return true if the docking is allowed.
/// @sa setTabbingAllowedFunc
typedef bool (*TabbingAllowedFunc)(const QVector<DockWidgetBase *> &source,
                                   const QVector<DockWidgetBase *> &target);

/**
 * @brief Singleton to allow to choose certain behaviours of the framework.
 *
 * The setters should only be used before creating any DockWidget or MainWindow,
 * preferably right after creating the QApplication.
 */
class DOCKS_EXPORT Config
{
public:
    ///@brief returns the singleton Config instance
    static Config &self();

    ///@brief destructor, called at shutdown
    ~Config();

    ///@brief Flag enum to tune certain behaviours, the defaults are Flag_Default
    enum Flag
    {
        Flag_None = 0, ///< No option set
        Flag_NativeTitleBar = 1, ///< Enables the Native OS title bar on OSes that support it (Windows 10, macOS), ignored otherwise.
        Flag_AeroSnapWithClientDecos = 2, ///< Deprecated. This is now default and cannot be turned off. Moving a window on Windows 10 uses native moving, as that works well across screens with different HDPI settings. There's no reason to use manual client/Qt window moving.
        Flag_AlwaysTitleBarWhenFloating = 4, ///< Floating windows will have a title bar even if Flag_HideTitleBarWhenTabsVisible is specified. Unneeded if Flag_HideTitleBarWhenTabsVisible isn't specified, as that's the default already.
        Flag_HideTitleBarWhenTabsVisible = 8, ///< Hides the title bar if there's tabs visible. The empty space in the tab bar becomes draggable.
        Flag_AlwaysShowTabs = 16, ///< Always show tabs, even if there's only one,
        Flag_AllowReorderTabs = 32, ///< Allows user to re-order tabs by dragging them
        Flag_TabsHaveCloseButton = 64, ///< Tabs will have a close button. Equivalent to QTabWidget::setTabsClosable(true).
        Flag_DoubleClickMaximizes = 128, ///< Double clicking the titlebar will maximize a floating window instead of re-docking it
        Flag_TitleBarHasMaximizeButton = 256, ///< The title bar will have a maximize/restore button when floating. This is mutually-exclusive with the floating button (since many apps behave that way).
        Flag_TitleBarIsFocusable = 512, ///< You can click the title bar and it will focus the last focused widget in the focus scope. If no previously focused widget then it focuses the user's dock widget guest, which should accept focus or use a focus proxy.
        Flag_LazyResize = 1024, ///< The dock widgets are resized in a lazy manner. The actual resize only happens when you release the mouse button.
        Flag_DontUseUtilityFloatingWindows = 0x1000,
        Flag_TitleBarHasMinimizeButton = 0x2000 | Flag_DontUseUtilityFloatingWindows, ///< The title bar will have a minimize button when floating. This implies Flag_DontUseUtilityFloatingWindows too, otherwise they wouldn't appear in the task bar.
        Flag_TitleBarNoFloatButton = 0x4000, ///< The TitleBar won't show the float button
        Flag_AutoHideSupport = 0x8000 | Flag_TitleBarNoFloatButton, ///< Supports minimizing dock widgets to the side-bar.
            ///< By default it also turns off the float button, but you can remove Flag_TitleBarNoFloatButton to have both.
        Flag_KeepAboveIfNotUtilityWindow = 0x10000, ///< Only meaningful if Flag_DontUseUtilityFloatingWindows is set. If floating windows are normal windows, you might still want them to keep above and not minimize when you focus the main window.
        Flag_CloseOnlyCurrentTab = 0x20000, ///< The TitleBar's close button will only close the current tab, instead of all of them
        Flag_ShowButtonsOnTabBarIfTitleBarHidden = 0x40000, ///< When using Flag_HideTitleBarWhenTabsVisible the close/float buttons disappear with the title bar. With Flag_ShowButtonsOnTabBarIfHidden they'll be shown in the tab bar.
        Flag_AllowSwitchingTabsViaMenu = 0x80000, ///< Allow switching tabs via a context menu when right clicking on the tab area
        Flag_Default = Flag_AeroSnapWithClientDecos ///< The defaults
    };
    Q_DECLARE_FLAGS(Flags, Flag)

    ///@brief List of customizable widgets
    enum CustomizableWidget
    {
        CustomizableWidget_None = 0, ///< None
        CustomizableWidget_TitleBar, ///< The title bar
        CustomizableWidget_DockWidget, ///< The dock widget
        CustomizableWidget_Frame, ///< The container for a group of 1 or more dockwidgets which are tabbed together
        CustomizableWidget_TabBar, ///< The tab bar, child of Frame, which contains 1 or more dock widgets
        CustomizableWidget_TabWidget, ///< The tab widget which relates to the tab bar
        CustomizableWidget_FloatingWindow, ///< A top-level window. The container for 1 or more Frame nested side by side
        CustomizableWidget_Separator ///< The draggable separator between dock widgets in a layout
    };
    Q_DECLARE_FLAGS(CustomizableWidgets, CustomizableWidget)

    ///@internal
    ///Internal flags for additional tuning.
    ///@warning Not for public consumption, support will be limited.
    enum InternalFlag
    {
        InternalFlag_None = 0, ///< The default
        InternalFlag_NoAeroSnap = 1, ///< Only for development. Disables Aero-snap.
        InternalFlag_DontUseParentForFloatingWindows = 2, ///< FloatingWindows won't have a parent top-level.
        InternalFlag_DontUseQtToolWindowsForFloatingWindows = 4, ///< FloatingWindows will use Qt::Window instead of Qt::Tool.
        InternalFlag_DontShowWhenUnfloatingHiddenWindow = 8, ///< DockWidget::setFloating(false) won't do anything if the window is hidden.
        InternalFlag_UseTransparentFloatingWindow = 16, ///< For QtQuick only. Allows to have round-corners. It's flaky when used with native Windows drop-shadow.
        InternalFlag_DisableTranslucency = 32, ///< KDDW tries to detect if your Window Manager doesn't support transparent windows, but the detection might fail
        /// with more exotic setups. This flag can be used to override.
        InternalFlag_TopLevelIndicatorRubberBand = 64 ///< Makes the rubber band of classic drop indicators to be top-level windows. Helps with working around MFC bugs
    };
    Q_DECLARE_FLAGS(InternalFlags, InternalFlag)

    ///@brief returns the chosen flags
    Flags flags() const;

    ///@brief setter for the flags
    ///@param flags the flags to set
    ///Not all flags are guaranteed to be set, as the OS might not supported them
    ///Call @ref flags() after the setter if you need to know what was really set
    void setFlags(Flags flags);

    /**
     * @brief Registers a DockWidgetFactoryFunc.
     *
     * This is optional, the default is nullptr.
     *
     * A DockWidgetFactoryFunc is a function that receives a dock widget name
     * and returns a DockWidget instance.
     *
     * While restoring, @ref LayoutSaver requires all dock widgets to exist.
     * If a DockWidget doesn't exist then a DockWidgetFactoryFunc function is
     * required, so the layout saver can ask to create the DockWidget and then
     * restore it.
     */
    void setDockWidgetFactoryFunc(DockWidgetFactoryFunc);

    ///@brief Returns the DockWidgetFactoryFunc.
    ///nullptr by default
    DockWidgetFactoryFunc dockWidgetFactoryFunc() const;

    ///@brief counter-part of DockWidgetFactoryFunc but for the main window.
    /// Should be rarely used. It's good practice to have the main window before restoring a layout.
    /// It's here so we can use it in the linter executable
    void setMainWindowFactoryFunc(MainWindowFactoryFunc);

    ///@brief Returns the MainWindowFactoryFunc.
    ///nullptr by default
    MainWindowFactoryFunc mainWindowFactoryFunc() const;

    /**
     * @brief Sets the WidgetFactory.
     *
     * By default DefaultWidgetFactory is used, which gives you FrameWidget, TitleBarWidget,
     * TabBarWidget, TabWidgetWidget etc. You can set your own factory, to supply your own variants
     * of those classes, for the purposes of changing GUI appearance and such.
     *
     * Also potentially useful to return QtQuick classes instead of the QtWidget based ones.
     * Ownership is taken.
     */
    void setFrameworkWidgetFactory(FrameworkWidgetFactory *);

    ///@brief getter for the framework widget factory
    FrameworkWidgetFactory *frameworkWidgetFactory() const;

    /**
     * @brief Returns the thickness of the separator.
     *
     * Default is 5px.
     */
    int separatorThickness() const;

    ///@brief setter for @ref separatorThickness
    ///Note: Only use this function at startup before creating any DockWidget or MainWindow.
    void setSeparatorThickness(int value);

    ///@brief sets the dragged window opacity
    ///1.0 is fully opaque while 0.0 is fully transparent
    void setDraggedWindowOpacity(qreal opacity);

    ///@brief returns the opacity to use when dragging dock widgets
    ///By default it's 1.0, fully opaque
    qreal draggedWindowOpacity() const;

    /// @brief Allows to disable support for drop indicators while dragging
    /// By default drop indicators will be shown when dragging dock widgets.
    /// This functionality can be toggled whenever you need it (it's not a startup-only setting).
    void setDropIndicatorsInhibited(bool inhibit) const;

    /// @brief Returns whether drop indicators are inhibited.
    /// by default this is false unless you call setDropIndicatorsInhibited(true)
    bool dropIndicatorsInhibited() const;

    /**
     * @brief Allows the user to intercept a docking attempt to center (tabbed) and disallow it.
     *
     * Whenever the user tries to tab two widgets together, the framework will call @p func. If
     * it returns true, then tabbing is allowed, otherwise not.
     *
     * Example:
     * @code
     * #include <kddockwidgets/Config.h>
     * (...)
     *
     * auto func = [] (const KDDockWidgets::DockWidgetBase::List &source,
     *                 const KDDockWidgets::DockWidgetBase::List &target)
     * {
     *    // disallows dockFoo to be tabbed with dockBar.
     *    return !(source.contains(dockFoo) && target.contains(dockBar));
     * }
     * @endcode
     * KDDockWidgets::Config::self()->setTabbingAllowedFunc(func);
     */
    void setTabbingAllowedFunc(TabbingAllowedFunc func);

    ///@brief Used internally by the framework. Returns the function which was passed to setTabbingAllowedFunc()
    ///By default it's nullptr.
    ///@sa setTabbingAllowedFunc().
    TabbingAllowedFunc tabbingAllowedFunc() const;

    ///@brief Sets the minimum size a dock widget can have.
    /// Widgets can still provide their own min-size and it will be respected, however it can never be
    /// smaller than this one.
    void setAbsoluteWidgetMinSize(QSize size);
    QSize absoluteWidgetMinSize() const;

    ///@brief Sets the maximum size a dock widget can have.
    /// Widgets can still provide their own max-size and it will be respected, however it can never be
    /// bigger than this one.
    void setAbsoluteWidgetMaxSize(QSize size);
    QSize absoluteWidgetMaxSize() const;

    ///@brief Disables our internal widget's paint events
    /// By default, KDDockWidget's internal widgets reimplement paintEvent(). Disabling them
    /// (which makes the base-class, QWidget::paintEvent() be called instead) can be useful if you want to style
    // via CSS stylesheets.
    void setDisabledPaintEvents(CustomizableWidgets);
    Config::CustomizableWidgets disabledPaintEvents() const;

    ///@internal
    ///@brief returns the internal flags.
    ///@warning Not for public consumption, support will be limited.
    InternalFlags internalFlags() const;

    ///@internal
    ///@brief setter for the internal flags
    ///@warning Not for public consumption, support will be limited.
    void setInternalFlags(InternalFlags flags);

    /// @brief Sets the MDI popup threshold. When the layout is MDI and you drag a dock widget
    /// X pixels behond the window's edge, it will float the dock widget.
    /// by default this value is 250px. Use -1 to disable
    void setMDIPopupThreshold(int);
    int mdiPopupThreshold() const;

#ifdef KDDOCKWIDGETS_QTQUICK
    ///@brief Sets the QQmlEngine to use. Applicable only when using QtQuick.
    void setQmlEngine(QQmlEngine *);
    QQmlEngine *qmlEngine() const;
#endif

private:
    Q_DISABLE_COPY(Config)
    Config();
    class Private;
    Private *const d;
};

}

Q_DECLARE_OPERATORS_FOR_FLAGS(KDDockWidgets::Config::Flags)

#endif
