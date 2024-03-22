/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief The DockWidget base-class that's shared between QtWidgets and QtQuick stack.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_DOCKWIDGET_BASE_H
#define KD_DOCKWIDGET_BASE_H

#include "docks_export.h"
#include "KDDockWidgets.h"
#include "QWidgetAdapter.h"
#include "LayoutSaver.h"

#include <QVector>
#include <memory>

// clazy:excludeall=ctor-missing-parent-argument

class TestDocks;

namespace KDDockWidgets {

class Frame;
class FloatingWindow;
class DragController;
class DockRegistry;
class LayoutSaver;
class TabWidget;
class TitleBar;
class MainWindowBase;
class StateDragging;
class FrameQuick;
class DockWidgetQuick;
class LayoutWidget;

/**
 * @brief The DockWidget base-class. DockWidget and DockWidgetBase are only
 * split in two so we can share some code with the QtQuick implementation,
 * which also derives from DockWidgetBase.
 *
 * Do not use instantiate directly in user code. Use DockWidget instead.
 */
#ifndef PYTHON_BINDINGS //Pyside bug: https://bugreports.qt.io/projects/PYSIDE/issues/PYSIDE-1327
class DOCKS_EXPORT DockWidgetBase : public QWidgetAdapter
#else
class DOCKS_EXPORT DockWidgetBase : public QWidget
#endif
{
    Q_OBJECT
    Q_PROPERTY(bool isFocused READ isFocused NOTIFY isFocusedChanged)
    Q_PROPERTY(bool isFloating READ isFloating WRITE setFloating NOTIFY isFloatingChanged)
    Q_PROPERTY(QString uniqueName READ uniqueName CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(QObject *widget READ widget NOTIFY widgetChanged)
    Q_PROPERTY(KDDockWidgets::DockWidgetBase::Options options READ options WRITE setOptions NOTIFY
                   optionsChanged)
public:
    typedef QVector<DockWidgetBase *> List;

    ///@brief DockWidget options to pass at construction time
    enum Option
    {
        Option_None = 0, ///< No option, the default
        Option_NotClosable = 1, ///< The DockWidget can't be closed on the [x], only programmatically
        Option_NotDockable = 2, ///< The DockWidget can't be docked, it's always floating
        Option_DeleteOnClose = 4 ///< Deletes the DockWidget when closed
    };
    Q_DECLARE_FLAGS(Options, Option)
    Q_ENUM(Options);

    /// @brief Options which will affect LayoutSaver save/restore
    enum class LayoutSaverOption
    {
        None = 0, ///< Just use the defaults
        Skip = 1, ///< The dock widget won't participate in save/restore. Currently only available for floating windows.
    };
    Q_DECLARE_FLAGS(LayoutSaverOptions, LayoutSaverOption)

    enum class IconPlace
    {
        TitleBar = 1,
        TabBar = 2,
        ToggleAction = 4,
        All = ToggleAction | TitleBar | TabBar
    };
    Q_ENUM(IconPlace)
    Q_DECLARE_FLAGS(IconPlaces, IconPlace)

    /**
     * @brief constructs a new DockWidget
     * @param uniqueName the name of the dockwidget, should be unique. Use title for user visible text.
     * @param options the options controlling certain behaviours
     * @param layoutSaverOptions the options to control save/restore
     *
     * There's no parent argument. The DockWidget is either parented to FloatingWindow or MainWindow
     * when visible, or stays without a parent when hidden.
     */
    explicit DockWidgetBase(const QString &uniqueName,
                            Options options = KDDockWidgets::DockWidgetBase::Options(),
                            LayoutSaverOptions layoutSaverOptions = KDDockWidgets::DockWidgetBase::LayoutSaverOptions());

    ///@brief destructor
    ~DockWidgetBase() override;

    /**
     * @brief docks @p other widget into this one. Tabs will be shown if not already.
     * @param other The other dock widget to dock into this one.
     * @param initialOption Allows to specify an InitialOption. Which is useful to add the dock widget
     * as hidden, recording only a placeholder in the tab. So it's restored to tabbed when eventually
     * shown.
     * @sa MainWindow::addDockWidget(), DockWidget::addDockWidgetToContainingWindow()
     */
    Q_INVOKABLE void addDockWidgetAsTab(KDDockWidgets::DockWidgetBase *other,
                                        KDDockWidgets::InitialOption initialOption = {});

    /**
     * @brief docks @p other widget into the window that contains this one.
     *        Equivalent to MainWindow::addDockWidget() with the difference
     *        that it also supports the case where the top-level window is a
     *        FloatingWindow instead of MainWindow.
     *
     * @param other The other dock widget to dock into the window.
     * @param location The location to dock.
     * @param relativeTo The dock widget that the @p location is relative to. If null then the window is considered
     * @param initialOption Allows to specify some extra options that are used while docking.
     * @sa MainWindow::addDockWidget(), DockWidget::addDockWidgetAsTab()
     */
    Q_INVOKABLE void
    addDockWidgetToContainingWindow(KDDockWidgets::DockWidgetBase *other,
                                    KDDockWidgets::Location location,
                                    KDDockWidgets::DockWidgetBase *relativeTo = nullptr,
                                    KDDockWidgets::InitialOption initialOption = {});

    /**
     * @brief sets the widget which this dock widget hosts.
     * @param widget the widget to show inside this dock widget. Must not be null.
     *
     * Ownership for @p widget is transferred to DockWidgetBase.
     * Ownsership for any previously existing widget is transferred back to the user. Meaning if you
     * call setWidget(A) followed by setWidget(B) then A will have to be deleted by you, while B is
     * owned by the dock widget.
     */
    virtual void setWidget(QWidgetOrQuick *widget);

    /**
     * @brief returns the widget which this dock widget hosts
     */
    QWidgetOrQuick *widget() const;

    /**
     * @brief Returns whether the dock widget is floating.
     * Floating means it's not docked and has a window of its own.
     * Note that if you dock a floating dock widget into another floating one
     * then they don't count as floating anymore, as they are
     * side-by-side (or tabbed).
     */
    bool isFloating() const;

    /**
     * @brief setter to make the dock widget float or dock.
     * @param floats If true makes the dock widget float, otherwise docks it.
     *
     * Returns true if the request was accomplished
     */
    bool setFloating(bool floats);

    /**
     * @brief Returns the QAction that allows to hide/show the dock widget
     * Useful to put in menus.
     */
    Q_INVOKABLE QAction *toggleAction() const;

    /**
     * @brief Returns the QAction that allows to dock/undock the dock widget
     * Useful to put in menus.
     */
    Q_INVOKABLE QAction *floatAction() const;

    /**
     * @brief the dock widget's unique name.
     * @internal
     */
    QString uniqueName() const;

    /**
     * @brief Returns the dock widget's title.
     *        This title is visible in title bars and tab bars.
     * @sa setTitle
     */
    QString title() const;

    /**
     * @brief setter for the dock widget's title
     * @param title the dock widget's new title
     * @sa setTitle
     */
    void setTitle(const QString &title);

    /**
     * @brief Returns the size of the dock widget's parent frame.
     *
     * This will always be bigger than the DockWidget's size, as there's margins and a title bar.
     * Also, a frame can contain more than 1 dock widget (tabbed), meaning the geometry will account
     * for the tab bar and title bar.
     *
     * The position of the rect is in layout coordinates. 0,0 is the top-left of the layout
     * holding the widgets.
     */
    QRect frameGeometry() const;

    /**
     * @brief Returns the dock widget's options which control behaviour.
     * @sa setOptions(), optionsChanged()
     */
    Options options() const;

    /// @brief returns the per-dockwidget options which will affect LayoutSaver
    /// These are the options which were passed to the constructor
    KDDockWidgets::DockWidgetBase::LayoutSaverOptions layoutSaverOptions() const;

    /**
     * @brief Setter for the options.
     * Only Option_NotClosable is allowed to change after construction. For the other options use
     * the constructor only.
     *
     * @sa options(), optionsChanged()
     */
    void setOptions(Options);

    /**
     * @brief returns if this dock widget is tabbed into another
     *
     * Technically a docked DockWidget always lives in a tab widget, but from the user's perspective
     * it's not tabbed when there's only 1 dock widget, as there are no tabs displayed. Unless
     * the frame is using Option_AlwaysShowsTabs, in which case this method will return true regardless
     * if being the single one.
     */
    bool isTabbed() const;

    /**
     * @brief Returns true if this dock widget is the current one in the tab
     *        widget that contains it. If the dock widget is alone then true is
     *        returned, as in this case there will also be a tab widget even
     *        though it's invisible.
     */
    bool isCurrentTab() const;

    /**
     * @brief Makes this dock widget current in its tab group.
     */
    Q_INVOKABLE void setAsCurrentTab();


    /**
     * @brief Returns which tab index this dock widget occupies in the tab widget it's contained in
     */
    int tabIndex() const;

    /**
     * @brief Sets an icon to show on title bars and tab bars.
     * @param places Specifies where the icon will be shown (TitleBar, TabBar, ToggleAction, or All)
     *
     * By default there's no icon set.
     *
     * @sa icon()
     */
    void setIcon(const QIcon &icon, IconPlaces places = IconPlace::All);

    /**
     * @brief Returns the dock widget's titlebar, tabbar, or toggle action icon (depending on the passed @p place)
     *
     * By default it's null.
     *
     * @sa setIcon()
     */
    QIcon icon(IconPlace place = IconPlace::TitleBar) const;

    /**
     * @brief Like QWidget::close() but the hosted widget won't be asked if we
     * should close.
     */
    Q_INVOKABLE void forceClose();

    /**
     * @brief Returns this dock widget's title bar.
     *
     * Note that several dock widgets can have the same title bar, in case they are tabbed together.
     * Hidden dock widgets have no associated title bar.
     */
    TitleBar *titleBar() const;

    /**
     * @brief Returns whether this dock widget is open.
     * Equivalent to calling toggleAction().isChecked() or isVisible()
     */
    Q_INVOKABLE bool isOpen() const;

    /**
     * @brief Sets the affinity names. Dock widgets can only dock into dock widgets of the same affinity.
     *
     * By default the affinity is empty and a dock widget can dock into any main window and into any
     * floating window. Usually you won't ever need to call
     * this function, unless you have requirements where certain dock widgets can only dock into
     * certain other dock widgets and main windows. @sa MainWindowBase::setAffinities().
     *
     * Note: Call this function right after creating your dock widget, before adding to a main window and
     * before restoring any layout.
     *
     * Note: Currently you can only call this function once, to keep the code simple and avoid
     * edge cases. This will only be changed if a good use case comes up that requires changing
     * affinities multiple times.
     *
     * @p names the affinity names
     */
    void setAffinities(const QStringList &);

    /// @deprecated @overload
    /// @param name the affinity name
    void setAffinityName(const QString &name);

    /**
     * @brief Returns the affinity name. Empty by default.
     */
    QStringList affinities() const;

    /// @brief Equivalent to QWidget::show(), but it's optimized to reduce flickering on some platforms
    Q_INVOKABLE void show();

    /// @brief Brings the dock widget to the front.
    ///
    /// This means:
    /// - If the dock widget is tabbed with other dock widgets but its tab is not current, it's made current.
    /// - If the dock widget is floating, QWindow::raise() is called.
    ///
    /// This only applies if the dock widget is already open. If closed, does nothing.
    Q_INVOKABLE void raise();

    /**
     * @brief Returns whether widget() is a KDDockWidget::MainWindow
     *
     * This function doesn't have anything to do with this dock widget being inside a main window or
     * not, but rather the inverse concept. It's not a very popular usage, but some applications
     * want to dock a main window into another main window. This is done by putting it into a
     * dock widget first.
     *
     * See also kddockwidgets_example -j
     */
    bool isMainWindow() const;

    /**
     * @brief Returns whether this dock widget is docked into a main window (as opposed to floating)
     *
     * Note that isFloating() returning false might either mean the dock widget is docked into a
     * main window or into a floating window (groupped/nested with other dock widgets. Use this function
     * then if you need to disambiguate
     */
    bool isInMainWindow() const;

    /// @brief Returns the main window this dock widget is in. nullptr if it's not inside a main window
    /// Also returns nullptr if it's minimized to a sidebar
    MainWindowBase *mainWindow() const;

    ///@brief Returns whether This or any child of this dock widget is focused
    ///Not to be confused with QWidget::hasFocus(), which just refers to 1 widget. This includes
    ///variant includes children.
    ///@sa isFocusedChanged()
    bool isFocused() const;

    /**
     * @brief Minimizes this dock widget to the MainWindow's side-bar.
     *
     * It will be undocked from current layout. It's previous docked position will be remembered.
     *
     * This action is only available if the dock widget is docked into a MainWindow.
     * The dockwidget will initially be visible and overlayed on top of the current layout (this is
     * the auto-hide feature).
     */
    Q_INVOKABLE void moveToSideBar();

    /// @brief Returns whether this dock widget is overlayed from the side-bar.
    ///
    /// This is only relevant when using the auto-hide and side-bar feature.
    /// Not to be confused with "floating", which means top-level window.
    bool isOverlayed() const;

    ///@brief Returns whether this dock widget is in a side bar, and which.
    /// SideBarLocation::None is returned if it's not in a sidebar.
    /// This is only relevant when using the auto-hide and side-bar feature.
    /// @sa isInSideBar
    SideBarLocation sideBarLocation() const;

    /// @brief Returns where this dockwidget is in a sidebar
    /// Similar to sideBarLocation(), but returns a bool
    bool isInSideBar() const;

    /// @brief Returns whether this floating dock widget knows its previous docked location
    /// Result only makes sense if it's floating.
    ///
    /// When you call dockWidget->setFloating(false) it will only dock if it knows where to.
    bool hasPreviousDockedLocation() const;

    /// @brief returns the last size the widget has when overlayed
    /// Empty otherwise
    QSize lastOverlayedSize() const;


    /// @brief Returns a dock widget by its name
    /// This is the same name you passed to DockWidget CTOR.
    /// nullptr is returned if the dock widget isn't found.
    static DockWidgetBase *byName(const QString &uniqueName);

    /// @brief Returns whether this widget has the LayoutSaverOption::Skip flag
    bool skipsRestore() const;

    /// @brief If this dock widget is floating, then sets its geometry to @p geo.
    ///
    /// If this dock widget is hidden then it stores the geometry so it can be used the next
    /// time it becomes floating.
    ///
    /// This is just convenience, equivalent to calling window()->setGeometry(rect), with the
    /// added bonus of remembering the requested geometry in case it's still hidden.
    void setFloatingGeometry(QRect geo);

    ///@brief Allows the user to set a type on this dock widget
    ///The type is opaque and will not be interpreted by KDDockWidgets.
    ///This type is passed to FrameWorkWidgetFactory::createTitleBar(), which the user can override
    ///and return different TitleBar subclasses, depending on the type.
    void setUserType(int userType);
    int userType() const;

    /// @brief Sets this dock widgets position to pos within the MDI layout
    /// This only applies if the main window is in MDI mode, which it is not by default
    void setMDIPosition(QPoint pos);
    /// @brief like setMDIPosition(), but for the size.
    void setMDISize(QSize size);
    /// @brief like setMDIPosition(), but for the Z
    /// only implemented for QtQuick
    void setMDIZ(int z);

    ///@brief Returns whether this dock widget is the main window persistent central widget
    ///This only applies when using MainWindowOption_HasCentralWidget
    bool isPersistentCentralDockWidget() const;

Q_SIGNALS:
#ifdef KDDOCKWIDGETS_QTWIDGETS
    ///@brief signal emitted when the parent changed
    /// QtQuick already has QQuickItem::parentChanged(), so add it only for QtWidgets here.
    void parentChanged();
#endif

    ///@brief signal emitted when the DockWidget is shown. As in QEvent::Show.
    void shown();

    ///@brief signal emitted when the DockWidget is hidden. As in QEvent::Hide.
    void hidden();

    ///@brief signal emitted when the icon changed
    void iconChanged();

    ///@brief signal emitted when the title changed
    ///@param title the new title
    void titleChanged(const QString &title);

    ///@brief emitted when the hosted widget changed
    void widgetChanged(KDDockWidgets::QWidgetOrQuick *);

    ///@brief emitted when the options change
    ///@sa setOptions(), options()
    void optionsChanged(KDDockWidgets::DockWidgetBase::Options);

    ///@brief emitted when isFocused changes
    ///@sa isFocused
    void isFocusedChanged(bool);

    ///@brief emitted when isOverlayed changes
    ///@sa isOverlayed
    void isOverlayedChanged(bool);

    ///@brief emitted when isFloating changes
    void isFloatingChanged(bool);

    ///@brief emitted when this dock widget is removed from a side-bar.
    ///Only relevant for the auto-hide/sidebar feature
    void removedFromSideBar();

    ///@brief Emitted when the top-level window this dock widget is in is activated or deactivated
    ///This is convenience to replace tracking dockWidget->window(), since the window changes when
    ///docking and undocking
    ///
    /// It's called 'aboutTo' because it's done in an event filter and the target window doesn't
    /// have it's 'activeWindow' property updated yet at this point.
    void windowActiveAboutToChange(bool activated);

    ///@brief Emitted when the title bar that serves this dock widget changes
    void actualTitleBarChanged();

    /// @brief Emitted when this dock widget is about to be deleted due to Option_DeleteOnClose
    void aboutToDeleteOnClose();

protected:
    void onParentChanged();
    void onShown(bool spontaneous);
    void onHidden(bool spontaneous);

#ifndef PYTHON_BINDINGS //Pyside bug: https://bugreports.qt.io/projects/PYSIDE/issues/PYSIDE-1327
    void onCloseEvent(QCloseEvent *e) override;
    bool onResize(QSize newSize) override;
#endif

#if defined(DOCKS_DEVELOPER_MODE)
public:
#else
private:
#endif
    Q_DISABLE_COPY(DockWidgetBase)
    friend class MultiSplitter;
    friend class LayoutWidget;
    friend class MDILayoutWidget;
    friend class Frame;
    friend class DropArea;
    friend class ::TestDocks;
    friend class StateDragging;
    friend class KDDockWidgets::TabWidget;
    friend class KDDockWidgets::TitleBar;
    friend class KDDockWidgets::DragController;
    friend class KDDockWidgets::DockRegistry;
    friend class KDDockWidgets::LayoutSaver;
    friend class KDDockWidgets::MainWindowBase;
    friend class KDDockWidgets::FrameQuick;
    friend class KDDockWidgets::DockWidgetQuick;

    /**
     * @brief Constructs a dock widget from its serialized form.
     * @internal
     */
    static DockWidgetBase *deserialize(const std::shared_ptr<LayoutSaver::DockWidget> &);


    class Private;
    Private *const d;

    Private *dptr() const;
};

}
Q_DECLARE_METATYPE(KDDockWidgets::Location)

#endif
