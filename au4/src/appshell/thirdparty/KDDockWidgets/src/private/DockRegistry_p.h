/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DOCKREGISTRY_P_H
#define KD_DOCKREGISTRY_P_H

#include "kddockwidgets/DockWidgetBase.h"
#include "kddockwidgets/MainWindowBase.h"

#include "kddockwidgets/private/Frame_p.h"

#include <QVector>
#include <QObject>
#include <QPointer>

/**
 * DockRegistry is a singleton that knows about all DockWidgets.
 * It's used so we can restore layouts.
 * It's a private implementation detail.
 */
namespace KDDockWidgets {

class FloatingWindow;
class Frame;
class LayoutWidget;
class MainWindowMDI;
class SideBar;
struct WindowBeingDragged;

class DOCKS_EXPORT DockRegistry : public QObject
{
    Q_OBJECT
    Q_PROPERTY(
        KDDockWidgets::Frame *frameInMDIResize READ frameInMDIResize NOTIFY frameInMDIResizeChanged)
public:
    enum class DockByNameFlag
    {
        None = 0,
        ConsultRemapping = 1,
        CreateIfNotFound = 2 ///< Creates the dock widget via the user's widget factory in case it doesn't exist
    };
    Q_DECLARE_FLAGS(DockByNameFlags, DockByNameFlag)

    static DockRegistry *self();
    ~DockRegistry();
    void registerDockWidget(DockWidgetBase *);
    void unregisterDockWidget(DockWidgetBase *);

    void registerMainWindow(MainWindowBase *);
    void unregisterMainWindow(MainWindowBase *);

    void registerFloatingWindow(FloatingWindow *);
    void unregisterFloatingWindow(FloatingWindow *);

    void registerLayout(LayoutWidget *);
    void unregisterLayout(LayoutWidget *);

    void registerFrame(Frame *);
    void unregisterFrame(Frame *);

    Q_INVOKABLE KDDockWidgets::DockWidgetBase *focusedDockWidget() const;

    Q_INVOKABLE bool containsDockWidget(const QString &uniqueName) const;
    Q_INVOKABLE bool containsMainWindow(const QString &uniqueName) const;

    Q_INVOKABLE KDDockWidgets::DockWidgetBase *dockByName(const QString &,
                                                          KDDockWidgets::DockRegistry::DockByNameFlags = {}) const;
    Q_INVOKABLE KDDockWidgets::MainWindowBase *mainWindowByName(const QString &) const;
    Q_INVOKABLE KDDockWidgets::MainWindowMDI *mdiMainWindowByName(const QString &) const;

    /// @brief returns the dock widget that hosts @p guest widget. Nullptr if there's none.
    DockWidgetBase *dockWidgetForGuest(QWidgetOrQuick *guest) const;

    bool isSane() const;

    ///@brief returns all DockWidget instances
    const DockWidgetBase::List dockwidgets() const;

    ///@brief overload returning only the ones with the specified names
    const DockWidgetBase::List dockWidgets(const QStringList &names);

    ///@brief returns all closed DockWidget instances
    const DockWidgetBase::List closedDockwidgets() const;

    ///@brief returns all MainWindow instances
    const MainWindowBase::List mainwindows() const;

    ///@brief overload returning only the ones with the specified names
    const MainWindowBase::List mainWindows(const QStringList &names);

    ///@brief returns the list of LayoutWidget instances
    const QVector<LayoutWidget *> layouts() const;

    ///@brief returns a list of all Frame instances
    const QList<Frame *> frames() const;

    ///@brief returns all FloatingWindow instances. Not necessarily all floating dock widgets,
    /// As there might be DockWidgets which weren't morphed yet.
    const QVector<FloatingWindow *> floatingWindows(bool includeBeingDeleted = false) const;

    ///@brief overload that returns list of QWindow. This is more friendly for supporting both QtWidgets and QtQuick
    const QVector<QWindow *> floatingQWindows() const;

    ///@brief returns whether if there's at least one floating window
    Q_INVOKABLE bool hasFloatingWindows() const;

    ///@brief Returns the window with the specified id
    QWindow *windowForHandle(WId id) const;

    ///@brief returns the FloatingWindow with handle @p windowHandle
    FloatingWindow *floatingWindowForHandle(QWindow *windowHandle) const;

    ///@brief returns the FloatingWindow with handle @p hwnd
    FloatingWindow *floatingWindowForHandle(WId hwnd) const;

    ///@brief returns the MainWindow with handle @p windowHandle
    MainWindowBase *mainWindowForHandle(QWindow *windowHandle) const;

    ///@brief returns the top level widget associated with the specified QWindow.
    ///For QtWidgets, it returns a QWidget which is either a KDDockWidgets::MainWindow or a FloatingWindow.
    ///For QtQuick ir returns the same, but the type is a QWidgetAdapter (a QQuickItem), not QWidget obviously.
    QWidgetOrQuick *topLevelForHandle(QWindow *windowHandle) const;

    ///@brief Returns the list with all visiblye top-level parents of our FloatingWindow and MainWindow instances.
    ///
    /// Typically these are the FloatingWindows and MainWindows themselves. However, since a
    /// MainWindow can be embedded into another widget (for whatever reason, like a QWinWidget),
    /// it means that a top-level can be something else.
    ///
    /// Every returned widget is either a FloatingWindow, MainWindow, or something that contains a MainWindow.
    ///
    /// If @p excludeFloatingDocks is true then FloatingWindow won't be returned
    QVector<QWindow *> topLevels(bool excludeFloatingDocks = false) const;

    /**
     * @brief Closes all dock widgets, and destroys all FloatingWindows
     * This is called before restoring a layout.
     * @param affinities if specified only closes dock widgets and main windows with the specified affinities
     */
    Q_INVOKABLE void clear(const QStringList &affinities = {});

    /**
     * @brief clear Overload that only clears the specified dockWidgets and main windows.
     */
    void clear(const DockWidgetBase::List &dockWidgets,
               const MainWindowBase::List &mainWindows,
               const QStringList &affinities);

    /**
     * @brief Ensures that all floating DockWidgets have a FloatingWindow as a window.
     *
     * This is to simplify things before saving a layout. So we don't have to care about the case
     * where the window is a DockWidget.
     */
    void ensureAllFloatingWidgetsAreMorphed();

    /**
     * @brief returns true if there's 0 dockwidgets, 0 main windows
     *
     * @param excludeBeingDeleted if true, any window currently being deleted won't count
     */
    bool isEmpty(bool excludeBeingDeleted = false) const;

    /**
     * @brief Calls LayoutWidget::checkSanity() on all layouts.
     *
     * @param dumpDebug If true then each layout is dumped too
     *
     * This is called by the unit-tests or the fuzzer. If during this the framework spits a
     * qWarning() then the app will qFatal()
     */
    void checkSanityAll(bool dumpDebug = false);

    /**
     * @brief Returns whether we're processing a QEvent::Quit
     *
     * Used internally to know if we should let Qt close a NonClosable dock widget at shutdown time.
     */
    bool isProcessingAppQuitEvent() const;

    /**
     * @brief Returns all main windows which match at least one of the @p affinities
     */
    MainWindowBase::List mainWindowsWithAffinity(const QStringList &affinities) const;

    // TODO: docs
    LayoutWidget *layoutForItem(const Layouting::Item *) const;

    // TODO: docs
    bool itemIsInMainWindow(const Layouting::Item *) const;


    bool affinitiesMatch(const QStringList &affinities1, const QStringList &affinities2) const;

    /// @brief Returns a list of all known main window unique names
    QStringList mainWindowsNames() const;

    /// @brief Returns a list of all known dock widget unique names
    QStringList dockWidgetNames() const;

    /// @brief returns if the specified window has some other window on top (with higher Z)
    /// This is an approximation, as we don't have ways to compare Z, so we mostly intersect
    /// geometries.
    /// @param target The window which we want to know if it's probably obscured
    /// @param exclude This window should not be counted as an obscurer. (It's being dragged).
    bool isProbablyObscured(QWindow *target, FloatingWindow *exclude) const;

    /// @overload
    bool isProbablyObscured(QWindow *target, WindowBeingDragged *exclude) const;

    ///@brief Returns whether the specified dock widget is in a side bar, and which.
    /// SideBarLocation::None is returned if it's not in a sidebar.
    /// This is only relevant when using the auto-hide and side-bar feature.
    SideBarLocation sideBarLocationForDockWidget(const DockWidgetBase *) const;

    ///@brief Overload that returns the SideBar itself
    SideBar *sideBarForDockWidget(const DockWidgetBase *) const;

    ///@brief Returns the Frame which is being resized in a MDI layout. nullptr if none
    Frame *frameInMDIResize() const;

Q_SIGNALS:
    /// @brief emitted when a main window or a floating window change screen
    void windowChangedScreen(QWindow *);

    /// @brief emitted when the MDI frame that's being resized changed
    void frameInMDIResizeChanged();

    /// @brief emitted whenever Config::dropIndicatorsInhibited changes
    void dropIndicatorsInhibitedChanged(bool inhibited);

protected:
    bool eventFilter(QObject *watched, QEvent *event) override;

private:
    friend class FocusScope;
    explicit DockRegistry(QObject *parent = nullptr);
    bool onDockWidgetPressed(DockWidgetBase *dw, QMouseEvent *);
    void onFocusObjectChanged(QObject *obj);
    void maybeDelete();
    void setFocusedDockWidget(DockWidgetBase *);

    bool m_isProcessingAppQuitEvent = false;
    DockWidgetBase::List m_dockWidgets;
    MainWindowBase::List m_mainWindows;
    QList<Frame *> m_frames;
    QVector<FloatingWindow *> m_floatingWindows;
    QVector<LayoutWidget *> m_layouts;
    QPointer<DockWidgetBase> m_focusedDockWidget;

    ///@brief Dock widget id remapping, used by LayoutSaver
    ///
    /// When LayoutSaver is trying to restore dock widget "foo", but it doesn't exist, it will
    /// attempt to call a user provided factory function. That function can however return a dock
    /// widget with another ID, such as "bar". When that happens this QHash gets a "foo" : "bar"
    /// entry
    mutable QHash<QString, QString> m_dockWidgetIdRemapping;
};

}

#endif
