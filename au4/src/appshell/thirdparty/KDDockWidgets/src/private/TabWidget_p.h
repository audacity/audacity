/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Implements a QTabWidget derived class with support for docking and undocking
 * KDockWidget::DockWidget as tabs .
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_TAB_WIDGET_P_H
#define KD_TAB_WIDGET_P_H

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/DockWidgetBase.h"

#include "Draggable_p.h"
#include "Frame_p.h"

#include <QVector>

#include <memory>

namespace KDDockWidgets {

class DockWidgetBase;
class TabWidget;

///@brief a QTabBar derived class to be used by KDDockWidgets::TabWidget
class DOCKS_EXPORT TabBar : public Draggable
{
public:
    /**
     * @brief Constructs a new TabBar
     * @param parent The parent TabWidget
     */
    explicit TabBar(QWidgetOrQuick *thisWidget, TabWidget *parent = nullptr);

    /**
     * @brief returns the dock widgets at tab number @p index
     * @param index the tab number from which we want the dock widget
     * @return the dock widget at tab number @p index
     */
    DockWidgetBase *dockWidgetAt(int index) const;

    ///@overload
    DockWidgetBase *dockWidgetAt(QPoint localPos) const;

    // Draggable
    std::unique_ptr<WindowBeingDragged> makeWindow() override;
    bool isWindow() const override;

    void onMousePress(QPoint localPos);
    void onMouseDoubleClick(QPoint localPos);

    ///@brief returns whether there's only 1 tab
    bool hasSingleDockWidget() const;

    int numDockWidgets() const;
    virtual int tabAt(QPoint localPos) const = 0;

    /// @brief returns the tab text at the specified index
    virtual QString text(int index) const = 0;

    /**
     * @brief Returns this class as a QWidget (if using QtWidgets) or QQuickItem
     */
    QWidgetOrQuick *asWidget() const;

    /// @brief Returns the rect of the tab at the specified index
    virtual QRect rectForTab(int index) const = 0;

    DockWidgetBase *singleDockWidget() const override;

    /// @reimp
    bool isMDI() const override;

    Frame *frame() const;

    /// Like QTabBar::moveTab(from, to)
    virtual void moveTabTo(int from, int to) = 0;

private:
    TabWidget *const m_tabWidget;
    QPointer<DockWidgetBase> m_lastPressedDockWidget = nullptr;
    QWidgetOrQuick *const m_thisWidget;
};

class DOCKS_EXPORT TabWidget : public Draggable
{
public:
    /**
     * @brief Constructs a new TabWidget, with @p frame as a parent
     */
    explicit TabWidget(QWidgetOrQuick *thisWidget, Frame *frame);

    /**
     * @brief returns the number of dock widgets in this TabWidget
     */
    virtual int numDockWidgets() const = 0;

    /**
     * @brief Removes a dock widget from the TabWidget
     */
    virtual void removeDockWidget(DockWidgetBase *) = 0;

    /**
     * @brief Returns the index of the dock widget, or -1 if it doesn't exist
     */
    virtual int indexOfDockWidget(const DockWidgetBase *) const = 0;

    /**
     * @brief Sets the current dock widget index
     */
    virtual void setCurrentDockWidget(int index) = 0;
    void setCurrentDockWidget(DockWidgetBase *);

    /// @brief Returns the current dock widget
    DockWidgetBase *currentDockWidget() const;

    virtual bool insertDockWidget(int index, DockWidgetBase *, const QIcon &, const QString &title) = 0;

    virtual void setTabBarAutoHide(bool) = 0;

    ///@brief rename's the tab's text
    virtual void renameTab(int index, const QString &) = 0;

    ///@brief change the tab's icon
    virtual void changeTabIcon(int index, const QIcon &) = 0;

    /**
     * @brief Returns the current index
     */
    virtual int currentIndex() const = 0;

    ///@brief appends a dock widget into this TabWidget
    void addDockWidget(DockWidgetBase *);

    /**
     * @brief Returns the dock widget tabbed at index @p index
     */
    virtual DockWidgetBase *dockwidgetAt(int index) const = 0;

    /**
     * @brief inserts @p dockwidget into the TabWidget, at @p index
     * @param dockwidget the dockwidget to insert
     * @param index The index to where to put it
     */
    bool insertDockWidget(DockWidgetBase *dockwidget, int index);

    /**
     * @brief Returns whether dockwidget @p dw is contained in this tab widget
     * Equivalent to indexOf(dw) != -1
     */
    bool contains(DockWidgetBase *dw) const;

    /**
     * @brief Returns the tab bar
     */
    virtual TabBar *tabBar() const = 0;

    /**
     * @brief Returns this class as a QWidget (if using QtWidgets) or QQuickItem
     */
    QWidgetOrQuick *asWidget() const;

    ///@brief getter for the frame
    Frame *frame() const;

    // Draggable interface
    std::unique_ptr<WindowBeingDragged> makeWindow() override;
    DockWidgetBase *singleDockWidget() const override;
    bool isWindow() const override;

    /// @reimp
    bool isMDI() const override;

    //Q_SIGNALS: // Not a OQbject
    virtual void currentTabChanged(int index) = 0;
    virtual void currentDockWidgetChanged(KDDockWidgets::DockWidgetBase *) = 0;
    virtual void countChanged() {};

protected:
    void onTabInserted();
    void onTabRemoved();
    void onCurrentTabChanged(int index);
    bool onMouseDoubleClick(QPoint localPos);

private:
    Frame *const m_frame;
    QWidgetOrQuick *const m_thisWidget;
    Q_DISABLE_COPY(TabWidget)
};
}

#endif
