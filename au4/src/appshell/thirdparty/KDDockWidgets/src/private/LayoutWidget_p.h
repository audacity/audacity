/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief A widget that supports an arbitrary number of splitters (called Separators) in any
 * combination of vertical/horizontal.
 *
 * This is a widget wrapper around the multisplitter layout (Layouting::Item)
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KDDOCKWIDGETS_LAYOUTWIDGET_P_H
#define KDDOCKWIDGETS_LAYOUTWIDGET_P_H

#pragma once

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/KDDockWidgets.h"
#include "kddockwidgets/LayoutSaver.h"
#include "kddockwidgets/QWidgetAdapter.h"

#include <QList>

namespace Layouting {
class Item;
class Separator;
class Widget_qwidget;
}

namespace KDDockWidgets {

class MainWindowBase;
class FloatingWindow;
class Frame;
class DockWidgetBase;

/**
 * @brief The widget (QWidget or QQuickItem) which holds a layout of dock widgets.
 *
 * Usually this would simply be MultiSplitter, but we've introduced this base class to support
 * different layouts, like MDI layouts, which are very different than traditional dock widget
 * layouts.
 *
 * This class makes the bridge between the GUI world (QWidget) and Layouting::Item world.
 * It's suitable to be set as a main window central widget for instance. The actual layouting is
 * then done by the root Item.
 */
class DOCKS_EXPORT LayoutWidget : public LayoutGuestWidget
{
    Q_OBJECT
public:
    explicit LayoutWidget(QWidgetOrQuick *parent = nullptr);
    ~LayoutWidget() override;

    bool isInMainWindow() const;
    MainWindowBase *mainWindow() const;
    FloatingWindow *floatingWindow() const;

    /**
     * @brief returns the layout's minimum size
     * @ref setLayoutMinimumSize
     */
    QSize layoutMinimumSize() const;

    /**
     * @brief returns the layout's maximum size hint
     */
    QSize layoutMaximumSizeHint() const;

    /**
     * @brief returns the contents width.
     * Usually it's the same width as the respective parent MultiSplitter.
     */
    int width() const
    {
        return size().width();
    }

    /**
     * @brief returns the contents height.
     * Usually it's the same height as the respective parent MultiSplitter.
     */
    int height() const
    {
        return size().height();
    }

    /**
     * @brief getter for the size
     */
    QSize size() const;

    /// @brief Runs some sanity checks. Returns true if everything is OK
    bool checkSanity() const;

    /// @brief clears the layout
    void clearLayout();

    /// @brief dumps the layout to stderr
    void dumpLayout() const;

    /**
     * @brief setter for the contents size
     * The "contents size" is just the size() of this layout. However, since resizing
     * QWidgets is async and we need it to be sync. As sometimes adding widgets will increase
     * the MultiSplitter size (due to widget's min-size constraints).
     */
    void setLayoutSize(QSize);


    /// @brief restores the dockwidget @p dw to its previous position
    void restorePlaceholder(DockWidgetBase *dw, Layouting::Item *, int tabIndex);

    /**
     * @brief The list of items in this layout.
     */
    const QVector<Layouting::Item *> items() const;

    /**
     * @brief Returns true if this layout contains the specified item.
     */
    bool containsItem(const Layouting::Item *) const;

    /**
     * @brief  Returns true if this layout contains the specified frame.
     */
    bool containsFrame(const Frame *) const;

    /**
     * @brief Returns the number of Item objects in this layout.
     * This includes non-visible (placeholder) Items too.
     * @sa visibleCount
     */
    int count() const;

    /**
     * @brief Returns the number of visible Items in this layout.
     * Which is @ref count minus @ref placeholderCount
     * @sa count
     */
    int visibleCount() const;

    /**
     * @brief Returns the number of placeholder items in this layout.
     * This is the same as @ref count minus @ref visibleCount
     * @sa count, visibleCount
     */
    int placeholderCount() const;

    /**
     * @brief returns the Item that holds @p frame in this layout
     */
    Layouting::Item *itemForFrame(const Frame *frame) const;

    /**
     * @brief Returns this list of Frame objects contained in this layout
     */
    QList<Frame *> frames() const;

    /// @brief Returns the list of dock widgets contained in this layout
    QVector<DockWidgetBase *> dockWidgets() const;

    /**
     * @brief Removes an item from this MultiSplitter.
     */
    void removeItem(Layouting::Item *item);

    /**
     * @brief Updates the min size of this layout.
     */
    void updateSizeConstraints();

    virtual bool deserialize(const LayoutSaver::MultiSplitter &);
    LayoutSaver::MultiSplitter serialize() const;

protected:
    void setRootItem(Layouting::ItemContainer *root);
    /**
     * @brief setter for the minimum size
     * @ref minimumSize
     */
    void setLayoutMinimumSize(QSize);

    void onLayoutRequest() override;
    bool onResize(QSize newSize) override;

    /**
     * @brief Removes unneeded placeholder items when adding new frames.
     *
     * A floating frame A might have a placeholder in the main window (for example to remember its
     * position on the Left), but then the user might attach it to the right, so the left
     * placeholder is no longer need. Right before adding the frame to the right we remove the left
     * placeholder, otherwise it's unrefed while we're adding causing a segfault. So what this does
     * is making the unrefing happen a bit earlier.
     */
    void unrefOldPlaceholders(const QList<Frame *> &framesBeingAdded) const;

    /**
     * @brief returns the frames contained in @p frameOrMultiSplitter
     * If frameOrMultiSplitter is a Frame, it returns a list of 1 element, with that frame
     * If frameOrMultiSplitter is a MultiSplitter then it returns a list of all frames it contains
     */
    QList<Frame *> framesFrom(QWidgetOrQuick *frameOrMultiSplitter) const;

Q_SIGNALS:
    void visibleWidgetCountChanged(int count);

private:
    bool m_inResizeEvent = false;
    Layouting::ItemContainer *m_rootItem = nullptr;
};

}

#endif
