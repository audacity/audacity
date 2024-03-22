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

#ifndef KDDOCKWIDGETS_MULTISPLITTER_P_H
#define KDDOCKWIDGETS_MULTISPLITTER_P_H

#include "LayoutWidget_p.h"
#include "kddockwidgets/KDDockWidgets.h"
#include "kddockwidgets/QWidgetAdapter.h"
#include "kddockwidgets/docks_export.h"

class TestDocks;

namespace KDDockWidgets {

struct WindowBeingDragged;

/**
 * MultiSplitter is simply a wrapper around Layouting::Item in which the hosted widgets are
 * of class KDDockWidgets::Frame. The stuff in Layouting:: being agnostic and generic, not specific
 * to KDDW.
 *
 * A MultiSplitter is like a QSplitter but supports mixing vertical and horizontal splitters in
 * any combination.
 *
 * It supports adding a widget to the left/top/bottom/right of the whole MultiSplitter or adding
 * relative to a single widget.
 */
class DOCKS_EXPORT MultiSplitter : public LayoutWidget
{
    Q_OBJECT
public:
    explicit MultiSplitter(QWidgetOrQuick *parent = nullptr);
    ~MultiSplitter() override;

    /**
     * @brief Adds a widget to this MultiSplitter.
     */
    void addWidget(QWidgetOrQuick *widget, KDDockWidgets::Location location,
                   Frame *relativeTo = nullptr,
                   InitialOption option = DefaultSizeMode::Fair);

    /**
     * Adds an entire MultiSplitter into this layout. The donor MultiSplitter will be deleted
     * after all its Frames are stolen. All added Frames will preserve their original layout, so,
     * if widgetFoo was at the left of widgetBar when in the donor splitter, then it will still be at left
     * of widgetBar when the whole splitter is dropped into this one.
     */
    void addMultiSplitter(MultiSplitter *splitter, KDDockWidgets::Location location,
                          Frame *relativeTo = nullptr,
                          InitialOption option = DefaultSizeMode::Fair);

    /**
     * Called by the indicators, so they draw the drop rubber band at the correct place.
     * The rect for the rubberband when dropping a widget at the specified location.
     * Excludes the Separator thickness, result is actually smaller than what needed. In other words,
     * the result will be exactly the same as the geometry the widget will get.
     */
    QRect rectForDrop(const WindowBeingDragged *wbd, KDDockWidgets::Location location,
                      const Layouting::Item *relativeTo) const;

    bool deserialize(const LayoutSaver::MultiSplitter &) override;

    ///@brief returns the list of separators
    QVector<Layouting::Separator *> separators() const;

    /// @brief See docs for MainWindowBase::layoutEqually()
    void layoutEqually();

    /// @brief overload that just resizes widgets within a sub-tree
    void layoutEqually(Layouting::ItemBoxContainer *);

private:
    friend class ::TestDocks;

    Layouting::ItemBoxContainer *rootItem() const;

    // For debug/hardening
    bool validateInputs(QWidgetOrQuick *widget, KDDockWidgets::Location location,
                        const Frame *relativeToFrame, InitialOption option) const;


    void setRootItem(Layouting::ItemBoxContainer *);

    /**
     * @brief Like @ref availableLengthForDrop but just returns the total available width or height (depending on @p orientation)
     * So no need to receive any location.
     * @param orientation If Qt::Vertical then returns the available height. Width otherwise.
     */
    int availableLengthForOrientation(Qt::Orientation orientation) const;

    /**
     * @brief Equivalent to @ref availableLengthForOrientation but returns for both orientations.
     * width is for Qt::Vertical.
     */
    QSize availableSize() const;

    Layouting::ItemBoxContainer *m_rootItem = nullptr;
};

}

#endif
