/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#pragma once

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/KDDockWidgets.h"

#include <QObject>
#include <QVector>
#include <QRect>
#include <QVariant>
#include <QDebug>

#include <memory>

class TestMultiSplitter;

namespace Layouting {
Q_NAMESPACE

class ItemContainer;
class ItemBoxContainer;
class Item;
class Separator;
class Widget;
struct LengthOnSide;

enum Side
{
    Side1,
    Side2
};
Q_ENUM_NS(Side)

enum class GrowthStrategy
{
    BothSidesEqually,
    Side1Only,
    Side2Only
};

enum class SeparatorOption
{
    None = 0,
    LazyResize
};
Q_DECLARE_FLAGS(SeparatorOptions, SeparatorOption)

enum class ChildrenResizeStrategy
{
    Percentage, ///< Resizes the container in a way that all children will keep occupying the same percentage
    Side1SeparatorMove, ///< When resizing a container, it takes/adds space from Side1 children first
    Side2SeparatorMove ///< When resizing a container, it takes/adds space from Side2 children first
};
Q_ENUM_NS(ChildrenResizeStrategy)

enum class NeighbourSqueezeStrategy
{
    AllNeighbours, ///< The squeeze is spread between all neighbours, not just immediate ones first
    ImmediateNeighboursFirst ///< The first neighbour takes as much squeeze as it can, only then the next neighbour is squezed, and so forth
};
Q_ENUM_NS(NeighbourSqueezeStrategy)

enum LayoutBorderLocation
{
    LayoutBorderLocation_None = 0,
    LayoutBorderLocation_North = 1,
    LayoutBorderLocation_East = 2,
    LayoutBorderLocation_West = 4,
    LayoutBorderLocation_South = 8,
    LayoutBorderLocation_All = LayoutBorderLocation_North | LayoutBorderLocation_East | LayoutBorderLocation_West | LayoutBorderLocation_South,
    LayoutBorderLocation_Verticals = LayoutBorderLocation_West | LayoutBorderLocation_East,
    LayoutBorderLocation_Horizontals = LayoutBorderLocation_North | LayoutBorderLocation_South,
};
Q_DECLARE_FLAGS(LayoutBorderLocations, LayoutBorderLocation)

inline int pos(QPoint p, Qt::Orientation o)
{
    return o == Qt::Vertical ? p.y()
                             : p.x();
}

inline int length(QSize sz, Qt::Orientation o)
{
    return o == Qt::Vertical ? sz.height()
                             : sz.width();
}

inline QVariantMap sizeToMap(QSize sz)
{
    QVariantMap map;
    map.insert(QStringLiteral("width"), sz.width());
    map.insert(QStringLiteral("height"), sz.height());

    return map;
}

inline QVariantMap rectToMap(QRect rect)
{
    QVariantMap map;
    map.insert(QStringLiteral("x"), rect.x());
    map.insert(QStringLiteral("y"), rect.y());
    map.insert(QStringLiteral("width"), rect.width());
    map.insert(QStringLiteral("height"), rect.height());

    return map;
}

inline QSize mapToSize(const QVariantMap &map)
{
    return { map.value(QStringLiteral("width")).toInt(),
             map.value(QStringLiteral("height")).toInt() };
}

inline QRect mapToRect(const QVariantMap &map)
{
    return QRect(map.value(QStringLiteral("x")).toInt(),
                 map.value(QStringLiteral("y")).toInt(),
                 map.value(QStringLiteral("width")).toInt(),
                 map.value(QStringLiteral("height")).toInt());
}

struct SizingInfo
{

    SizingInfo();

    QSize size() const
    {
        return geometry.size();
    }

    void setSize(QSize sz)
    {
        geometry.setSize(sz);
    }

    int length(Qt::Orientation o) const
    {
        return Layouting::length(size(), o);
    }

    int minLength(Qt::Orientation o) const
    {
        return Layouting::length(minSize, o);
    }

    int maxLengthHint(Qt::Orientation o) const
    {
        return qMax(minLength(o), Layouting::length(maxSizeHint, o));
    }

    int availableLength(Qt::Orientation o) const
    {
        return qMax(0, length(o) - minLength(o));
    }

    int missingLength(Qt::Orientation o) const
    {
        return qMax(0, minLength(o) - length(o));
    }

    QPoint pos() const
    {
        return geometry.topLeft();
    }

    int position(Qt::Orientation o) const
    {
        return Layouting::pos(pos(), o);
    }

    int edge(Qt::Orientation o) const
    {
        return o == Qt::Vertical ? geometry.bottom()
                                 : geometry.right();
    }

    void setLength(int l, Qt::Orientation o)
    {
        if (o == Qt::Vertical) {
            geometry.setHeight(l);
        } else {
            geometry.setWidth(l);
        }
    }

    void incrementLength(int byAmount, Qt::Orientation o)
    {
        setLength(length(o) + byAmount, o);
    }

    void setOppositeLength(int l, Qt::Orientation o);

    void setPos(int p, Qt::Orientation o)
    {
        if (o == Qt::Vertical)
            geometry.moveTop(p);
        else
            geometry.moveLeft(p);
    }

    bool isNull() const
    {
        return geometry.isNull();
    }

    void setGeometry(QRect geo)
    {
        geometry = geo;
    }

    int availableToGrow(Qt::Orientation o) const
    {
        return maxLengthHint(o) - length(o);
    }

    int neededToShrink(Qt::Orientation o) const
    {
        return qMax(0, length(o) - maxLengthHint(o));
    }

    bool isPastMax(Qt::Orientation o) const
    {
        return availableToGrow(o) >= 0;
    }

    QVariantMap toVariantMap() const;
    void fromVariantMap(const QVariantMap &);

    typedef QVector<SizingInfo> List;
    QRect geometry;
    QSize minSize;
    QSize maxSizeHint;
    double percentageWithinParent = 0.0;
    bool isBeingInserted = false;
};

class DOCKS_EXPORT_FOR_UNIT_TESTS Item : public QObject
{
    Q_OBJECT
    Q_PROPERTY(int x READ x NOTIFY xChanged)
    Q_PROPERTY(int y READ y NOTIFY yChanged)
    Q_PROPERTY(int width READ width NOTIFY widthChanged)
    Q_PROPERTY(int height READ height NOTIFY heightChanged)
    Q_PROPERTY(QRect geometry READ geometry NOTIFY geometryChanged)
    Q_PROPERTY(bool isContainer READ isContainer CONSTANT)
public:
    typedef QVector<Item *> List;

    explicit Item(Widget *hostWidget, ItemContainer *parent = nullptr);
    ~Item() override;

    /// @brief returns whether this item is a root container
    bool isRoot() const;

    ///@brief Returns whether the item is touching the layout's borders.
    ///Returns Location_None if it's not touching a border.
    LayoutBorderLocations adjacentLayoutBorders() const;

    virtual int visibleCount_recursive() const;

    /**
     * @brief No widget can have a minimum size smaller than this, regardless of their minimum size.
     */
    static QSize hardcodedMinimumSize;
    static QSize hardcodedMaximumSize;
    static int separatorThickness;

    int x() const;
    int y() const;
    int width() const;
    int height() const;
    QSize size() const;
    void setSize(QSize);
    QPoint pos() const;
    int pos(Qt::Orientation) const;
    QRect geometry() const;
    QRect rect() const;
    bool isContainer() const;
    ItemContainer *parentContainer() const;
    ItemBoxContainer *parentBoxContainer() const;
    void setMinSize(QSize);
    void setMaxSizeHint(QSize);
    bool isPlaceholder() const;
    void setGeometry(QRect rect);
    ItemBoxContainer *root() const;
    QRect mapToRoot(QRect) const;
    QPoint mapToRoot(QPoint) const;
    int mapToRoot(int p, Qt::Orientation) const;
    QPoint mapFromRoot(QPoint) const;
    QRect mapFromRoot(QRect) const;
    QPoint mapFromParent(QPoint) const;
    int mapFromRoot(int p, Qt::Orientation) const;

    QObject *guestAsQObject() const;
    Widget *guestWidget() const
    {
        return m_guest;
    }
    void setGuestWidget(Widget *);

    void ref();
    void unref();
    int refCount() const;

    int minLength(Qt::Orientation) const;
    int maxLengthHint(Qt::Orientation) const;

    QObject *host() const;
    Widget *hostWidget() const;
    void restore(Widget *guestWidget);

    QVector<int> pathFromRoot() const;

    Q_REQUIRED_RESULT virtual bool checkSanity();

    bool isMDI() const;

    virtual QSize minSize() const;
    virtual QSize maxSizeHint() const;
    virtual void setSize_recursive(QSize newSize, ChildrenResizeStrategy strategy = ChildrenResizeStrategy::Percentage);
    virtual bool isVisible(bool excludeBeingInserted = false) const;
    virtual void setGeometry_recursive(QRect rect);
    virtual void dumpLayout(int level = 0);
    virtual void setHostWidget(Widget *);
    virtual QVariantMap toVariantMap() const;
    virtual void fillFromVariantMap(const QVariantMap &map, const QHash<QString, Widget *> &widgets);

    static Item *createFromVariantMap(Widget *hostWidget, ItemContainer *parent,
                                      const QVariantMap &map, const QHash<QString, Widget *> &widgets);

Q_SIGNALS:
    void geometryChanged();
    void xChanged();
    void yChanged();
    void widthChanged();
    void heightChanged();
    void visibleChanged(Layouting::Item *thisItem, bool visible);
    void minSizeChanged(Layouting::Item *thisItem);
    void maxSizeChanged(Layouting::Item *thisItem);

protected:
    friend class ::TestMultiSplitter;
    explicit Item(bool isContainer, Widget *hostWidget, ItemContainer *parent);
    void setParentContainer(ItemContainer *parent);
    void connectParent(ItemContainer *parent);
    void setPos(QPoint);
    void setPos(int pos, Qt::Orientation);
    const ItemContainer *asContainer() const;
    ItemContainer *asContainer();
    ItemBoxContainer *asBoxContainer();
    void setLength(int length, Qt::Orientation);
    virtual void setLength_recursive(int length, Qt::Orientation);
    int length(Qt::Orientation) const;
    int availableLength(Qt::Orientation) const;
    QSize missingSize() const;
    virtual void updateWidgetGeometries();
    virtual void setIsVisible(bool);
    bool isBeingInserted() const;
    void setBeingInserted(bool);

    SizingInfo m_sizingInfo;
    const bool m_isContainer;
    ItemContainer *m_parent = nullptr;
    bool m_isSettingGuest = false;
private Q_SLOTS:
    void onWidgetLayoutRequested();

private:
    friend class ItemContainer;
    friend class ItemBoxContainer;
    friend class ItemFreeContainer;
    void turnIntoPlaceholder();
    bool eventFilter(QObject *o, QEvent *event) override;
    int m_refCount = 0;
    void updateObjectName();
    void onWidgetDestroyed();
    bool m_isVisible = false;
    Widget *m_hostWidget = nullptr;
    Widget *m_guest = nullptr;
};

/// @brief And Item which can contain other Items
class DOCKS_EXPORT_FOR_UNIT_TESTS ItemContainer : public Item
{
    Q_OBJECT
public:
    explicit ItemContainer(Widget *hostWidget, ItemContainer *parent);
    explicit ItemContainer(Widget *hostWidget);
    ~ItemContainer();

    virtual void removeItem(Item *, bool hardRemove = true) = 0;
    virtual void restore(Item *child) = 0;
    virtual void onChildMinSizeChanged(Item *child) = 0;
    virtual void onChildVisibleChanged(Item *child, bool visible) = 0;

    int numVisibleChildren() const;
    int numChildren() const;
    bool hasChildren() const;
    bool hasVisibleChildren(bool excludeBeingInserted = false) const;
    const List childItems() const;
    bool isEmpty() const;
    bool contains(const Item *item) const;
    Item *itemForObject(const QObject *) const;
    Item *itemForWidget(const Widget *w) const;
    Item::List visibleChildren(bool includeBeingInserted = false) const;
    Item::List items_recursive() const;
    bool contains_recursive(const Item *item) const;
    int visibleCount_recursive() const override;
    int count_recursive() const;
    virtual void clear() = 0;

protected:
    bool hasSingleVisibleItem() const;

    Item::List m_children;

Q_SIGNALS:
    void itemsChanged();
    void numVisibleItemsChanged(int);
    void numItemsChanged();

private:
    struct Private;
    Private *const d;
};

/// @brief A container for items which can either be vertical or horizontal
///
/// Similar analogy to QBoxLayout
class DOCKS_EXPORT_FOR_UNIT_TESTS ItemBoxContainer : public ItemContainer
{
    Q_OBJECT
public:
    explicit ItemBoxContainer(Widget *hostWidget, ItemContainer *parent);
    explicit ItemBoxContainer(Widget *hostWidget);
    ~ItemBoxContainer();
    void insertItem(Item *item, int index, KDDockWidgets::InitialOption option = KDDockWidgets::DefaultSizeMode::Fair);
    void insertItem(Item *item, KDDockWidgets::Location, KDDockWidgets::InitialOption = {});

    static void insertItemRelativeTo(Item *item, Item *relativeTo, KDDockWidgets::Location,
                                     KDDockWidgets::InitialOption = KDDockWidgets::DefaultSizeMode::Fair);

    void requestSeparatorMove(Separator *separator, int delta);
    int minPosForSeparator(Separator *, bool honourMax = true) const;
    int maxPosForSeparator(Separator *, bool honourMax = true) const;
    int minPosForSeparator_global(Separator *, bool honourMax = true) const;
    int maxPosForSeparator_global(Separator *, bool honourMax = true) const;
    void requestEqualSize(Separator *separator);
    void layoutEqually();
    void layoutEqually_recursive();
    void removeItem(Item *, bool hardRemove = true) override;
    QSize minSize() const override;
    QSize maxSizeHint() const override;
    QSize availableSize() const;
    Q_REQUIRED_RESULT bool checkSanity() override;
    void dumpLayout(int level = 0) override;
    void setSize_recursive(QSize newSize, ChildrenResizeStrategy strategy = ChildrenResizeStrategy::Percentage) override;
    QRect suggestedDropRect(const Item *item, const Item *relativeTo, KDDockWidgets::Location) const;
    QVariantMap toVariantMap() const override;
    void fillFromVariantMap(const QVariantMap &map, const QHash<QString, Widget *> &widgets) override;
    void clear() override;
    Qt::Orientation orientation() const;
    bool isVertical() const;
    bool isHorizontal() const;
    int length() const;

private:
    bool hasOrientation() const;
    int indexOfVisibleChild(const Item *) const;
    void restore(Item *) override;
    void restoreChild(Item *, NeighbourSqueezeStrategy neighbourSqueezeStrategy = NeighbourSqueezeStrategy::AllNeighbours);

    void setGeometry_recursive(QRect rect) override;

    ItemBoxContainer *convertChildToContainer(Item *leaf);
    bool hasOrientationFor(KDDockWidgets::Location) const;
    int usableLength() const;
    void setChildren(const Item::List &children, Qt::Orientation o);
    void setOrientation(Qt::Orientation);
    void updateChildPercentages();
    void updateChildPercentages_recursive();
    void updateWidgetGeometries() override;
    int oppositeLength() const;

    void layoutEqually(SizingInfo::List &sizes);

    ///@brief Grows the side1Neighbour to the right and the side2Neighbour to the left
    ///So they occupy the empty space that's between them (or bottom/top if Qt::Vertical).
    ///This is useful when an Item is removed. Its neighbours will occupy its space.
    ///side1Neighbour or side2Neighbour are allowed to be null, in which case the non-null one
    ///will occupy the entire space.
    void growNeighbours(Item *side1Neighbour, Item *side2Neighbour);

    ///@brief grows an item by @p amount. It calculates how much to grow on side1 and on side2
    ///Then calls growItem(item, side1Growth, side2Growth) which will effectively grow it,
    ///and shrink the neighbours which are donating the size.
    void growItem(Item *, int amount, GrowthStrategy,
                  NeighbourSqueezeStrategy neighbourSqueezeStrategy,
                  bool accountForNewSeparator = false,
                  ChildrenResizeStrategy = ChildrenResizeStrategy::Percentage);
    void growItem(int index, SizingInfo::List &sizes, int missing, GrowthStrategy,
                  NeighbourSqueezeStrategy neighbourSqueezeStrategy,
                  bool accountForNewSeparator = false);

    ///@brief Shrinks the neighbours of the item at @p index
    ///
    /// The neighbours at the left/top of the item, will be shrunk by @p side1Amount, while the items
    /// at right/bottom will be shrunk by @p side2Amount.
    /// Squeezes all the neighbours (not just the immediate ones).
    void shrinkNeighbours(int index, SizingInfo::List &sizes, int side1Amount, int side2Amount,
                          NeighbourSqueezeStrategy = NeighbourSqueezeStrategy::AllNeighbours);

    Item *visibleNeighbourFor(const Item *item, Side side) const;
    int availableLength() const;
    LengthOnSide lengthOnSide(const SizingInfo::List &sizes, int fromIndex, Side, Qt::Orientation) const;
    int neighboursLengthFor(const Item *item, Side, Qt::Orientation) const;
    int neighboursLengthFor_recursive(const Item *item, Side, Qt::Orientation) const;
    int neighboursMinLengthFor(const Item *item, Side, Qt::Orientation) const;
    int neighboursMaxLengthFor(const Item *item, Side, Qt::Orientation) const;
    int availableToSqueezeOnSide(const Item *child, Side) const;
    int availableToGrowOnSide(const Item *child, Side) const;
    int availableToSqueezeOnSide_recursive(const Item *child, Side, Qt::Orientation) const;
    int availableToGrowOnSide_recursive(const Item *child, Side, Qt::Orientation) const;
    void onChildMinSizeChanged(Item *child) override;
    void onChildVisibleChanged(Item *child, bool visible) override;
    void updateSizeConstraints();
    SizingInfo::List sizes(bool ignoreBeingInserted = false) const;
    QVector<int> calculateSqueezes(SizingInfo::List::ConstIterator begin,
                                   SizingInfo::List::ConstIterator end, int needed,
                                   NeighbourSqueezeStrategy, bool reversed = false) const;
    QRect suggestedDropRectFallback(const Item *item, const Item *relativeTo, KDDockWidgets::Location) const;
    void positionItems();
    void positionItems_recursive();
    void positionItems(SizingInfo::List &sizes);
    Item *itemAt(QPoint p) const;
    Item *itemAt_recursive(QPoint p) const;
    void setHostWidget(Widget *) override;
    void setIsVisible(bool) override;
    bool isVisible(bool excludeBeingInserted = false) const override;
    void setLength_recursive(int length, Qt::Orientation) override;
    void applyGeometries(const SizingInfo::List &sizes, ChildrenResizeStrategy = ChildrenResizeStrategy::Percentage);
    void applyPositions(const SizingInfo::List &sizes);

    int indexOf(Separator *) const;
    bool isInSimplify() const;

#ifdef DOCKS_DEVELOPER_MODE
    bool test_suggestedRect();
#endif

public:
    QVector<Layouting::Separator *> separators_recursive() const;
    QVector<Layouting::Separator *> separators() const;

private:
    void simplify();
    static bool s_inhibitSimplify;
    friend class Layouting::Item;
    friend class ::TestMultiSplitter;
    struct Private;
    Private *const d;
};

}
