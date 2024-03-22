/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief File with KDDockWidgets namespace-level enums and methods.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_KDDOCKWIDGETS_H
#define KD_KDDOCKWIDGETS_H

#include "docks_export.h"
#include "Qt5Qt6Compat_p.h"

#include <QObject>
#include <QDebug>

#ifdef Q_OS_WIN
// Only on Windows, where this is popular. On linux the Qt::Tool windows need reparenting. Untested on macOS.
#define KDDOCKWIDGETS_SUPPORTS_NESTED_MAINWINDOWS
#endif

namespace Layouting {
class Item;
class ItemBoxContainer;
}

namespace KDDockWidgets {
DOCKS_EXPORT
Q_NAMESPACE
class MultiSplitter;
class DropArea;

enum Location
{
    Location_None,
    Location_OnLeft, ///> Left docking location
    Location_OnTop, ///> Top docking location
    Location_OnRight, ///> Right docking location
    Location_OnBottom ///> Bottom docking location
};
Q_ENUM_NS(Location)

enum MainWindowOption
{
    MainWindowOption_None = 0, ///> No option set
    MainWindowOption_HasCentralFrame = 1, ///> Makes the MainWindow always have a central frame, for tabbing documents
    MainWindowOption_MDI = 2, ///> The layout will be MDI. DockWidgets can have arbitrary positions, not restricted by any layout
    MainWindowOption_HasCentralWidget = 4 | MainWindowOption_HasCentralFrame, ///> Similar to MainWindowOption_HasCentralFrame but
    ///> you'll have a central widget which can't be detached (Similar to regular QMainWindow).
};
Q_DECLARE_FLAGS(MainWindowOptions, MainWindowOption)
Q_ENUM_NS(MainWindowOptions)

///@internal
///@brief Describes some sizing strategies for the layouting engine.
///This is internal. The public API for dealing with sizing is InitialOption.
///@sa InitialOption
enum class DefaultSizeMode
{
    ItemSize, ///< Simply uses the Item::size() of the item being added. Actual used size might be smaller if our window isn't big enough.
    Fair, ///< Gives an equal relative size as the items that are already in the layout
    FairButFloor, ///< Equal to fair, but if the item we're adding is smaller than the fair suggestion, then that small size is used.
    NoDefaultSizeMode, ///< Don't do any sizing
};
Q_ENUM_NS(DefaultSizeMode)

///@brief Only here for source-compat with v1.2. Do not use.
///Use InitialVisibilityOption instead.
enum AddingOption
{
    AddingOption_None = 0,
    AddingOption_StartHidden
};
Q_ENUM_NS(AddingOption)

enum class InitialVisibilityOption
{
    StartVisible = 0, ///< The dock widget is made visible when docked
    StartHidden ///< Don't show the dock widget when adding it
};
Q_ENUM_NS(InitialVisibilityOption)

/**
     * @brief Struct describing the preferred dock widget size and visibility when adding it to a layout
     *
     * You can pass this to MainWindowBase::addDockWidget() to give an hint of your preferred size
     * and visibility.
     *
     * See below the documentation for InitialOption::visibility and InitialOption::preferredSize.
     *
     * @sa MainWindowBase::addDockWidget()
     */
struct InitialOption
{
    // Implicit ctors for convenience:

    InitialOption() = default;

    InitialOption(InitialVisibilityOption v)
        : visibility(v)
    {
    }

    InitialOption(QSize size)
        : preferredSize(size)
    {
    }

    InitialOption(InitialVisibilityOption v, QSize size)
        : visibility(v)
        , preferredSize(size)
    {
    }

    QT_DEPRECATED_X("AddingOption is deprecated and will be removed in v1.5. Use InitialVisibilityOption instead.")
    InitialOption(AddingOption opt)
        : visibility(opt == AddingOption_StartHidden ? InitialVisibilityOption::StartHidden
                                                     : InitialVisibilityOption::StartVisible)
    {
    }

    bool startsHidden() const
    {
        return visibility == InitialVisibilityOption::StartHidden;
    }

    int preferredLength(Qt::Orientation o) const
    {
        return o == Qt::Horizontal ? preferredSize.width()
                                   : preferredSize.height();
    }

    bool hasPreferredLength(Qt::Orientation o) const
    {
        return preferredLength(o) > 0;
    }

    /**
         * @brief Allows a dock widget to be docked as hidden.
         *
         * Next time you call DockWidget::show() it will be shown at that place. This avoids
         * flickering, as no show()/hide() workarounds are needed.
         */
    InitialVisibilityOption visibility = InitialVisibilityOption::StartVisible;

    /**
         * @brief Allows to control the size a dock widget should get when docked.
         *
         * If an invalid or empty size is passed then KDDW's default heuristics are applied.
         *
         * Note that usually only the width or the height will be honoured: For example, when adding a
         * dock widget to the left then only the preferred width will be taken into account, as the
         * height will simply fill the whole layout.
         */
    QSize preferredSize;

private:
    friend class Layouting::Item;
    friend class Layouting::ItemBoxContainer;
    friend class KDDockWidgets::MultiSplitter;
    friend class KDDockWidgets::DropArea;

    InitialOption(DefaultSizeMode mode)
        : sizeMode(mode)
    {
    }

    DefaultSizeMode sizeMode = DefaultSizeMode::Fair;
};

enum RestoreOption
{
    RestoreOption_None = 0,
    RestoreOption_RelativeToMainWindow = 1, ///< Skips restoring the main window geometry and the restored dock widgets will use relative sizing.
        ///< Loading layouts won't change the main window geometry and just use whatever the user has at the moment.
};
Q_DECLARE_FLAGS(RestoreOptions, RestoreOption)
Q_ENUM_NS(RestoreOptions)

enum class DropIndicatorType
{
    Classic, ///< The default
    Segmented, ///< Segmented indicators
    None ///< Don't show any drop indicators while dragging
};
Q_ENUM_NS(DropIndicatorType)

///@internal
enum SuggestedGeometryHint
{
    SuggestedGeometryHint_None,
    SuggestedGeometryHint_PreserveCenter = 1,
    SuggestedGeometryHint_GeometryIsFromDocked = 2
};
Q_DECLARE_FLAGS(SuggestedGeometryHints, SuggestedGeometryHint)
Q_ENUM_NS(SuggestedGeometryHint)

/// @brief Each main window supports 4 sidebars
enum class SideBarLocation
{
    None,
    North,
    East,
    West,
    South
};

///@brief describes a type of button you can have in the title bar
enum class TitleBarButtonType
{
    Close,
    Float,
    Minimize,
    Maximize,
    Normal, // Restore from maximized state
    AutoHide,
    UnautoHide
};
Q_ENUM_NS(TitleBarButtonType)

///@internal
inline Qt5Qt6Compat::qhashtype qHash(SideBarLocation loc, Qt5Qt6Compat::qhashtype seed)
{
    return ::qHash(static_cast<uint>(loc), seed);
}

///@internal
enum CursorPosition
{
    CursorPosition_Undefined = 0,
    CursorPosition_Left = 1,
    CursorPosition_Right = 2,
    CursorPosition_Top = 4,
    CursorPosition_Bottom = 8,
    CursorPosition_TopLeft = CursorPosition_Top | CursorPosition_Left,
    CursorPosition_TopRight = CursorPosition_Top | CursorPosition_Right,
    CursorPosition_BottomRight = CursorPosition_Bottom | CursorPosition_Right,
    CursorPosition_BottomLeft = CursorPosition_Bottom | CursorPosition_Left,
    CursorPosition_Horizontal = CursorPosition_Right | CursorPosition_Left,
    CursorPosition_Vertical = CursorPosition_Top | CursorPosition_Bottom,
    CursorPosition_All = CursorPosition_Left | CursorPosition_Right | CursorPosition_Top | CursorPosition_Bottom
};
Q_DECLARE_FLAGS(CursorPositions, CursorPosition)
Q_ENUM_NS(CursorPosition)

///@internal
enum FrameOption
{
    FrameOption_None = 0,
    FrameOption_AlwaysShowsTabs = 1,
    FrameOption_IsCentralFrame = 2,
    FrameOption_IsOverlayed = 4,
    FrameOption_NonDockable = 8 ///> You can't DND and tab things into this Frame
};
Q_DECLARE_FLAGS(FrameOptions, FrameOption)
Q_ENUM_NS(FrameOptions)

///@internal
inline QString locationStr(Location loc)
{
    switch (loc) {
    case KDDockWidgets::Location_None:
        return QStringLiteral("none");
    case KDDockWidgets::Location_OnLeft:
        return QStringLiteral("left");
    case KDDockWidgets::Location_OnTop:
        return QStringLiteral("top");
    case KDDockWidgets::Location_OnRight:
        return QStringLiteral("right");
    case KDDockWidgets::Location_OnBottom:
        return QStringLiteral("bottom");
    }

    return QString();
}
}

QT_BEGIN_NAMESPACE
///@internal
inline QDebug operator<<(QDebug d, KDDockWidgets::InitialOption o)
{
    d << o.startsHidden();
    return d;
}
QT_END_NAMESPACE

Q_DECLARE_OPERATORS_FOR_FLAGS(KDDockWidgets::FrameOptions)
Q_DECLARE_METATYPE(KDDockWidgets::InitialVisibilityOption)

#endif
