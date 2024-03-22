/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_MULTISPLITTER_SEPARATOR_P_H
#define KD_MULTISPLITTER_SEPARATOR_P_H

#include "kddockwidgets/docks_export.h"

#include <QObject>
#include <QPoint>

namespace Layouting {

class Config;
class ItemBoxContainer;
class Separator;
class Widget;

class DOCKS_EXPORT Separator
{
public:
    typedef QVector<Separator *> List;

    virtual ~Separator();

    bool isVertical() const;
    void move(int p);
    Qt::Orientation orientation() const;
    void setGeometry(int pos, int pos2, int length);
    void setGeometry(QRect r);
    int position() const;
    QObject *host() const;

    void init(Layouting::ItemBoxContainer *, Qt::Orientation orientation);

    ItemBoxContainer *parentContainer() const;

    ///@brief Returns whether we're dragging a separator. Can be useful for the app to stop other work while we're not in the final size
    static bool isResizing();
    virtual Widget *asWidget() = 0;

    /// @internal Just for the unit-tests.
    /// Returns the total amount of Separator() instances currently alive.
    static int numSeparators();

protected:
    explicit Separator(Widget *hostWidget);
    virtual Widget *createRubberBand(Widget *parent)
    {
        Q_UNUSED(parent);
        return nullptr;
    }
    void onMousePress();
    void onMouseReleased();
    void onMouseDoubleClick();
    void onMouseMove(QPoint pos);

private:
    friend class Config;

    Q_DISABLE_COPY(Separator)
    void setLazyPosition(int);
    bool isBeingDragged() const;
    bool usesLazyResize() const;
    static bool s_isResizing;
    static Separator *s_separatorBeingDragged;
    struct Private;
    Private *const d;
};

}

#endif
