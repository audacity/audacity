/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_MULTISPLITTER_SEPARATOR_QUICK_H
#define KD_MULTISPLITTER_SEPARATOR_QUICK_H

#include "kddockwidgets/docks_export.h"
#include "Separator_p.h"
#include "Widget_quick.h"

#include <QQuickItem>

namespace Layouting {

class DOCKS_EXPORT SeparatorQuick
    : public QQuickItem,
      public Layouting::Separator,
      public Layouting::Widget_quick
{
    Q_OBJECT
    Q_PROPERTY(bool isVertical READ isVertical NOTIFY isVerticalChanged)
public:
    explicit SeparatorQuick(Layouting::Widget *parent = nullptr);

    bool isVertical() const;

protected:
    Widget *createRubberBand(Widget *parent) override;
    Widget *asWidget() override;

public:
    // Interface with QML:
    Q_INVOKABLE void onMousePressed();
    Q_INVOKABLE void onMouseMoved(QPointF localPos);
    Q_INVOKABLE void onMouseReleased();
    Q_INVOKABLE void onMouseDoubleClicked();
Q_SIGNALS:
    // constant but it's only set after Separator::init
    void isVerticalChanged();
};

}

#endif
