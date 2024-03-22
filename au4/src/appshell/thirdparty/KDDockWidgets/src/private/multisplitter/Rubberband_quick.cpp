/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "Rubberband_quick.h"

using namespace Layouting;

RubberBand::RubberBand(Layouting::Widget *parent)
    : QQuickItem(parent ? qobject_cast<QQuickItem *>(parent->asQObject()) : nullptr)
    , Layouting::Widget_quick(this)
{
}

RubberBand::~RubberBand() = default;
