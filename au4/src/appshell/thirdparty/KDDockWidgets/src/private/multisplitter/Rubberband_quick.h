/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_MULTISPLITTER_RUBBERBAND_QUICK_H
#define KD_MULTISPLITTER_RUBBERBAND_QUICK_H

#include "kddockwidgets/docks_export.h"
#include "Widget_quick.h"

#include <QQuickItem>

namespace Layouting {

// TODO: Finish rubberband
class DOCKS_EXPORT_FOR_UNIT_TESTS RubberBand
    : public QQuickItem,
      public Layouting::Widget_quick
{
    Q_OBJECT
public:
    explicit RubberBand(Layouting::Widget *parent);
    ~RubberBand() override;
};

}

#endif
