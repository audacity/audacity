/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "RubberBandQuick.h"
#include "Config.h"

using namespace KDDockWidgets;

RubberBandQuick::RubberBandQuick(QQuickItem *parent)
    : QWidgetAdapter(parent)
{
    setVisible(false);
    setZ(1000);
    QQuickItem *visualItem = createItem(Config::self().qmlEngine(), QStringLiteral("qrc:/kddockwidgets/private/quick/qml/RubberBand.qml"));
    visualItem->setParent(this);
    visualItem->setParentItem(this);
}
