/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "QmlTypes.h"
#include "DockWidgetBase.h"
#include "../DropAreaWithCentralFrame_p.h"
#include "../DropIndicatorOverlayInterface_p.h"
#include "../Frame_p.h"
#include "../TitleBar_p.h"
#include "../indicators/ClassicIndicators_p.h"
#include "DockWidgetInstantiator_p.h"
#include "MainWindowInstantiator_p.h"
#include "LayoutSaverInstantiator_p.h"

#include <QQmlEngine>
#include <QDebug>

void KDDockWidgets::registerQmlTypes()
{
    qmlRegisterType<DropAreaWithCentralFrame>("com.kdab.dockwidgets", 1, 0, "DropAreaWithCentralFrame");
    qmlRegisterType<MainWindowInstantiator>("com.kdab.dockwidgets", 1, 0, "MainWindowLayout");
    qmlRegisterType<DockWidgetInstantiator>("com.kdab.dockwidgets", 1, 0, "DockWidget");
    qmlRegisterType<LayoutSaverInstantiator>("com.kdab.dockwidgets", 1, 0, "LayoutSaver");

    qmlRegisterUncreatableType<TitleBar>("com.kdab.dockwidgets", 1, 0, "TitleBar", QStringLiteral("Enum access only"));
    qmlRegisterUncreatableType<DropIndicatorOverlayInterface>("com.kdab.dockwidgets", 1, 0, "DropIndicatorOverlayInterface", QStringLiteral("Enum access only"));
    qmlRegisterUncreatableMetaObject(KDDockWidgets::staticMetaObject, "com.kdab.dockwidgets", 1, 0, "KDDockWidgets", QStringLiteral("Enum access only"));

    qRegisterMetaType<Frame *>();
    qRegisterMetaType<DropArea *>();
    qRegisterMetaType<DockWidgetBase *>();
    qRegisterMetaType<ClassicIndicators *>();
}
