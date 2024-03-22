/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "docksetup.h"

#include "internal/dropcontroller.h"
#include "internal/dockseparator.h"
#include "internal/dockframemodel.h"
#include "internal/docktabbar.h"
#include "internal/dockwindowactionscontroller.h"
#include "internal/dockwindowprovider.h"

#include "dockwindow.h"
#include "dockpanelview.h"
#include "docktoolbarview.h"
#include "dockstatusbarview.h"
#include "dockingholderview.h"
#include "dockcentralview.h"
#include "dockpageview.h"
#include "docktitlebar.h"

#include "docktypes.h"

#include "thirdparty/KDDockWidgets/src/Config.h"
#include "thirdparty/KDDockWidgets/src/DockWidgetBase.h"
#include "thirdparty/KDDockWidgets/src/FrameworkWidgetFactory.h"
#include "thirdparty/KDDockWidgets/src/private/FloatingWindow_p.h"

#include <QQmlEngine>

namespace mu::dock {
class DockWidgetFactory : public KDDockWidgets::DefaultWidgetFactory
{
public:
    KDDockWidgets::DropIndicatorOverlayInterface* createDropIndicatorOverlay(KDDockWidgets::DropArea* dropArea) const override
    {
        return new DropController(dropArea);
    }

    Layouting::Separator* createSeparator(Layouting::Widget* parent = nullptr) const override
    {
        return new DockSeparator(parent);
    }

    KDDockWidgets::TitleBar* createTitleBar(KDDockWidgets::Frame* frame) const override
    {
        return new DockTitleBar(frame);
    }

    KDDockWidgets::TitleBar* createTitleBar(KDDockWidgets::FloatingWindow* floatingWindow) const override
    {
        return new DockTitleBar(floatingWindow);
    }

    KDDockWidgets::TabBar* createTabBar(KDDockWidgets::TabWidget* parent) const override
    {
        return new DockTabBar(parent);
    }

    QUrl titleBarFilename() const override
    {
        return QUrl("qrc:/qml/dockwindow/DockTitleBar.qml");
    }

    QUrl dockwidgetFilename() const override
    {
        return QUrl("qrc:/qml/dockwindow/DockWidget.qml");
    }

    QUrl frameFilename() const override
    {
        return QUrl("qrc:/qml/dockwindow/DockFrame.qml");
    }

    QUrl floatingWindowFilename() const override
    {
        return QUrl("qrc:/qml/dockwindow/DockFloatingWindow.qml");
    }
};
}

using namespace mu::dock;
using namespace mu::modularity;

static std::shared_ptr<DockWindowActionsController> s_actionsController = std::make_shared<DockWindowActionsController>();

void DockSetup::registerQmlTypes()
{
    qmlRegisterType<DockWindow>("MuseScore.Dock", 1, 0, "DockWindow");
    qmlRegisterType<DockPanelView>("MuseScore.Dock", 1, 0, "DockPanelView");
    qmlRegisterType<DockStatusBarView>("MuseScore.Dock", 1, 0, "DockStatusBar");
    qmlRegisterType<DockToolBarView>("MuseScore.Dock", 1, 0, "DockToolBarView");
    qmlRegisterType<DockingHolderView>("MuseScore.Dock", 1, 0, "DockingHolderView");
    qmlRegisterType<DockCentralView>("MuseScore.Dock", 1, 0, "DockCentralView");
    qmlRegisterType<DockPageView>("MuseScore.Dock", 1, 0, "DockPageView");
    qmlRegisterType<DockFrameModel>("MuseScore.Dock", 1, 0, "DockFrameModel");

    qmlRegisterUncreatableType<DockToolBarAlignment>("MuseScore.Dock", 1, 0, "DockToolBarAlignment", "Not creatable from QML");
    qmlRegisterUncreatableType<DockLocation>("MuseScore.Dock", 1, 0, "Location", "Not creatable from QML");
}

void DockSetup::registerExports()
{
    ioc()->registerExport<IDockWindowProvider>("dock", new DockWindowProvider());
}

void DockSetup::setup(QQmlEngine* engine)
{
    KDDockWidgets::Config::self().setFrameworkWidgetFactory(new DockWidgetFactory());
    KDDockWidgets::Config::self().setQmlEngine(engine);

    auto flags = KDDockWidgets::Config::self().flags()
                 | KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible
                 | KDDockWidgets::Config::Flag_TitleBarNoFloatButton;

    KDDockWidgets::Config::self().setFlags(flags);

    KDDockWidgets::FloatingWindow::s_windowFlagsOverride = Qt::Tool
                                                           | Qt::NoDropShadowWindowHint
                                                           | Qt::FramelessWindowHint;

    auto internalFlags = KDDockWidgets::Config::self().internalFlags()
                         | KDDockWidgets::Config::InternalFlag_UseTransparentFloatingWindow;

    KDDockWidgets::Config::self().setInternalFlags(internalFlags);

    KDDockWidgets::Config::self().setAbsoluteWidgetMinSize(QSize(10, 10));
    KDDockWidgets::Config::self().setSeparatorThickness(1);
}

void DockSetup::onInit()
{
    s_actionsController->init();
}
