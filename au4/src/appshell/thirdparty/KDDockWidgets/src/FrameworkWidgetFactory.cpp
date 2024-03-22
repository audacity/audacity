/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "FrameworkWidgetFactory.h"
#include "Config.h"

#include "private/Frame_p.h"
#include "private/TitleBar_p.h"
#include "private/multisplitter/Separator_p.h"
#include "private/FloatingWindow_p.h"
#include "private/indicators/ClassicIndicators_p.h"
#include "private/indicators/NullIndicators_p.h"
#include "private/Utils_p.h"
#include "private/TabWidget_p.h"

#ifdef KDDOCKWIDGETS_QTWIDGETS
#include "private/widgets/FrameWidget_p.h"
#include "private/widgets/TitleBarWidget_p.h"
#include "private/widgets/TabBarWidget_p.h"
#include "private/widgets/SideBarWidget_p.h"
#include "private/widgets/TabWidgetWidget_p.h"
#include "private/multisplitter/Separator_qwidget.h"
#include "private/widgets/FloatingWindowWidget_p.h"
#include "private/indicators/SegmentedIndicators_p.h"

#include <QRubberBand>
#include <QToolButton>
#else
#include "DockWidgetQuick.h"
#include "private/quick/FrameQuick_p.h"
#include "private/quick/TitleBarQuick_p.h"
#include "private/quick/TabWidgetQuick_p.h"
#include "private/quick/TabBarQuick_p.h"
#include "private/quick/FloatingWindowQuick_p.h"
#include "private/quick/RubberBandQuick.h"
#include "private/multisplitter/Separator_quick.h"
#endif

// clazy:excludeall=ctor-missing-parent-argument

using namespace KDDockWidgets;

DropIndicatorType DefaultWidgetFactory::s_dropIndicatorType = DropIndicatorType::Classic;

FrameworkWidgetFactory::~FrameworkWidgetFactory()
{
}

#ifdef KDDOCKWIDGETS_QTWIDGETS
Frame *DefaultWidgetFactory::createFrame(QWidgetOrQuick *parent, FrameOptions options) const
{
    return new FrameWidget(parent, options);
}

TitleBar *DefaultWidgetFactory::createTitleBar(Frame *frame) const
{
    return new TitleBarWidget(frame);
}

TitleBar *DefaultWidgetFactory::createTitleBar(FloatingWindow *fw) const
{
    return new TitleBarWidget(fw);
}

TabBar *DefaultWidgetFactory::createTabBar(TabWidget *parent) const
{
    return new TabBarWidget(parent);
}

TabWidget *DefaultWidgetFactory::createTabWidget(Frame *parent) const
{
    return new TabWidgetWidget(parent);
}

Layouting::Separator *DefaultWidgetFactory::createSeparator(Layouting::Widget *parent) const
{
    return new Layouting::SeparatorWidget(parent);
}

FloatingWindow *DefaultWidgetFactory::createFloatingWindow(MainWindowBase *parent) const
{
    return new FloatingWindowWidget(QRect(), parent);
}

FloatingWindow *DefaultWidgetFactory::createFloatingWindow(Frame *frame, MainWindowBase *parent, QRect suggestedGeometry) const
{
    return new FloatingWindowWidget(frame, suggestedGeometry, parent);
}

DropIndicatorOverlayInterface *DefaultWidgetFactory::createDropIndicatorOverlay(DropArea *dropArea) const
{
#ifdef Q_OS_WASM
    // On WASM windows don't support translucency, which is required for the classic indicators.
    return new SegmentedIndicators(dropArea);
#endif

    switch (s_dropIndicatorType) {
    case DropIndicatorType::Classic:
        return new ClassicIndicators(dropArea);
    case DropIndicatorType::Segmented:
        return new SegmentedIndicators(dropArea);
    case DropIndicatorType::None:
        return new NullIndicators(dropArea);
    }

    return new ClassicIndicators(dropArea);
}

QWidgetOrQuick *DefaultWidgetFactory::createRubberBand(QWidgetOrQuick *parent) const
{
    return new QRubberBand(QRubberBand::Rectangle, parent);
}

SideBar *DefaultWidgetFactory::createSideBar(SideBarLocation loc, MainWindowBase *parent) const
{
    return new SideBarWidget(loc, parent);
}

QAbstractButton *DefaultWidgetFactory::createTitleBarButton(QWidget *parent, TitleBarButtonType type) const
{
    if (!parent) {
        qWarning() << Q_FUNC_INFO << "Parent not provided";
        return nullptr;
    }

    auto button = new Button(parent);
    button->setIcon(iconForButtonType(type, parent->devicePixelRatioF()));

    return button;
}

#else

Frame *DefaultWidgetFactory::createFrame(QWidgetOrQuick *parent, FrameOptions options) const
{
    return new FrameQuick(parent, options);
}

TitleBar *DefaultWidgetFactory::createTitleBar(Frame *frame) const
{
    return new TitleBarQuick(frame);
}

TitleBar *DefaultWidgetFactory::createTitleBar(FloatingWindow *fw) const
{
    return new TitleBarQuick(fw);
}

/*Separator *DefaultWidgetFactory::createSeparator(QWidgetAdapter *parent) const
{
    return new SeparatorQuick(parent);
}*/

FloatingWindow *DefaultWidgetFactory::createFloatingWindow(MainWindowBase *parent) const
{
    return new FloatingWindowQuick(parent);
}

FloatingWindow *DefaultWidgetFactory::createFloatingWindow(Frame *frame, MainWindowBase *parent, QRect suggestedGeometry) const
{
    return new FloatingWindowQuick(frame, suggestedGeometry, parent);
}

DropIndicatorOverlayInterface *DefaultWidgetFactory::createDropIndicatorOverlay(DropArea *dropArea) const
{
    switch (s_dropIndicatorType) {
    case DropIndicatorType::Classic:
        return new ClassicIndicators(dropArea);
    case DropIndicatorType::Segmented:
        qWarning() << "Segmented indicators not supported for QtQuick yet";
        return new NullIndicators(dropArea);
    case DropIndicatorType::None:
        return new NullIndicators(dropArea);
    }

    return new ClassicIndicators(dropArea);
}

TabBar *DefaultWidgetFactory::createTabBar(TabWidget *parent) const
{
    return new TabBarQuick(parent);
}

TabWidget *DefaultWidgetFactory::createTabWidget(Frame *parent) const
{
    return new TabWidgetQuick(parent);
}

Layouting::Separator *DefaultWidgetFactory::createSeparator(Layouting::Widget *parent) const
{
    return new Layouting::SeparatorQuick(parent);
}

QWidgetOrQuick *DefaultWidgetFactory::createRubberBand(QWidgetOrQuick *parent) const
{
    return new RubberBandQuick(parent);
}

SideBar *DefaultWidgetFactory::createSideBar(SideBarLocation loc, MainWindowBase *parent) const
{
    Q_UNUSED(loc);
    Q_UNUSED(parent);

    qWarning() << Q_FUNC_INFO << "Not implemented yet";
    return nullptr;
}

QUrl DefaultWidgetFactory::titleBarFilename() const
{
    return QUrl(QStringLiteral("qrc:/kddockwidgets/private/quick/qml/TitleBar.qml"));
}

QUrl DefaultWidgetFactory::dockwidgetFilename() const
{
    return QUrl(QStringLiteral("qrc:/kddockwidgets/private/quick/qml/DockWidget.qml"));
}

QUrl DefaultWidgetFactory::frameFilename() const
{
    return QUrl(QStringLiteral("qrc:/kddockwidgets/private/quick/qml/Frame.qml"));
}

QUrl DefaultWidgetFactory::floatingWindowFilename() const
{
    return QUrl(QStringLiteral("qrc:/kddockwidgets/private/quick/qml/FloatingWindow.qml"));
}

#endif // QtQuick

// iconForButtonType impl is the same for QtQuick and QtWidgets
QIcon DefaultWidgetFactory::iconForButtonType(TitleBarButtonType type, qreal dpr) const
{
    QString iconName;
    switch (type) {
    case TitleBarButtonType::AutoHide:
        iconName = QStringLiteral("auto-hide");
        break;
    case TitleBarButtonType::UnautoHide:
        iconName = QStringLiteral("unauto-hide");
        break;
    case TitleBarButtonType::Close:
        iconName = QStringLiteral("close");
        break;
    case TitleBarButtonType::Minimize:
        iconName = QStringLiteral("min");
        break;
    case TitleBarButtonType::Maximize:
        iconName = QStringLiteral("max");
        break;
    case TitleBarButtonType::Normal:
        // We're using the same icon as dock/float
        iconName = QStringLiteral("dock-float");
        break;
    case TitleBarButtonType::Float:
        iconName = QStringLiteral("dock-float");
        break;
    }

    if (iconName.isEmpty())
        return {};

    QIcon icon(QStringLiteral(":/img/%1.png").arg(iconName));
    if (!scalingFactorIsSupported(dpr))
        return icon;

    // Not using Qt's sugar syntax, which doesn't support 1.5x anyway when we need it.
    // Simply add the high-res files and Qt will pick them when needed

    if (scalingFactorIsSupported(1.5))
        icon.addFile(QStringLiteral(":/img/%1-1.5x.png").arg(iconName));

    icon.addFile(QStringLiteral(":/img/%1-2x.png").arg(iconName));

    return icon;
}
