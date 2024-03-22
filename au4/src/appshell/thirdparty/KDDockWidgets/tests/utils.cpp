/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "utils.h"
#include "DropArea_p.h"
#include "Config.h"
#include "TitleBar_p.h"
#include "FloatingWindow_p.h"
#include "FrameworkWidgetFactory.h"

#include <QCloseEvent>
#include <QDebug>
#include <QPainter>
#include <QtTest/QtTest>

#ifdef KDDOCKWIDGETS_QTQUICK
# include "DockWidgetQuick.h"
# include "private/quick/MainWindowQuick_p.h"
# include <QQuickView>
#else
# include "DockWidget.h"
# include "MainWindow.h"
# include <QPushButton>
#endif

using namespace KDDockWidgets;
using namespace KDDockWidgets::Tests;

// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates


std::unique_ptr<KDDockWidgets::MainWindowBase>
KDDockWidgets::Tests::createMainWindow(QSize sz, KDDockWidgets::MainWindowOptions options,
                                       const QString &name, bool show)
{
    static int count = 0;
    count++;

    if (!sz.isValid())
        sz = QSize(1000, 1000);

    const QString mainWindowName = name.isEmpty() ? QStringLiteral("MyMainWindow%1").arg(count)
                                                  : name;

    WidgetType *parent = nullptr;
#ifdef KDDOCKWIDGETS_QTQUICK
    auto view = new QQuickView(Config::self().qmlEngine(), nullptr);
    view->resize(sz);
    view->setResizeMode(QQuickView::SizeRootObjectToView);
    view->setSource(QUrl("qrc:/main.qml"));
    if (show)
        view->show();
    parent = view->rootObject();
    QTest::qWait(100); // the root object gets sized delayed
#endif

    auto ptr = std::unique_ptr<MainWindowType>(new MainWindowType(mainWindowName, options, parent));
    if (show)
        ptr->show();
    ptr->resize(sz);
    return ptr;
}

DockWidgetBase *KDDockWidgets::Tests::createDockWidget(const QString &name, QWidgetOrQuick *w,
                                                       DockWidgetBase::Options options,
                                                       DockWidgetBase::LayoutSaverOptions layoutSaverOptions,
                                                       bool show,
                                                       const QString &affinityName)
{
    w->setFocusPolicy(Qt::StrongFocus);
    auto dock = new DockWidgetType(name, options, layoutSaverOptions);
    dock->setAffinityName(affinityName);
    dock->setWidget(w);
    dock->setObjectName(name);
    dock->setGeometry(QRect(0, 0, 400, 400));
    if (show) {
        dock->show();
        dock->dptr()->morphIntoFloatingWindow();
        dock->activateWindow();
        Q_ASSERT(dock->window());
        if (QTest::qWaitForWindowActive(dock->window()->windowHandle(), 1000)) {
            return dock;
        }
        qWarning() << Q_FUNC_INFO << "Couldn't activate window";
        return nullptr;
    } else {
        return dock;
    }
};

DockWidgetBase *KDDockWidgets::Tests::createDockWidget(const QString &name, QColor color)
{
    return createDockWidget(name, new MyWidget(name, color));
};

static QWidgetOrQuick *createGuestWidget(int i)
{
#ifdef KDDOCKWIDGETS_QTWIDGETS
    return new QPushButton(QStringLiteral("%1").arg(i));
#else
    Q_UNUSED(i);
    return new QWidgetAdapter();
#endif
}

std::unique_ptr<MainWindowBase> KDDockWidgets::Tests::createMainWindow(QVector<DockDescriptor> &docks)
{
    static int count = 0;
    count++;

    WidgetType *parent = nullptr;
#ifdef KDDOCKWIDGETS_QTQUICK
    auto view = new QQuickView(Config::self().qmlEngine(), nullptr);
    const QSize initialSize(1000, 1000);
    view->setResizeMode(QQuickView::SizeRootObjectToView);
    view->resize(initialSize);
    view->setSource(QUrl("qrc:/main.qml"));
    view->show();
    parent = view->rootObject();
    QTest::qWait(100); // the root object gets sized delayed
#endif

    auto m = std::unique_ptr<MainWindowType>(new MainWindowType(QStringLiteral("MyMainWindow%1").arg(count), MainWindowOption_None, parent));
    auto layout = m->layoutWidget();
    m->show();
    m->resize(QSize(700, 700));

    int i = 0;
    for (DockDescriptor &desc : docks) {
        desc.createdDock = createDockWidget(QStringLiteral("%1-%2").arg(i).arg(count), createGuestWidget(i), {}, {}, false);
        DockWidgetBase *relativeTo = nullptr;
        if (desc.relativeToIndex != -1)
            relativeTo = docks.at(desc.relativeToIndex).createdDock;

        m->addDockWidget(desc.createdDock, desc.loc, relativeTo, desc.option);

        layout->checkSanity();
        ++i;
    }

    return m;
}

MyWidget::MyWidget(const QString &, QColor c)
    : QWidgetOrQuick()
    , c(c)
{
}

MyWidget::~MyWidget()
{
}

#ifdef KDDOCKWIDGETS_QTWIDGETS
void MyWidget::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    p.fillRect(rect(), c);
}

NonClosableWidget::NonClosableWidget(QWidget *parent)
    : QWidget(parent)
{
}

NonClosableWidget::~NonClosableWidget()
{
}

void NonClosableWidget::closeEvent(QCloseEvent *ev)
{
    ev->ignore(); // don't allow to close
}

#endif

bool KDDockWidgets::Tests::shouldBlacklistWarning(const QString &msg, const QString &category)
{
    if (category == QLatin1String("qt.qpa.xcb"))
        return true;

    return msg.contains(QLatin1String("QSocketNotifier: Invalid socket")) ||
           msg.contains(QLatin1String("QWindowsWindow::setGeometry")) ||
           msg.contains(QLatin1String("This plugin does not support")) ||
           msg.contains(QLatin1String("Note that Qt no longer ships fonts")) ||
           msg.contains(QLatin1String("Another dock KDDockWidgets::DockWidget")) ||
           msg.contains(QLatin1String("There's multiple MainWindows, not sure what to do about parenting"));
}

void KDDockWidgets::Tests::doubleClickOn(QPoint globalPos, WidgetType *receiver)
{
    QCursor::setPos(globalPos);
    pressOn(globalPos, receiver); // double-click involves an initial press

    QMouseEvent ev(QEvent::MouseButtonDblClick, receiver->mapFromGlobal(globalPos), receiver->window()->mapFromGlobal(globalPos), globalPos,
                   Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);

    if (auto actualReceiver = receiver->property("titleBarMouseArea").value<QObject*>()) {
        // QtQuick case, we need to send the event to the mouse area
        qApp->sendEvent(actualReceiver, &ev);
    } else {
        // QtWidgets case
        qApp->sendEvent(receiver, &ev);
    }
}

void KDDockWidgets::Tests::doubleClickOn(QPoint globalPos, QWindow *receiver)
{
    QCursor::setPos(globalPos);
    QMouseEvent ev(QEvent::MouseButtonDblClick, receiver->mapFromGlobal(globalPos), receiver->mapFromGlobal(globalPos), globalPos,
                   Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);

    pressOn(globalPos, receiver); // double-click involves an initial press
    qApp->sendEvent(receiver, &ev);
}

void KDDockWidgets::Tests::pressOn(QPoint globalPos, WidgetType *receiver)
{
    QCursor::setPos(globalPos);
    QMouseEvent ev(QEvent::MouseButtonPress, receiver->mapFromGlobal(globalPos), receiver->window()->mapFromGlobal(globalPos), globalPos,
                   Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
    qApp->sendEvent(receiver, &ev);
}

void KDDockWidgets::Tests::pressOn(QPoint globalPos, QWindow *receiver)
{
    QCursor::setPos(globalPos);
    QMouseEvent ev(QEvent::MouseButtonPress, receiver->mapFromGlobal(globalPos), receiver->mapFromGlobal(globalPos), globalPos,
                   Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
    qApp->sendEvent(receiver, &ev);
}

void KDDockWidgets::Tests::releaseOn(QPoint globalPos, WidgetType *receiver)
{
    QMouseEvent ev(QEvent::MouseButtonRelease, receiver->mapFromGlobal(globalPos), receiver->window()->mapFromGlobal(globalPos), globalPos,
                   Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
    qApp->sendEvent(receiver, &ev);
}

void KDDockWidgets::Tests::clickOn(QPoint globalPos, WidgetType *receiver)
{
    pressOn(globalPos, receiver);
    releaseOn(globalPos, receiver);
}

void KDDockWidgets::Tests::moveMouseTo(QPoint globalDest, WidgetType *receiver)
{
    QPoint globalSrc = KDDockWidgets::mapToGlobal(receiver, QPoint(5, 5));
    auto receiverP = Tests::make_qpointer(receiver);

    while (globalSrc != globalDest) {
        if (globalSrc.x() < globalDest.x()) {
            globalSrc.setX(globalSrc.x() + 1);
        } else if (globalSrc.x() > globalDest.x()) {
            globalSrc.setX(globalSrc.x() - 1);
        }
        if (globalSrc.y() < globalDest.y()) {
            globalSrc.setY(globalSrc.y() + 1);
        } else if (globalSrc.y() > globalDest.y()) {
            globalSrc.setY(globalSrc.y() - 1);
        }

        QCursor::setPos(globalSrc); // Since some code uses QCursor::pos()
        QMouseEvent ev(QEvent::MouseMove, receiver->mapFromGlobal(globalSrc), receiver->window()->mapFromGlobal(globalSrc), globalSrc,
                       Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);

        if (!receiverP) {
            qWarning() << "Receiver was deleted";
            return;
        }

        qApp->sendEvent(receiver, &ev);
        QTest::qWait(2);
    }
}

void KDDockWidgets::Tests::nestDockWidget(DockWidgetBase *dock, DropArea *dropArea, Frame *relativeTo, Location location)
{
    auto frame = Config::self().frameworkWidgetFactory()->createFrame();
    frame->addWidget(dock);
    dock->d->frame()->setObjectName(dock->objectName());

    dropArea->addWidget(frame, location, relativeTo);
    QVERIFY(dropArea->checkSanity());
}

EmbeddedWindow::~EmbeddedWindow() = default;

#ifdef KDDOCKWIDGETS_QTQUICK
    MyWidget2::~MyWidget2() = default;
    NonClosableWidget::~NonClosableWidget() = default;
    QTextEdit::~QTextEdit() = default;
    FocusableWidget::~FocusableWidget() = default;
    QPushButton::~QPushButton() = default;
#endif
