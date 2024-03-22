/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

// We don't care about performance related checks in the tests
// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#include "tst_docks.h"
#include "Config.h"
#include "DockWidgetBase.h"
#include "DockWidgetBase_p.h"
#include "DropAreaWithCentralFrame_p.h"
#include "LayoutSaver_p.h"
#include "MDILayoutWidget_p.h"
#include "MainWindowMDI.h"
#include "Position_p.h"
#include "SideBar_p.h"
#include "TabWidget_p.h"
#include "TitleBar_p.h"
#include "WindowBeingDragged_p.h"
#include "multisplitter/Separator_p.h"
#include "private/MultiSplitter_p.h"

#include <QAction>

#ifdef Q_OS_WIN
# include <windows.h>
#endif

using namespace KDDockWidgets;
using namespace Layouting;
using namespace KDDockWidgets::Tests;

static int osWindowMinWidth()
{
#ifdef Q_OS_WIN
    return GetSystemMetrics(SM_CXMIN);
#else
    return 140; // Some random value for our windows. It's only important on Windows
#endif
}

static QPoint dragPointForWidget(Frame *frame, int index)
{
    if (frame->hasSingleDockWidget()) {
        Q_ASSERT(index == 0);
        return frame->titleBar()->mapToGlobal(QPoint(5, 5));
    } else {
        QRect rect = frame->tabWidget()->tabBar()->rectForTab(index);
        return frame->tabWidget()->tabBar()->asWidget()->mapToGlobal(rect.center());
    }
}

template <typename T>
inline int widgetMinLength(const T *w, Qt::Orientation o)
{
    const QSize sz = Widget::widgetMinSize(w);
    return o == Qt::Vertical ? sz.height() : sz.width();
}

static DockWidgetBase *createAndNestDockWidget(DropArea *dropArea, Frame *relativeTo,
                                               KDDockWidgets::Location location)
{
    static int count = 0;
    count++;
    const QString name = QString("dock%1").arg(count);
    auto dock = createDockWidget(name, Qt::red);
    dock->setObjectName(name);
    nestDockWidget(dock, dropArea, relativeTo, location);
    dropArea->checkSanity();
    return dock;
}

static std::unique_ptr<MainWindowBase> createSimpleNestedMainWindow(DockWidgetBase * *centralDock,
                                                                    DockWidgetBase * *leftDock,
                                                                    DockWidgetBase * *rightDock)
{
    auto window = createMainWindow({900, 500});
    *centralDock = createDockWidget("centralDock", Qt::green);
    window->addDockWidgetAsTab(*centralDock);
    auto dropArea = window->dropArea();

    *leftDock = createAndNestDockWidget(dropArea, nullptr, KDDockWidgets::Location_OnLeft);
    *rightDock = createAndNestDockWidget(dropArea, nullptr, KDDockWidgets::Location_OnRight);
    return window;
}

void TestDocks::tst_simple1()
{
    // Simply create a MainWindow
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_simple2()
{
    // Simply create a MainWindow, and dock something on top
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dw = createDockWidget("dw", new MyWidget("dw", Qt::blue));
    auto fw = dw->floatingWindow();
    m->addDockWidget(dw, KDDockWidgets::Location_OnTop);
    m->layoutWidget()->checkSanity();
    delete fw;
}

void TestDocks::tst_restoreSimple()
{
    EnsureTopLevelsDeleted e;
    // Tests restoring a very simple layout, composed of just 1 docked widget

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto layout = m->multiSplitter();
    auto dock1 = createDockWidget("one", new QTextEdit());
    auto dock2 = createDockWidget("two", new QTextEdit());
    auto dock3 = createDockWidget("three", new QTextEdit());

    m->addDockWidget(dock1, Location_OnTop);

    // Dock2 floats at 150,150
    const QPoint dock2FloatingPoint = QPoint(150, 150);
    dock2->window()->move(dock2FloatingPoint);
    QVERIFY(dock2->isVisible());

    const QPoint dock3FloatingPoint = QPoint(200, 200);
    dock3->window()->move(dock3FloatingPoint);
    dock3->close();

    LayoutSaver saver;
    QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreSimple.json")));
    auto f1 = dock1->dptr()->frame();
    dock2->window()->move(QPoint(0, 0)); // Move *after* we saved.
    dock3->window()->move(QPoint(0, 0)); // Move *after* we saved.
    dock1->close();
    dock2->close();
    QVERIFY(!dock2->isVisible());
    QCOMPARE(layout->count(), 1);
    QVERIFY(Testing::waitForDeleted(f1));
    QCOMPARE(layout->placeholderCount(), 1);

    QCOMPARE(DockRegistry::self()->floatingWindows().size(), 0);
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreSimple.json")));
    QVERIFY(layout->checkSanity());
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);
    QVERIFY(dock1->isVisible());
    QCOMPARE(saver.restoredDockWidgets().size(), 3);

    // Test a crash I got:
    dock1->setFloating(true);
    QVERIFY(layout->checkSanity());
    dock1->setFloating(false);

    auto fw2 = dock2->floatingWindow();
    QVERIFY(fw2);
    QVERIFY(fw2->isVisible());
    QVERIFY(fw2->QWidgetAdapter::isTopLevel());
    QCOMPARE(fw2->pos(), dock2FloatingPoint);
    QCOMPARE(fw2->windowHandle()->transientParent(), m->windowHandle());
    QVERIFY(dock2->isFloating());
    QVERIFY(dock2->isVisible());

    QVERIFY(!dock3->isVisible()); // Remains closed
    QVERIFY(dock3->parentWidget() == nullptr);

    dock3->show();
    dock3->dptr()->morphIntoFloatingWindow(); // as it would take 1 event loop. Do it now so we can
                                              // compare already.

    QCOMPARE(dock3->window()->pos(), dock3FloatingPoint);
}

void TestDocks::tst_doesntHaveNativeTitleBar()
{
    // Tests that a floating window doesn't have a native title bar
    // This test is mostly to test a bug that was happening with QtQuick, where the native title bar
    // would appear on linux
    EnsureTopLevelsDeleted e;

    auto dw1 = createDockWidget("dock1");
    FloatingWindow *fw = dw1->floatingWindow();
    QVERIFY(fw);
    QVERIFY(fw->windowFlags() & Qt::Tool);

#if defined(Q_OS_LINUX)
    QVERIFY(fw->windowFlags() & Qt::FramelessWindowHint);
#elif defined(Q_OS_WIN)
    QVERIFY(!(fw->windowFlags() & Qt::FramelessWindowHint));
#endif

    delete dw1->window();
}

void TestDocks::tst_resizeWindow2()
{
    // Tests that resizing the width of the main window will never move horizontal anchors

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1");
    auto dock2 = createDockWidget("2");

    FloatingWindow *fw1 = dock1->floatingWindow();
    FloatingWindow *fw2 = dock2->floatingWindow();
    m->addDockWidget(dock1, Location_OnTop);
    m->addDockWidget(dock2, Location_OnBottom);

    auto layout = m->multiSplitter();
    Separator *anchor = layout->separators().at(0);
    const int oldPosY = anchor->position();
    m->resize(QSize(m->width() + 10, m->height()));
    QCOMPARE(anchor->position(), oldPosY);
    layout->checkSanity();

    delete fw1;
    delete fw2;
}

void TestDocks::tst_hasLastDockedLocation()
{
    // Tests DockWidgetBase::hasPreviousDockedLocation()

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1");
    m->layoutWidget()->checkSanity();
    m->multiSplitter()->setObjectName("mainWindow-dropArea");
    dock1->floatingWindow()->layoutWidget()->setObjectName("first-dropArea1");
    dock1->floatingWindow()->layoutWidget()->checkSanity();
    auto window1 = dock1->window();
    QVERIFY(dock1->isFloating());
    QVERIFY(!dock1->hasPreviousDockedLocation());
    QVERIFY(dock1->setFloating(true));
    QVERIFY(!dock1->setFloating(false)); // No docking location, so it's not docked
    QVERIFY(dock1->isFloating());
    QVERIFY(!dock1->hasPreviousDockedLocation());

    m->addDockWidget(dock1, Location_OnBottom);
    m->layoutWidget()->checkSanity();

    QVERIFY(!dock1->isFloating());
    QVERIFY(dock1->setFloating(true));

    auto ms1 = dock1->floatingWindow()->layoutWidget();
    ms1->setObjectName("dropArea1");
    ms1->checkSanity();
    QVERIFY(dock1->hasPreviousDockedLocation());
    auto window11 = dock1->window();
    QVERIFY(dock1->setFloating(false));

    delete window1;
    delete window11;
}

void TestDocks::tst_ghostSeparator()
{
    // Tests a situation where a separator wouldn't be removed after a widget had been removed
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1");
    auto dock2 = createDockWidget("2");
    auto dock3 = createDockWidget("3");

    QPointer<FloatingWindow> fw1 = dock1->floatingWindow();
    QPointer<FloatingWindow> fw2 = dock2->floatingWindow();
    QPointer<FloatingWindow> fw3 = dock3->floatingWindow();

    dock1->addDockWidgetToContainingWindow(dock2, Location_OnRight);
    QCOMPARE(fw1->multiSplitter()->separators().size(), 1);
    QCOMPARE(Layouting::Separator::numSeparators(), 1);

    m->addDockWidget(dock3, Location_OnBottom);
    QCOMPARE(m->multiSplitter()->separators().size(), 0);
    QCOMPARE(Layouting::Separator::numSeparators(), 1);

    m->multiSplitter()->addMultiSplitter(fw1->multiSplitter(), Location_OnRight);
    QCOMPARE(m->multiSplitter()->separators().size(), 2);
    QCOMPARE(Layouting::Separator::numSeparators(), 2);

    delete fw1;
    delete fw2;
    delete fw3;
}

void TestDocks::tst_detachFromMainWindow()
{
    // Tests a situation where clicking the float button wouldn't work on QtQuick
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1");
    auto fw1 = dock1->window();
    m->addDockWidget(dock1, Location_OnTop);

    QVERIFY(m->layoutWidget()->mainWindow() != nullptr);
    QVERIFY(!dock1->isFloating());
    TitleBar *tb = dock1->titleBar();
    QVERIFY(tb == dock1->dptr()->frame()->titleBar());
    QVERIFY(tb->isVisible());
    QVERIFY(!tb->isFloating());

    delete fw1;
}

void TestDocks::tst_detachPos()
{
    // Tests a situation where detaching a dock widget would send it to a bogus position
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new MyWidget(QStringLiteral("1"), Qt::black), {}, {}, /** show = */false); // we're creating the dock widgets without showing them as floating initially, so it doesn't record the previous floating position
    auto dock2 = createDockWidget("2", new MyWidget(QStringLiteral("2"), Qt::black), {}, {}, /** show = */false);

    QVERIFY(!dock1->isVisible());
    QVERIFY(!dock2->isVisible());

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);

    QVERIFY(!dock1->dptr()->lastPositions().lastFloatingGeometry().isValid());
    QVERIFY(!dock2->dptr()->lastPositions().lastFloatingGeometry().isValid());

    const int previousWidth = dock1->width();
    dock1->setFloating(true);
    QTest::qWait(400); // Needed for QtQuick

    QVERIFY(qAbs(previousWidth - dock1->width()) < 15); // 15px of difference when floating is fine, due to margins and what not.
    delete dock1->window();
}

void TestDocks::tst_floatingWindowSize()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1");
    auto fw1 = dock1->window();

    QTest::qWait(100);

    QVERIFY(!fw1->geometry().isNull());
    QCOMPARE(fw1->size(), fw1->windowHandle()->size());

    delete fw1;
}

void TestDocks::tst_tabbingWithAffinities()
{
    EnsureTopLevelsDeleted e;
    // Tests that dock widgets with different affinities should not tab together

    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    m1->setAffinities({ "af1", "af2" });

    auto dw1 = new DockWidgetType("1");
    dw1->setAffinities({ "af1" });
    dw1->show();

    auto dw2 = new DockWidgetType("2");
    dw2->setAffinities({ "af2" });
    dw2->show();

    FloatingWindow *fw1 = dw1->floatingWindow();
    FloatingWindow *fw2 = dw2->floatingWindow();

    {
        SetExpectedWarning ignoreWarning("Refusing to dock widget with incompatible affinity");
        dw1->addDockWidgetAsTab(dw2);
        QVERIFY(dw1->window() != dw2->window());
    }

    m1->addDockWidget(dw1, Location_OnBottom);
    QVERIFY(!dw1->isFloating());

    {
        SetExpectedWarning ignoreWarning("Refusing to dock widget with incompatible affinity");
        auto dropArea = m1->dropArea();
        WindowBeingDragged wbd(fw2, fw2);
        QVERIFY(!dropArea->drop(&wbd, dw1->dptr()->frame(),
                                DropIndicatorOverlayInterface::DropLocation_Center));
        QVERIFY(dw1->window() != dw2->window());
    }

    delete fw1;
    delete fw2;
}

void TestDocks::tst_sizeAfterRedock()
{
    EnsureTopLevelsDeleted e;
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto dw2 = new DockWidgetType(QStringLiteral("2"));
    dw2->setWidget(new MyWidget("2", Qt::red));

    dw1->addDockWidgetToContainingWindow(dw2, Location_OnBottom);
    const int height2 = dw2->dptr()->frame()->height();

    dw2->setFloating(true);
    QTest::qWait(100);

    QCOMPARE(height2, dw2->window()->height());
    auto oldFw2 = dw2->floatingWindow();

    // Redock
    FloatingWindow *fw1 = dw1->floatingWindow();
    DropArea *dropArea = fw1->dropArea();

    MultiSplitter *ms1 = fw1->multiSplitter();
    {
        WindowBeingDragged wbd2(oldFw2);
        const QRect suggestedDropRect = ms1->rectForDrop(&wbd2, Location_OnBottom, nullptr);
        QCOMPARE(suggestedDropRect.height(), height2);
    }

    dropArea->drop(dw2->floatingWindow(), Location_OnBottom, nullptr);

    QCOMPARE(dw2->dptr()->frame()->height(), height2);

    delete dw1->window();
    delete oldFw2;
}

void TestDocks::tst_honourUserGeometry()
{
    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    QVERIFY(!dw1->testAttribute(Qt::WA_PendingMoveEvent));

    const QPoint pt(10, 10);
    dw1->move(pt);
    dw1->show();
    FloatingWindow *fw1 = dw1->floatingWindow();
    QCOMPARE(fw1->windowHandle()->geometry().topLeft(), pt);

    delete dw1->window();
}

void TestDocks::tst_floatingWindowTitleBug()
{
    // Test for #74
    EnsureTopLevelsDeleted e;
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto dw2 = new DockWidgetType(QStringLiteral("2"));
    auto dw3 = new DockWidgetType(QStringLiteral("3"));

    dw1->setObjectName(QStringLiteral("1"));
    dw2->setObjectName(QStringLiteral("2"));
    dw3->setObjectName(QStringLiteral("3"));

    dw1->show();
    dw1->addDockWidgetAsTab(dw2);
    dw1->addDockWidgetToContainingWindow(dw3, Location_OnBottom);

    dw1->titleBar()->onFloatClicked();

    QCOMPARE(dw3->titleBar()->title(), QLatin1String("3"));

    delete dw1->window();
    delete dw3->window();
}

void TestDocks::tst_resizeWindow_data()
{
    QTest::addColumn<bool>("doASaveRestore");
    QTest::newRow("false") << false;
    QTest::newRow("true") << true;
}

void TestDocks::tst_resizeWindow()
{
    QFETCH(bool, doASaveRestore);

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new MyWidget("1", Qt::red));
    auto dock2 = createDockWidget("2", new MyWidget("2", Qt::blue));
    QPointer<FloatingWindow> fw1 = dock1->floatingWindow();
    QPointer<FloatingWindow> fw2 = dock2->floatingWindow();
    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);

    auto layout = m->multiSplitter();

    layout->checkSanity();

    const int oldWidth1 = dock1->width();
    const int oldWidth2 = dock2->width();

    QVERIFY(oldWidth2 - oldWidth1 <= 1); // They're not equal if separator thickness if even

    if (doASaveRestore) {
        LayoutSaver saver;
        saver.restoreLayout(saver.serializeLayout());
    }

    m->showMaximized();
    Testing::waitForResize(m.get());

    const int maximizedWidth1 = dock1->width();
    const int maximizedWidth2 = dock2->width();

    const double relativeDifference = qAbs((maximizedWidth1 - maximizedWidth2) / (1.0 * layout->width()));

    QVERIFY(relativeDifference <= 0.01);

    m->showNormal();
    Testing::waitForResize(m.get());

    const int newWidth1 = dock1->width();
    const int newWidth2 = dock2->width();

    QCOMPARE(oldWidth1, newWidth1);
    QCOMPARE(oldWidth2, newWidth2);
    layout->checkSanity();

    delete fw1;
    delete fw2;
}

void TestDocks::tst_restoreTwice()
{
    // Tests that restoring multiple times doesn't hide the floating windows for some reason
    EnsureTopLevelsDeleted e;

    auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_restoreTwice");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidgetAsTab(dock1);

    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dock3 = createDockWidget("3", new QPushButton("3"));

    dock2->dptr()->morphIntoFloatingWindow();
    dock3->dptr()->morphIntoFloatingWindow();

    {
        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreTwice.json")));
        QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreTwice.json")));
        QVERIFY(dock2->isVisible());
        QVERIFY(dock3->isVisible());
    }

    {
        LayoutSaver saver;
        QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreTwice.json")));
        QVERIFY(dock2->isVisible());
        QVERIFY(dock3->isVisible());
        QVERIFY(dock2->window()->isVisible());
        QVERIFY(dock3->window()->isVisible());
        auto fw = dock2->floatingWindow();
        QVERIFY(fw);
    }
}

void TestDocks::tst_restoreEmpty()
{
    EnsureTopLevelsDeleted e;

    // Create an empty main window, save it to disk.
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto layout = m->multiSplitter();
    LayoutSaver saver;
    const QSize oldSize = m->size();
    QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreEmpty.json")));
    saver.restoreFromFile(QStringLiteral("layout_tst_restoreEmpty.json"));
    QVERIFY(m->layoutWidget()->checkSanity());
    QCOMPARE(layout->separators().size(), 0);
    QCOMPARE(layout->count(), 0);
    QCOMPARE(m->size(), oldSize);
    QVERIFY(layout->checkSanity());
}

void TestDocks::tst_restoreCentralFrame()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500));
    auto layout = m->multiSplitter();

    QCOMPARE(layout->count(), 1);
    Item *item = m->dropArea()->centralFrame();
    QVERIFY(item);
    auto frame = static_cast<Frame *>(item->guestAsQObject());
    QCOMPARE(frame->options(), FrameOption_IsCentralFrame | FrameOption_AlwaysShowsTabs);
    QVERIFY(!frame->titleBar()->isVisible());

    LayoutSaver saver;
    QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreCentralFrame.json")));
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreCentralFrame.json")));

    QCOMPARE(layout->count(), 1);
    item = m->dropArea()->centralFrame();
    QVERIFY(item);
    frame = static_cast<Frame *>(item->guestAsQObject());
    QCOMPARE(frame->options(), FrameOption_IsCentralFrame | FrameOption_AlwaysShowsTabs);
    QVERIFY(!frame->titleBar()->isVisible());
}

void TestDocks::tst_restoreMaximizedState()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();

    m->showMaximized();

    QCOMPARE(m->windowHandle()->windowState(), Qt::WindowMaximized);
    LayoutSaver saver;

    const QByteArray saved = saver.serializeLayout();
    m->showNormal();
    QVERIFY(m->windowHandle()->windowState() != Qt::WindowMaximized);

    saver.restoreLayout(saved);
    QCOMPARE(m->windowHandle()->windowState(), Qt::WindowMaximized);
}

void TestDocks::tst_restoreFloatingMaximizedState()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_TitleBarHasMaximizeButton);
    auto dock1 = createDockWidget("dock1", new MyWidget("one"));
    const QRect originalNormalGeometry = dock1->floatingWindow()->normalGeometry();
    dock1->floatingWindow()->showMaximized();
    qDebug() << originalNormalGeometry;

    QCOMPARE(dock1->floatingWindow()->windowHandle()->windowState(), Qt::WindowMaximized);

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();

    saver.restoreLayout(saved);
    QCOMPARE(dock1->floatingWindow()->windowHandle()->windowState(), Qt::WindowMaximized);
    QCOMPARE(dock1->floatingWindow()->normalGeometry(), originalNormalGeometry);

    dock1->floatingWindow()->showNormal();
    QCOMPARE(dock1->floatingWindow()->normalGeometry(), originalNormalGeometry);
}

void TestDocks::tst_restoreFloatingMinimizedState()
{
    EnsureTopLevelsDeleted e;
    auto dock1 = createDockWidget("dock1", new MyWidget("one"));
    dock1->floatingWindow()->showMinimized();

    QCOMPARE(dock1->floatingWindow()->windowHandle()->windowState(), Qt::WindowMinimized);

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();

    saver.restoreLayout(saved);
    QCOMPARE(dock1->floatingWindow()->windowHandle()->windowState(), Qt::WindowMinimized);
}

void TestDocks::tst_restoreNonExistingDockWidget()
{
    // If the layout is old and doesn't know about some dock widget, then we need to float it
    // before restoring the MainWindow's layout

    QByteArray saved;
    const QSize defaultMainWindowSize = { 500, 500 };

    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(defaultMainWindowSize, MainWindowOption_None, "mainwindow1");
        LayoutSaver saver;
        saved = saver.serializeLayout();
    }

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(defaultMainWindowSize, MainWindowOption_None, "mainwindow1");
    auto dock2 = createDockWidget("dock2", new MyWidget("dock2"));
    m->addDockWidget(dock2, Location_OnBottom);
    LayoutSaver restorer;
    SetExpectedWarning sew("Couldn't find dock widget");
    QVERIFY(restorer.restoreLayout(saved));
    auto da = m->dropArea();
    QVERIFY(m->dropArea()->checkSanity());
    QCOMPARE(da->frames().size(), 0);

    QVERIFY(dock2->isOpen());
    QVERIFY(dock2->isFloating());
}

void TestDocks::tst_setFloatingSimple()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new MyWidget("one"));
    m->addDockWidget(dock1, Location_OnTop);
    auto l = m->multiSplitter();
    dock1->setFloating(true);
    QVERIFY(l->checkSanity());
    dock1->setFloating(false);
    QVERIFY(l->checkSanity());
    dock1->setFloating(true);
    QVERIFY(l->checkSanity());
    dock1->setFloating(false);
    QVERIFY(l->checkSanity());
}

void TestDocks::tst_nonDockable()
{
    { // First test without Option_NotDockable
        auto dock = new DockWidgetType("1");
        dock->show();

        TitleBar *tb = dock->titleBar();
        QVERIFY(tb->isVisible());
        QVERIFY(tb->isFloatButtonVisible());

        delete dock->window();
    }

    {
        // Test that when using Option_NotDockable we don't get a dock/undock icon
        auto dock = new DockWidgetType("1", DockWidgetBase::Option_NotDockable);
        dock->show();

        TitleBar *tb = dock->titleBar();
        QVERIFY(tb->isVisible());
        QVERIFY(!tb->isFloatButtonVisible());

        delete dock->window();
    }
}

int main(int argc, char *argv[])
{
    if (!qpaPassedAsArgument(argc, argv)) {
        // Use offscreen by default as it's less annoying, doesn't create visible windows
        qputenv("QT_QPA_PLATFORM", "offscreen");
    }

    QApplication app(argc, argv);
    if (shouldSkipTests())
        return 0;

    TestDocks test;
    return QTest::qExec(&test, argc, argv);
}

void TestDocks::tst_closeDockWidgets()
{
    EnsureTopLevelsDeleted e;
    auto dock1 = createDockWidget("hello1", Qt::green);
    auto dock2 = createDockWidget("hello2", Qt::green);

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    m->addDockWidget(dock1, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnBottom);

    QVERIFY(m->closeDockWidgets(true));
    QCOMPARE(m->layoutWidget()->visibleCount(), 0);
}

void TestDocks::tst_layoutEqually()
{
    EnsureTopLevelsDeleted e;

    const QString mainWindowId = "{7829427d-88e3-402e-9120-50c628dfd0bc}";
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None, mainWindowId);
    m->setAffinities({ mainWindowId });

    auto dock1 = createDockWidget("Favorite-481", new MyWidget2(QSize(536, 438)));
    auto dock2 = createDockWidget("Favorite-482", new MyWidget2(QSize(229, 118)));
    auto dock3 = createDockWidget("Favorite-483", new MyWidget2(QSize(356, 90)));
#ifdef KDDOCKWIDGETS_QTWIDGETS
    m->setContentsMargins(10, 0, 10, 0);
#endif
    dock1->setAffinities({ mainWindowId });
    dock2->setAffinities({ mainWindowId });
    dock3->setAffinities({ mainWindowId });

    LayoutSaver restorer;
    restorer.restoreFromFile(":/layouts/layoutEquallyCrash.json");

    m->layoutEqually();
}

void TestDocks::tst_doubleClose()
{
    EnsureTopLevelsDeleted e;
    {
        // Via close()
        auto dock1 = createDockWidget("hello", Qt::green);
        dock1->close();
        dock1->close();

        delete dock1->window();
    }
    {
        // Via the button
        auto dock1 = createDockWidget("hello", Qt::green);
        auto fw1 = dock1->floatingWindow();

        auto t = dock1->dptr()->frame()->titleBar();
        t->onCloseClicked();
        t->onCloseClicked();

        delete dock1;
        delete fw1;
    }
    {
        // Test for #141, double delete would ruin lastPositions()
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow();
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        m->addDockWidget(dock1, Location_OnBottom);

        QVERIFY(!dock1->dptr()->lastPositions().wasFloating());
        dock1->close();
        QVERIFY(!dock1->dptr()->lastPositions().wasFloating());
        dock1->close();
        QVERIFY(!dock1->dptr()->lastPositions().wasFloating());
    }
}

void TestDocks::tst_dockInternal()
{
    /**
     * Here we dock relative to an existing widget, and not to the drop-area.
     */
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dropArea = m->dropArea();

    auto centralWidget = static_cast<Frame*>(dropArea->items()[0]->guestAsQObject());
    nestDockWidget(dock1, dropArea, centralWidget, KDDockWidgets::Location_OnRight);

    QVERIFY(dock1->width() < dropArea->width() - centralWidget->width());
}

void TestDocks::tst_maximizeAndRestore()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));

    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);

    auto dropArea = m->dropArea();
    QVERIFY(dropArea->checkSanity());

    m->showMaximized();
    Testing::waitForResize(m.get());

    QVERIFY(dropArea->checkSanity());
    m->showNormal();
    Testing::waitForResize(m.get());

    QVERIFY(dropArea->checkSanity());
}

void TestDocks::tst_propagateResize2()
{
    // |5|1|2|
    // | |3|4|

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnTop);
    m->addDockWidget(dock2, KDDockWidgets::Location_OnRight, dock1);

    auto dock3 = createDockWidget("dock3", new QPushButton("three"));
    auto dock4 = createDockWidget("dock4", new QPushButton("four"));

    m->addDockWidget(dock3, KDDockWidgets::Location_OnBottom);
    m->addDockWidget(dock4, KDDockWidgets::Location_OnRight, dock3);

    auto dock5 = createDockWidget("dock5", new QPushButton("five"));
    m->addDockWidget(dock5, KDDockWidgets::Location_OnLeft);

    auto dropArea = m->dropArea();
    dropArea->checkSanity();
}

void TestDocks::tst_shutdown()
{
    EnsureTopLevelsDeleted e;
    auto dock = createDockWidget("doc1", Qt::green);

    auto m = createMainWindow();
    m->show();
    QVERIFY(QTest::qWaitForWindowActive(m->windowHandle()));
    delete dock->window();
}

#ifdef KDDOCKWIDGETS_QTWIDGETS

void TestDocks::tst_complex()
{
    // Tests some anchors out of bounds I got

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(600, 500), MainWindowOption_None);
    auto layout = m->multiSplitter();
    m->resize(3266, 2239);
    m->show(); // TODO: Remove and see if it crashes

    DockWidgetBase::List docks;

    QVector<KDDockWidgets::Location> locations = {Location_OnLeft, Location_OnLeft, Location_OnLeft,
                                                  Location_OnRight, Location_OnRight, Location_OnRight, Location_OnRight,
                                                  Location_OnBottom, Location_OnBottom, Location_OnBottom, Location_OnBottom, Location_OnBottom,
                                                  Location_OnBottom, Location_OnBottom, Location_OnBottom, Location_OnBottom, Location_OnBottom,
                                                  Location_OnBottom, Location_OnBottom, Location_OnBottom, Location_OnBottom
                                                  };

    QVector<KDDockWidgets::InitialVisibilityOption> options = { InitialVisibilityOption::StartVisible, InitialVisibilityOption::StartVisible,
                                                    InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,
                                                    InitialVisibilityOption::StartVisible,
                                                    InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,
                                                    InitialVisibilityOption::StartVisible, InitialVisibilityOption::StartVisible,
                                                    InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden,InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartHidden
    };

    QVector<bool> floatings =  {true, false, true, false, false, false, false, false, false, false, false, false,
                               true, false, false, true, true, true, true, true, false };

    QVector<QSize> minSizes= {
        QSize(316, 219),
        QSize(355, 237),
        QSize(293, 66),
        QSize(158, 72),
        QSize(30, 141),
        QSize(104, 143),
        QSize(104, 105),
        QSize(84, 341),
        QSize(130, 130),
        QSize(404, 205),
        QSize(296, 177),
        QSize(914, 474),
        QSize(355, 237),
        QSize(104, 104),
        QSize(104, 138),
        QSize(1061, 272),
        QSize(165, 196),
        QSize(296, 177),
        QSize(104, 104),
        QSize(355, 237),
        QSize(104, 138)
    };

    const int num = 21;
    for (int i = 0; i < num; ++i) {
        auto widget = new MyWidget2(minSizes.at(i));
        auto dw = new DockWidgetType(QString::number(i));
        dw->setWidget(widget);
        docks << dw;
    }

    for (int i = 0; i < num; ++i) {
        m->addDockWidget(docks[i], locations[i], nullptr, options[i]);
        layout->checkSanity();
        docks[i]->setFloating(floatings[i]);
        layout->checkSanity();
    }

    m->show();

    // Cleanup
    qDeleteAll(docks);
    qDeleteAll(DockRegistry::self()->frames());
}
#else
void TestDocks::tst_hoverShowsDropIndicators()
{
    // For QtQuick on Windows, there was a bug where drop indicators wouldn't be shown if MainWindowBase
    // wasn't the root item.

    EnsureTopLevelsDeleted e;
    QQmlApplicationEngine engine(":/main2.qml");

    const MainWindowBase::List mainWindows = DockRegistry::self()->mainwindows();
    QCOMPARE(mainWindows.size(), 1);
    MainWindowBase *m = mainWindows.first();

    m->window()->windowHandle()->setPosition(500, 800);

    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));

    auto floatingDockWidget = createDockWidget("floatingDockWidget", new MyWidget2(QSize(400, 400)));

    m->addDockWidget(dock0, Location_OnLeft);

    const QPoint mainWindowCenterPos = m->mapToGlobal(m->geometry().center());

    QTest::qWait(100);

    auto fw = floatingDockWidget->floatingWindow();
    dragFloatingWindowTo(fw, mainWindowCenterPos);

    QCOMPARE(dock0->dptr()->frame()->dockWidgetCount(), 2);
}
#endif

void TestDocks::tst_28NestedWidgets_data()
{
    QTest::addColumn<QVector<DockDescriptor>>("docksToCreate");
    QTest::addColumn<QVector<int>>("docksToHide");

    QVector<DockDescriptor> docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible }
    };
#ifdef KDDOCKWIDGETS_QTWIDGETS
    QTest::newRow("28") << docks << QVector<int>{11, 0};
#endif
    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },

    };

    QVector<int> docksToHide;
    for (int i = 0; i < docks.size(); ++i) {
        docksToHide << i;
    }

    QTest::newRow("anchor_intersection") << docks << docksToHide;

    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
    };
#ifdef KDDOCKWIDGETS_QTWIDGETS
    // 2. Produced valgrind invalid reads while adding
    QTest::newRow("valgrind") << docks << QVector<int>{};
#endif
    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
    };
#ifdef KDDOCKWIDGETS_QTWIDGETS
    QTest::newRow("bug_when_closing") << docks << QVector<int>{}; // Q_ASSERT(!isSquashed())
#endif
    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
    };
#ifdef KDDOCKWIDGETS_QTWIDGETS
    QTest::newRow("bug_when_closing2") << docks << QVector<int>{};    // Tests for void KDDockWidgets::Anchor::setPosition(int, KDDockWidgets::Anchor::SetPositionOptions) Negative position -69
#endif
    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, 0, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible }
    };

    docksToHide.clear();
    for (int i = 0; i < 28; ++i) {
        if (i != 16 && i != 17 && i != 18 && i != 27)
            docksToHide << i;
    }
#ifdef KDDOCKWIDGETS_QTWIDGETS
    QTest::newRow("bug_with_holes") << docks << docksToHide;
#endif
    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnLeft, 17, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    docksToHide.clear();
    QTest::newRow("add_as_placeholder") << docks << docksToHide;

    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden } };

    QTest::newRow("add_as_placeholder_simple") << docks << docksToHide;


    docks = {
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden } };

    docksToHide.clear();
    QTest::newRow("isSquashed_assert") << docks << docksToHide;

    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden } };

    docksToHide.clear();
    QTest::newRow("negative_pos_warning") << docks << docksToHide;

    docks = {
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    docksToHide.clear();
    QTest::newRow("bug") << docks << docksToHide;

    docks = {
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    docksToHide.clear();
    QTest::newRow("bug2") << docks << docksToHide;

    docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    docksToHide.clear();
    QTest::newRow("bug3") << docks << docksToHide;
}

void TestDocks::tst_28NestedWidgets()
{
    QFETCH(QVector<DockDescriptor>, docksToCreate);
    QFETCH(QVector<int>, docksToHide);

    // Tests a case that used to cause negative anchor position when turning into placeholder
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    int i = 0;
    for (DockDescriptor &desc : docksToCreate) {
        desc.createdDock = createDockWidget(QString("%1").arg(i), new QPushButton(QString("%1").arg(i).toLatin1()), {}, {}, false);

        DockWidgetBase *relativeTo = nullptr;
        if (desc.relativeToIndex != -1)
            relativeTo = docksToCreate.at(desc.relativeToIndex).createdDock;
        m->addDockWidget(desc.createdDock, desc.loc, relativeTo, desc.option);
        QVERIFY(layout->checkSanity());
        ++i;
    }

    layout->checkSanity();

    // Run the saver in these complex scenarios:
    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    QVERIFY(!saved.isEmpty());
    QVERIFY(saver.restoreLayout(saved));

    layout->checkSanity();

    for (int i : docksToHide) {
        docksToCreate.at(i).createdDock->close();
        layout->checkSanity();
        QTest::qWait(200);
    }

    layout->checkSanity();

    for (int i : docksToHide) {
        docksToCreate.at(i).createdDock->deleteLater();
        QVERIFY(Testing::waitForDeleted(docksToCreate.at(i).createdDock));
    }

    layout->checkSanity();

    // And hide the remaining ones
    i = 0;
    for (auto dock : docksToCreate) {
        if (dock.createdDock && dock.createdDock->isVisible()) {
            dock.createdDock->close();
            QTest::qWait(200); // Wait for the docks to be closed. TODO Replace with a global event filter and wait for any resize ?
        }
        ++i;
    }

    layout->checkSanity();

    // Cleanup
    for (auto dock : DockRegistry::self()->dockwidgets()) {
        dock->deleteLater();
        QVERIFY(Testing::waitForDeleted(dock));
    }
}

void TestDocks::tst_closeReparentsToNull()
{
    EnsureTopLevelsDeleted e;
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto fw1 = dock1->window();
    QVERIFY(dock1->parent() != nullptr);
    dock1->close();
    QVERIFY(dock1->parent() == nullptr);
    delete fw1;
    delete dock1;
}

void TestDocks::tst_startHidden()
{
    // A really simple test for InitialVisibilityOption::StartHidden

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"), {}, {}, /*show=*/false);
    m->addDockWidget(dock1, Location_OnRight, nullptr, InitialVisibilityOption::StartHidden);
    delete dock1;
}

void TestDocks::tst_startHidden2()
{
    EnsureTopLevelsDeleted e;
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"), {}, {}, false);
        auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, {}, false);

        auto dropArea = m->dropArea();
        MultiSplitter *layout = dropArea;

        m->addDockWidget(dock1, Location_OnTop, nullptr, InitialVisibilityOption::StartHidden);
        QVERIFY(layout->checkSanity());

        QCOMPARE(layout->count(), 1);
        QCOMPARE(layout->placeholderCount(), 1);

        m->addDockWidget(dock2, Location_OnTop);
        QVERIFY(layout->checkSanity());

        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 1);

        qDebug() << dock1->isVisible();
        dock1->show();

        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 0);

        Testing::waitForResize(dock2);
    }

    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"), {}, {}, false);
        auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, {}, false);
        auto dock3 = createDockWidget("dock3", new QPushButton("three"), {}, {}, false);

        auto dropArea = m->dropArea();
        MultiSplitter *layout = dropArea;
        m->addDockWidget(dock1, Location_OnLeft, nullptr, InitialVisibilityOption::StartHidden);

        m->addDockWidget(dock2, Location_OnBottom, nullptr, InitialVisibilityOption::StartHidden);
        m->addDockWidget(dock3, Location_OnRight, nullptr, InitialVisibilityOption::StartHidden);

        dock1->show();

        QCOMPARE(layout->count(), 3);
        QCOMPARE(layout->placeholderCount(), 2);

        dock2->show();
        dock3->show();
        Testing::waitForResize(dock2);
        layout->checkSanity();
    }
}

void TestDocks::tst_negativeAnchorPosition()
{
    // Tests that we don't hit:
    // void KDDockWidgets::Anchor::setPosition(int, KDDockWidgets::Anchor::SetPositionOptions) Negative position

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(1002, 806));

    auto w1 = new MyWidget2(QSize(104, 104));
    w1->resize(994, 718);
    auto w2 = new MyWidget2(QSize(133, 343));
    w2->resize(392, 362);
    auto w3 = new MyWidget2(QSize(133, 343));
    w3->resize(392, 362);

    MultiSplitter *layout = m->multiSplitter();

    auto d1 = createDockWidget("1", w1);
    auto d2 = createDockWidget("2", w2);
    auto d3 = createDockWidget("3", w3);

    m->addDockWidgetAsTab(d1);

    m->addDockWidget(d2, Location_OnTop);
    m->addDockWidget(d3, Location_OnTop);

    d2->close();

    Testing::waitForResize(d3);
    d2->show(); // Should not result in negative anchor positions (Test will fail due to a qWarning)
    Testing::waitForResize(d3);
    layout->checkSanity();

    d2->close();
    Testing::waitForResize(d3);
    layout->checkSanity();

    // Now resize the Window, after removing middle one
    const int availableToShrink = layout->size().height() - layout->minimumSize().height();
    const QSize newSize = { layout->width(), layout->height() - availableToShrink };
    if (layout->layoutMinimumSize().expandedTo(newSize) != newSize) {
        qDebug() << "Size to set is too small=" << newSize
                 << "; min=" << layout->layoutMinimumSize();
        QFAIL("");
    }

    layout->setLayoutSize(newSize);

    d2->deleteLater();
    Testing::waitForDeleted(d2);
    layout->checkSanity();
}

void TestDocks::tst_negativeAnchorPosition2()
{
    // Tests that the "Out of bounds position" warning doesn't appear. Test will abort if yes.
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    auto dock1 = createDockWidget("1", new QPushButton("1"), {}, {}, /*show=*/false);
    auto dock2 = createDockWidget("2", new QPushButton("2"), {}, {}, /*show=*/false);
    auto dock3 = createDockWidget("3", new QPushButton("3"), {}, {}, /*show=*/false);

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight, nullptr, InitialVisibilityOption::StartHidden);
    m->addDockWidget(dock3, Location_OnRight);
    QCOMPARE(layout->placeholderCount(), 1);
    QCOMPARE(layout->count(), 3);

    dock1->setFloating(true);
    dock1->setFloating(false);
    dock2->deleteLater();
    layout->checkSanity();
    QVERIFY(Testing::waitForDeleted(dock2));
}

void TestDocks::tst_negativeAnchorPosition3()
{
    // 1. Another case, when floating a dock:
    EnsureTopLevelsDeleted e;
    QVector<DockDescriptor> docks = { {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
                                     {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
                                     {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
                                     {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
                                     {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };
    auto m = createMainWindow(docks);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;
    layout->checkSanity();

    auto dock1 = docks.at(1).createdDock;
    auto dock3 = docks.at(3).createdDock;

    dock1->setFloating(true);
    delete dock1->window();
    delete dock3->window();

    layout->checkSanity();
}

void TestDocks::tst_negativeAnchorPosition4()
{
    // 1. Tests that we don't get a warning
    // Out of bounds position= -5 ; oldPosition= 0 KDDockWidgets::Anchor(0x55e726be9090, name = "left") KDDockWidgets::MainWindow(0x55e726beb8d0)
    EnsureTopLevelsDeleted e;
    QVector<DockDescriptor> docks = { { Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
                                      { Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
                                      { Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
                                      { Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
                                      { Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    auto m = createMainWindow(docks);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;
    layout->checkSanity();

    auto dock1 = docks.at(1).createdDock;
    auto dock2 = docks.at(2).createdDock;
    dock2->setFloating(true);
    auto fw2 = dock2->floatingWindow();
    dropArea->addWidget(fw2->dropArea(), Location_OnLeft, dock1->dptr()->frame());
    dock2->setFloating(true);
    fw2 = dock2->floatingWindow();

    dropArea->addWidget(fw2->dropArea(), Location_OnRight, dock1->dptr()->frame());

    layout->checkSanity();
    docks.at(0).createdDock->deleteLater();
    docks.at(4).createdDock->deleteLater();
    Testing::waitForDeleted(docks.at(4).createdDock);
}

void TestDocks::tst_negativeAnchorPosition5()
{
    EnsureTopLevelsDeleted e;
    QVector<DockDescriptor> docks = {
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        };

    auto m = createMainWindow(docks);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;
    layout->checkSanity();

    auto dock0 = docks.at(0).createdDock;
    auto dock1 = docks.at(1).createdDock;

    dock1->show();
    QVERIFY(layout->checkSanity());
    dock0->show();
    QVERIFY(layout->checkSanity());

    // Cleanup
    for (auto dock : DockRegistry::self()->dockwidgets())
        dock->deleteLater();

    QVERIFY(Testing::waitForDeleted(dock0));
}

void TestDocks::tst_negativeAnchorPosition6()
{
    // Tests a case when we add a widget to left/right but the layout doesn't have enough height (or vice-versa)
    EnsureTopLevelsDeleted e;

    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    m->resize(QSize(100, 100));
    m->show();

    auto layout = m->multiSplitter();

    auto w1 = new MyWidget2(QSize(400,100));
    auto w2 = new MyWidget2(QSize(400,100));
    auto w3 = new MyWidget2(QSize(400,100));
    auto w4 = new MyWidget2(QSize(400,900));
    auto d1 = createDockWidget("1", w1);
    auto d2 = createDockWidget("2", w2);
    auto d3 = createDockWidget("3", w3);
    auto d4 = createDockWidget("4", w4);

    m->addDockWidget(d1, Location_OnBottom);
    m->addDockWidget(d2, Location_OnBottom);
    m->addDockWidget(d3, Location_OnBottom);

    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 0);

    m->addDockWidget(d4, Location_OnRight, d3);

    layout->checkSanity();

    Item *centralItem = m->dropArea()->centralFrame();
    layout->rectForDrop(nullptr, Location_OnTop, centralItem);
    layout->checkSanity();
}

void TestDocks::tst_negativeAnchorPosition7()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    m->show();
    auto w1 = new MyWidget2(QSize(400,400));
    auto w2 = new MyWidget2(QSize(400,400));

    auto d1 = new DockWidgetType("1");
    d1->setWidget(w1);
    auto d2 = new DockWidgetType("2");
    d2->setWidget(w2);

    auto w3 = new MyWidget2(QSize(100,100));
    auto d3 = new DockWidgetType("3");
    d3->setWidget(w3);

    // Stack 1, 2
    m->addDockWidget(d2, Location_OnTop);
    m->addDockWidget(d1, Location_OnTop);

    // add a small one to the middle

    // Stack: 1, 3, 2
    m->addDockWidget(d3, Location_OnTop, d2);
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_invalidAnchorGroup()
{
    // Tests a bug I got. Should not warn.
    EnsureTopLevelsDeleted e;

    {
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        QPointer<FloatingWindow> fw = dock2->dptr()->morphIntoFloatingWindow();
        nestDockWidget(dock1, fw->dropArea(), nullptr, KDDockWidgets::Location_OnTop);

        dock1->close();
        Testing::waitForResize(dock2);
        auto layout = fw->dropArea();
        layout->checkSanity();

        dock2->close();
        dock1->deleteLater();
        dock2->deleteLater();
        Testing::waitForDeleted(dock1);
    }

    {
        // Stack 1, 2, 3, close 2, close 1

        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dock3 = createDockWidget("dock3", new QPushButton("three"));

        m->addDockWidget(dock3, Location_OnTop);
        m->addDockWidget(dock2, Location_OnTop);
        m->addDockWidget(dock1, Location_OnTop);

        dock2->close();
        dock1->close();

        dock1->deleteLater();
        dock2->deleteLater();
        Testing::waitForDeleted(dock1);
    }
}

void TestDocks::tst_addAsPlaceholder()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new QPushButton("one"), {}, {}, false);
    auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, {}, false);

    m->addDockWidget(dock1, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnTop, nullptr, InitialVisibilityOption::StartHidden);

    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);
    QVERIFY(!dock2->isVisible());

    dock2->show();
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 0);

    layout->checkSanity();

    // Cleanup
    dock2->deleteLater();
    Testing::waitForDeleted(dock2);
}

void TestDocks::tst_removeItem()
{
    // Tests that MultiSplitterLayout::removeItem() works
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, {}, false);
    auto dock3 = createDockWidget("dock3", new QPushButton("three"));

    m->addDockWidget(dock1, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnTop, nullptr, InitialVisibilityOption::StartHidden);
    Item *item2 = dock2->dptr()->lastPositions().lastItem();

    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);

    // 1. Remove an item that's a placeholder

    layout->removeItem(item2);

    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    // 2. Remove an item that has an actual widget
    Item *item1 = dock1->dptr()->lastPositions().lastItem();
    layout->removeItem(item1);
    QCOMPARE(layout->count(), 0);
    QCOMPARE(layout->placeholderCount(), 0);

    // 3. Remove an item that has anchors following one of its other anchors (Tests that anchors stop following)
    // Stack 1, 2, 3
    m->addDockWidget(dock3, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnBottom);
    m->addDockWidget(dock1, Location_OnBottom);
    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 0);

    dock2->close();
    auto frame1 = dock1->dptr()->frame();
    dock1->close();
    QVERIFY(Testing::waitForDeleted(frame1));

    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 2);

    // Now remove the items
    layout->removeItem(dock2->dptr()->lastPositions().lastItem());
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);
    layout->checkSanity();
    layout->removeItem(dock1->dptr()->lastPositions().lastItem());
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    // Add again
    m->addDockWidget(dock2, Location_OnBottom);
    m->addDockWidget(dock1, Location_OnBottom);
    dock2->close();
    frame1 = dock1->dptr()->frame();
    dock1->close();
    QVERIFY(Testing::waitForDeleted(frame1));

    // Now remove the items, but first dock1
    layout->removeItem(dock1->dptr()->lastPositions().lastItem());
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);
    layout->checkSanity();
    layout->removeItem(dock2->dptr()->lastPositions().lastItem());
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);
    layout->checkSanity();

    // Add again, stacked as 1, 2, 3, then close 2 and 3.
    m->addDockWidget(dock2, Location_OnTop);
    m->addDockWidget(dock1, Location_OnTop);

    auto frame2 = dock2->dptr()->frame();
    dock2->close();
    Testing::waitForDeleted(frame2);

    auto frame3 = dock3->dptr()->frame();
    dock3->close();
    Testing::waitForDeleted(frame3);

    // The second anchor is now following the 3rd, while the 3rd is following 'bottom'
    layout->removeItem(dock3->dptr()->lastPositions().lastItem()); // will trigger the 3rd anchor to
                                                                   // be removed
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);
    layout->checkSanity();

    dock1->deleteLater();
    dock2->deleteLater();
    dock3->deleteLater();
    Testing::waitForDeleted(dock3);
}

void TestDocks::tst_clear()
{
    // Tests MultiSplitterLayout::clear()
    EnsureTopLevelsDeleted e;
    QCOMPARE(Frame::dbg_numFrames(), 0);

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dock3 = createDockWidget("3", new QPushButton("3"));
    auto fw3 = dock3->floatingWindow();

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);
    m->addDockWidget(dock3, Location_OnRight);
    QVERIFY(Testing::waitForDeleted(fw3));
    dock3->close();

    QCOMPARE(Frame::dbg_numFrames(), 3);

    auto layout = m->multiSplitter();
    layout->clearLayout();

    QCOMPARE(layout->count(), 0);
    QCOMPARE(layout->placeholderCount(), 0);
    layout->checkSanity();

    // Cleanup
    dock3->deleteLater();
    QVERIFY(Testing::waitForDeleted(dock3));
}

void TestDocks::tst_samePositionAfterHideRestore()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dock3 = createDockWidget("3", new QPushButton("3"));

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);
    m->addDockWidget(dock3, Location_OnRight);
    QRect geo2 = dock2->dptr()->frame()->QWidgetAdapter::geometry();
    dock2->setFloating(true);

    auto fw2 = dock2->floatingWindow();
    dock2->setFloating(false);
    QVERIFY(Testing::waitForDeleted(fw2));
    QCOMPARE(geo2, dock2->dptr()->frame()->QWidgetAdapter::geometry());
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_startClosed()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));

    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    m->addDockWidget(dock1, Location_OnTop);
    Frame *frame1 = dock1->dptr()->frame();
    dock1->close();
    Testing::waitForDeleted(frame1);

    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 1);

    m->addDockWidget(dock2, Location_OnTop);

    layout->checkSanity();

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);

    dock1->show();
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 0);
}

void TestDocks::tst_dockDockWidgetNested()
{
    EnsureTopLevelsDeleted e;
    // Test detaching too, and check if the window size is correct
    // TODO
}

void TestDocks::tst_dockFloatingWindowNested()
{
    EnsureTopLevelsDeleted e;
    // TODO
}

void TestDocks::tst_crash()
{
     // tests some crash I got

    EnsureTopLevelsDeleted e;

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    auto layout = m->multiSplitter();

    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    Item *item1 = layout->itemForFrame(dock1->dptr()->frame());
    dock1->addDockWidgetAsTab(dock2);

    QVERIFY(!dock1->isFloating());
    dock1->setFloating(true);
    QVERIFY(dock1->isFloating());
    QVERIFY(!dock1->isInMainWindow());

    Item *layoutItem = dock1->dptr()->lastPositions().lastItem();
    QVERIFY(layoutItem && DockRegistry::self()->itemIsInMainWindow(layoutItem));
    QCOMPARE(layoutItem, item1);

    QCOMPARE(layout->placeholderCount(), 0);
    QCOMPARE(layout->count(), 1);

    // Move from tab to bottom
    m->addDockWidget(dock2, KDDockWidgets::Location_OnBottom);

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);

    delete dock1->window();
}

void TestDocks::tst_refUnrefItem()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("1"));
    auto dock2 = createDockWidget("dock2", new QPushButton("2"));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);
    auto dropArea = m->dropArea();
    auto layout = dropArea;
    QPointer<Frame> frame1 = dock1->dptr()->frame();
    QPointer<Frame> frame2 = dock2->dptr()->frame();
    QPointer<Item> item1 = layout->itemForFrame(frame1);
    QPointer<Item> item2 = layout->itemForFrame(frame2);
    QVERIFY(item1.data());
    QVERIFY(item2.data());
    QCOMPARE(item1->refCount(), 2); // 2 - the item and its frame, which can be persistent
    QCOMPARE(item2->refCount(), 2);

    // 1. Delete a dock widget directly. It should delete its frame and also the Item
    delete dock1;
    Testing::waitForDeleted(frame1);
    QVERIFY(!frame1.data());
    QVERIFY(!item1.data());

    // 2. Delete dock3, but neither the frame or the item is deleted, since there were two tabs to begin with
    auto dock3 = createDockWidget("dock3", new QPushButton("3"));
    QCOMPARE(item2->refCount(), 2);
    dock2->addDockWidgetAsTab(dock3);
    QCOMPARE(item2->refCount(), 3);
    delete dock3;
    QVERIFY(item2.data());
    QCOMPARE(frame2->dockWidgets().size(), 1);

    // 3. Close dock2. frame2 should be deleted, but item2 preserved.
    QCOMPARE(item2->refCount(), 2);
    dock2->close();
    Testing::waitForDeleted(frame2);
    QVERIFY(dock2);
    QVERIFY(item2.data());
    QCOMPARE(item2->refCount(), 1);
    QCOMPARE(dock2->dptr()->lastPositions().lastItem(), item2.data());
    delete dock2;

    QVERIFY(!item2.data());
    QCOMPARE(layout->count(), 1);

    // 4. Move a closed dock widget from one mainwindow to another
    // It should delete its old placeholder
    auto dock4 = createDockWidget("dock4", new QPushButton("4"));
    m->addDockWidget(dock4, KDDockWidgets::Location_OnLeft);

    QPointer<Frame> frame4 = dock4->dptr()->frame();
    QPointer<Item> item4 = layout->itemForFrame(frame4);
    dock4->close();
    Testing::waitForDeleted(frame4);
    QCOMPARE(item4->refCount(), 1);
    QVERIFY(item4->isPlaceholder());
    layout->checkSanity();

    auto m2 = createMainWindow();
    m2->addDockWidget(dock4, KDDockWidgets::Location_OnLeft);
    m2->layoutWidget()->checkSanity();
    QVERIFY(!item4.data());
}

void TestDocks::tst_placeholderCount()
{
    EnsureTopLevelsDeleted e;
    // Tests MultiSplitterLayout::count(),visibleCount() and placeholdercount()

    // 1. MainWindow with just the initial frame.
    auto m = createMainWindow();
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dropArea = m->dropArea();
    auto layout = dropArea;

    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->visibleCount(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    // 2. MainWindow with central frame and left widget
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 2);
    QCOMPARE(layout->placeholderCount(), 0);

    // 3. Add another dockwidget, this time tabbed in the center. It won't increase count, as it reuses an existing frame.
    m->addDockWidgetAsTab(dock2);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 2);
    QCOMPARE(layout->placeholderCount(), 0);

    // 4. Float dock1. It should create a placeholder
    dock1->setFloating(true);

    auto fw = dock1->floatingWindow();

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 1);
    QCOMPARE(layout->placeholderCount(), 1);

    // 5. Re-dock dock1. It should reuse the placeholder
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 2);
    QCOMPARE(layout->placeholderCount(), 0);

    // 6. Again
    dock1->setFloating(true);
    fw = dock1->floatingWindow();
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 2);
    QCOMPARE(layout->placeholderCount(), 0);
    layout->checkSanity();

    Testing::waitForDeleted(fw);
}

void TestDocks::tst_availableLengthForOrientation()
{
    EnsureTopLevelsDeleted e;

    // 1. Test a completely empty window, it's available space is its size minus the static separators thickness
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    int availableWidth = layout->availableLengthForOrientation(Qt::Horizontal);
    int availableHeight = layout->availableLengthForOrientation(Qt::Vertical);
    QCOMPARE(availableWidth, layout->width());
    QCOMPARE(availableHeight, layout->height());

    //2. Now do the same, but we have some widget docked

    auto dock1 = createDockWidget("dock1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);

    const int dock1MinWidth =
        layout->itemForFrame(dock1->dptr()->frame())->minLength(Qt::Horizontal);
    const int dock1MinHeight =
        layout->itemForFrame(dock1->dptr()->frame())->minLength(Qt::Vertical);

    availableWidth = layout->availableLengthForOrientation(Qt::Horizontal);
    availableHeight = layout->availableLengthForOrientation(Qt::Vertical);
    QCOMPARE(availableWidth, layout->width() - dock1MinWidth);
    QCOMPARE(availableHeight, layout->height() - dock1MinHeight);
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_closeShowWhenNoCentralFrame()
{
    EnsureTopLevelsDeleted e;
    // Tests a crash I got when hiding and showing and no central frame

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
    QPointer<DockWidgetBase> dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);
    dock1->close();
    m->layoutWidget()->checkSanity();

    QVERIFY(!dock1->dptr()->frame());
    QVERIFY(!Testing::waitForDeleted(dock1)); // It was being deleted due to a bug
    QVERIFY(dock1);
    dock1->show();
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_setAsCurrentTab()
{
    EnsureTopLevelsDeleted e;

    // Tests DockWidget::setAsCurrentTab() and DockWidget::isCurrentTab()
    // 1. a single dock widget is current, by definition
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    QVERIFY(dock1->isCurrentTab());

    // 2. Tab dock2 to the group, dock2 is current now
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    dock1->addDockWidgetAsTab(dock2);

    QVERIFY(!dock1->isCurrentTab());
    QVERIFY(dock2->isCurrentTab());

    // 3. Set dock1 as current
    dock1->setAsCurrentTab();
    QVERIFY(dock1->isCurrentTab());
    QVERIFY(!dock2->isCurrentTab());

    auto fw = dock1->floatingWindow();
    QVERIFY(fw);
    fw->layoutWidget()->checkSanity();

    delete dock1;
    delete dock2;
    Testing::waitForDeleted(fw);
}

void TestDocks::tst_placeholderDisappearsOnReadd()
{
    // This tests that addMultiSplitter also updates refcount of placeholders

    // 1. Detach a widget and dock it on the opposite side. Placeholder
    // should have been deleted and anchors properly positioned

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
    MultiSplitter *layout = m->multiSplitter();

    QPointer<DockWidgetBase> dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    dock1->setFloating(true);
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 1);

    dock1->dptr()->morphIntoFloatingWindow();
    auto fw = dock1->floatingWindow();
    layout->addMultiSplitter(fw->dropArea(), Location_OnRight );

    QCOMPARE(layout->placeholderCount(), 0);
    QCOMPARE(layout->count(), 1);

    layout->checkSanity();
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    // The dock1 should occupy the entire width
    QCOMPARE(dock1->dptr()->frame()->width(), layout->width());

    QVERIFY(Testing::waitForDeleted(fw));
}

void TestDocks::tst_placeholdersAreRemovedProperly()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
    MultiSplitter *layout = m->multiSplitter();
    QPointer<DockWidgetBase> dock1 = createDockWidget("1", new QPushButton("1"));
    QPointer<DockWidgetBase> dock2 = createDockWidget("2", new QPushButton("2"));
    m->addDockWidget(dock1, Location_OnLeft);
    Item *item = layout->items().constFirst();
    m->addDockWidget(dock2, Location_OnRight);
    QVERIFY(!item->isPlaceholder());
    dock1->setFloating(true);
    QVERIFY(item->isPlaceholder());

    QCOMPARE(layout->separators().size(), 0);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);
    layout->removeItem(item);
    QCOMPARE(layout->separators().size(), 0);
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);

    // 2. Recreate the placeholder. This time delete the dock widget to see if placeholder is deleted too.
    m->addDockWidget(dock1, Location_OnLeft);
    dock1->setFloating(true);
    auto window1 = Tests::make_qpointer(dock1->window());
    delete dock1;
    QCOMPARE(layout->separators().size(), 0);
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);
    layout->checkSanity();

    delete window1;
}

void TestDocks::tst_floatMaintainsSize()
{
    // Tests that when we make a window float by pressing the float button, it will popup with
    // the same size it had when docked

    EnsureTopLevelsDeleted e;
    auto dw1 = new DockWidgetType("1");
    auto dw2 = new DockWidgetType("2");

    dw1->addDockWidgetToContainingWindow(dw2, Location_OnRight);
    const int oldWidth2 = dw2->width();
    dw1->show();

    dw2->setFloating(true);
    QTest::qWait(100);

    QVERIFY(qAbs(dw2->width() - oldWidth2) < 16); // 15px for margins

    delete dw1->window();
    delete dw2->window();
}

void TestDocks::tst_preferredInitialSize()
{
    EnsureTopLevelsDeleted e;
    auto dw1 = new DockWidgetType("1");
    auto dw2 = new DockWidgetType("2");
    auto m = createMainWindow(QSize(1200, 1200), MainWindowOption_None);

    m->addDockWidget(dw1, Location_OnTop);
    m->addDockWidget(dw2, Location_OnBottom, nullptr, QSize(0, 200));

    QCOMPARE(dw2->dptr()->frame()->height(), 200);
}

void TestDocks::tst_crash2_data()
{
    QTest::addColumn<bool>("show");
    QTest::newRow("true") << true;
    QTest::newRow("false") << false;
}

void TestDocks::tst_crash2()
{
    QFETCH(bool, show);

    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
        auto layout = m->multiSplitter();
        m->setVisible(show);

        DockWidgetBase::List docks;
        const int num = 4;
        for (int i = 0; i < num; ++i)
            docks << new DockWidgetType(QString::number(i));

        QVector<KDDockWidgets::Location> locations = {Location_OnLeft,
                                                      Location_OnRight, Location_OnRight, Location_OnRight};

        QVector<KDDockWidgets::InitialVisibilityOption> options = { InitialVisibilityOption::StartHidden,
                                                        InitialVisibilityOption::StartHidden, InitialVisibilityOption::StartVisible, InitialVisibilityOption::StartHidden};

        QVector<bool> floatings =  {true, false, false, false};

        for (int i = 0; i < num; ++i) {

            m->addDockWidget(docks[i], locations[i], nullptr, options[i]);
            layout->checkSanity();
            docks[i]->setFloating(floatings[i]);
        }

        qDeleteAll(docks);
        qDeleteAll(DockRegistry::self()->frames());
    }

    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
        auto layout = m->multiSplitter();
        m->show();

        const int num = 3;
        DockWidgetBase::List docks;
        for (int i = 0; i < num; ++i)
            docks << new DockWidgetType(QString::number(i));

        QVector<KDDockWidgets::Location> locations = {Location_OnLeft, Location_OnLeft,
                                                      Location_OnRight};

        QVector<KDDockWidgets::InitialVisibilityOption> options = { InitialVisibilityOption::StartVisible, InitialVisibilityOption::StartVisible,
                                                        InitialVisibilityOption::StartHidden};

        QVector<bool> floatings =  {true, false, false};

        for (int i = 0; i < num; ++i) {
            m->addDockWidget(docks[i], locations[i], nullptr, options[i]);
            layout->checkSanity();
            if (i == 2) {
                // Wait for the resizes. This used to make the app crash.
                QTest::qWait(1000);
            }

            docks[i]->setFloating(floatings[i]);
        }
        layout->checkSanity();

        qDeleteAll(docks);
        qDeleteAll(DockRegistry::self()->frames());
    }

}

void TestDocks::tst_closeAllDockWidgets()
{
    EnsureTopLevelsDeleted e;

    auto m = createMainWindow();
    auto dropArea = m->dropArea();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("one"));
    auto dock3 = createDockWidget("dock3", new QPushButton("one"));
    auto dock4 = createDockWidget("dock4", new QPushButton("one"));
    auto dock5 = createDockWidget("dock5", new QPushButton("one"));
    auto dock6 = createDockWidget("dock6", new QPushButton("one"));

    QPointer<FloatingWindow> fw = dock3->dptr()->morphIntoFloatingWindow();

    nestDockWidget(dock4, dropArea, nullptr, KDDockWidgets::Location_OnRight);
    nestDockWidget(dock5, dropArea, nullptr, KDDockWidgets::Location_OnTop);

    const int oldFWHeight = fw->height();
    nestDockWidget(dock6, fw->dropArea(), nullptr, KDDockWidgets::Location_OnTop);

    QVERIFY(oldFWHeight <= fw->height());
    QCOMPARE(fw->frames().size(), 2);

    QCOMPARE(dock3->window(), fw.data());
    QCOMPARE(dock4->window(), m.get());
    QCOMPARE(dock5->window(), m.get());
    QCOMPARE(dock6->window(), fw.data());
    auto layout = m->multiSplitter();
    layout->checkSanity();
    DockRegistry::self()->clear();
    layout->checkSanity();

    Testing::waitForDeleted(fw);
    QVERIFY(!fw);

    QCOMPARE(dock1->window(), dock1);
    QCOMPARE(dock2->window(), dock2);
    QCOMPARE(dock3->window(), dock3);
    QCOMPARE(dock4->window(), dock4);
    QCOMPARE(dock5->window(), dock5);
    QCOMPARE(dock6->window(), dock6);

    QVERIFY(!dock1->isVisible());
    QVERIFY(!dock2->isVisible());
    QVERIFY(!dock3->isVisible());
    QVERIFY(!dock4->isVisible());
    QVERIFY(!dock5->isVisible());
    QVERIFY(!dock6->isVisible());

    delete dock1;
    delete dock2;
    delete dock3;
    delete dock4;
    delete dock5;
    delete dock6;
}

void TestDocks::tst_toggleMiddleDockCrash()
{
    // tests some crash I got

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
    MultiSplitter *layout = m->multiSplitter();
    QPointer<DockWidgetBase> dock1 = createDockWidget("1", new QPushButton("1"));
    QPointer<DockWidgetBase> dock2 = createDockWidget("2", new QPushButton("2"));
    QPointer<DockWidgetBase> dock3 = createDockWidget("3", new QPushButton("3"));

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);
    m->addDockWidget(dock3, Location_OnRight);

    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 0);

    auto frame = dock2->dptr()->frame();
    dock2->close();
    QVERIFY(Testing::waitForDeleted(frame));

    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 1);
    QVERIFY(layout->checkSanity());

    dock2->show();
    layout->checkSanity();
}

void TestDocks::tst_stealFrame()
{
    // Tests using addWidget() with dock widgets which are already in a layout
    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));

    auto m2 = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock3 = createDockWidget("dock3", new QPushButton("three"));
    auto dock4 = createDockWidget("dock4", new QPushButton("four"));

    auto dropArea1 = m1->dropArea();
    auto dropArea2 = m2->dropArea();

    m1->addDockWidget(dock1, Location_OnRight);
    m1->addDockWidget(dock2, Location_OnRight);
    m2->addDockWidget(dock3, Location_OnRight);
    m2->addDockWidget(dock4, Location_OnRight);

    // 1. MainWindow #1 steals a widget from MainWindow2 and vice-versa
    m1->addDockWidget(dock3, Location_OnRight);
    m1->addDockWidget(dock4, Location_OnRight);
    m2->addDockWidget(dock1, Location_OnRight);
    QPointer<Item> item2 = dropArea1->itemForFrame(dock2->dptr()->frame());
    m2->addDockWidget(dock2, Location_OnRight);
    QVERIFY(!item2.data());

    QCOMPARE(dropArea1->count(), 2);
    QCOMPARE(dropArea2->count(), 2);
    QCOMPARE(dropArea1->placeholderCount(), 0);
    QCOMPARE(dropArea2->placeholderCount(), 0);

    // 2. MainWindow #1 steals a widget from MainWindow2 and vice-versa, but adds as tabs
    dock1->addDockWidgetAsTab(dock3);
    QPointer<Frame> f2 = dock2->dptr()->frame();
    dock4->addDockWidgetAsTab(dock2);
    QVERIFY(Testing::waitForDeleted(f2.data()));
    QVERIFY(!f2.data());

    QCOMPARE(dropArea1->count(), 1);
    QCOMPARE(dropArea2->count(), 1);
    QCOMPARE(dropArea1->placeholderCount(), 0);
    QCOMPARE(dropArea2->placeholderCount(), 0);

    // 3. Test stealing a tab from the same tab-widget we're in. Nothing happens
    {
        SetExpectedWarning sew("Already contains KDDockWidgets::DockWidget"); // Suppress the qFatal this time
        dock1->addDockWidgetAsTab(dock3);
        QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
    }

    // 4. Steal from another tab which resides in another Frame, which resides in the same main window
    m1->addDockWidget(dock1, Location_OnTop);
    f2 = dock2->dptr()->frame();
    dock1->addDockWidgetAsTab(dock2);
    QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
    QCOMPARE(dock4->dptr()->frame()->dockWidgetCount(), 1);

    QCOMPARE(dropArea1->count(), 2);
    QCOMPARE(dropArea1->placeholderCount(), 0);

    // 5. And also steal a side-by-side one into the tab
    QPointer<Frame> f4 = dock4->dptr()->frame();
    dock1->addDockWidgetAsTab(dock4);
    QVERIFY(Testing::waitForDeleted(f4.data()));
    QCOMPARE(dropArea1->count(), 1);
    QCOMPARE(dropArea1->placeholderCount(), 0);

    // 6. Steal from tab to side-by-side within the same MainWindow
    m1->addDockWidget(dock1, Location_OnLeft);
    QCOMPARE(dropArea1->count(), 2);
    QCOMPARE(dropArea1->placeholderCount(), 0);

    // 6. side-by-side to side-by-side within same MainWindow
    m2->addDockWidget(dock1, Location_OnRight);
    QCOMPARE(dropArea2->count(), 2);
    QCOMPARE(dropArea2->placeholderCount(), 0);

    {
        SetExpectedWarning sew("Invalid parameters KDDockWidgets::DockWidget"); // Suppress the qFatal this time
        m2->addDockWidget(dock1, Location_OnLeft, dock1);
        QCOMPARE(dropArea2->count(), 2);  // Nothing happened
        QCOMPARE(dropArea2->placeholderCount(), 0);
        QVERIFY(dock1->isVisible());
    }

    QVERIFY(dock1->isVisible());
    m2->addDockWidget(dock1, Location_OnLeft, nullptr); // Should not warn

    QVERIFY(dock1->isVisible());
    QCOMPARE(dropArea2->count(), 2);  // Nothing happened
    QCOMPARE(dropArea2->placeholderCount(), 0);

    m2->addDockWidget(dock1, Location_OnLeft, nullptr);
    QVERIFY(dock1->isVisible());
    QCOMPARE(dropArea2->count(), 2);  // Nothing happened
    QCOMPARE(dropArea2->placeholderCount(), 0);
    dropArea1->checkSanity();
    dropArea2->checkSanity();
}

void TestDocks::tst_setFloatingWhenWasTabbed()
{
    // Tests DockWidget::isTabbed() and DockWidget::setFloating(false|true) when tabbed (it should redock)
    // setFloating(false) for side-by-side is tested in another function

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));

    // 1. Two floating dock widgets. They are floating, not tabbed.
    QVERIFY(!dock1->isTabbed());
    QVERIFY(!dock2->isTabbed());
    QVERIFY(dock1->isFloating());
    QVERIFY(dock2->isFloating());

    // 2. Dock a floating dock into another floating dock. They're not floating anymore, just tabbed.
    dock1->addDockWidgetAsTab(dock2);
    QVERIFY(dock1->isTabbed());
    QVERIFY(dock2->isTabbed());
    QVERIFY(!dock1->isFloating());
    QVERIFY(!dock2->isFloating());

    // 2.1 Set one of them invisible. // Not much will happen, the tab will be still there, just showing an empty space.
    // Users should use close() instead. Tabwidgets control visibility, they hide the widget when it's not the current tab.
    dock2->setVisible(false);
    QVERIFY(dock2->isTabbed());
    QVERIFY(!dock1->isFloating());
    QCOMPARE(dock2->dptr()->frame()->dockWidgetCount(), 2);
    // 3. Set one floating. Now both cease to be tabbed, and both are floating.
    dock1->setFloating(true);
    QVERIFY(dock1->isFloating());
    QVERIFY(dock2->isFloating());
    QVERIFY(!dock1->isTabbed());
    QVERIFY(!dock2->isTabbed());

    // 4. Dock one floating dock into another, side-by-side. They're neither docking or tabbed now.
    dock1->addDockWidgetToContainingWindow(dock2, KDDockWidgets::Location_OnLeft);
    QVERIFY(!dock1->isFloating());
    QVERIFY(!dock2->isFloating());
    QVERIFY(!dock1->isTabbed());
    QVERIFY(!dock2->isTabbed());

    // 5. float one of them, now both are floating, not tabbed anymore.
    dock2->setFloating(true);
    QVERIFY(dock1->isFloating());
    QVERIFY(dock2->isFloating());
    QVERIFY(!dock1->isTabbed());
    QVERIFY(!dock2->isTabbed());

    // 6. With two dock widgets tabbed, detach 1, and reattach it, via DockWidget::setFloating(false)
    m->addDockWidgetAsTab(dock1);
    m->addDockWidgetAsTab(dock2);

    dock2->setFloating(true);
    QVERIFY(dock1->isTabbed());
    QVERIFY(!dock2->isTabbed());
    QVERIFY(!dock1->isFloating());
    QVERIFY(dock2->isFloating());

    QCOMPARE(dock2->dptr()->lastPositions().lastTabIndex(), 1);
    QVERIFY(dock2->dptr()->lastPositions().isValid());
    dock2->setFloating(false);

    QVERIFY(dock1->isTabbed());
    QVERIFY(dock2->isTabbed());
    QVERIFY(!dock1->isFloating());
    QVERIFY(!dock2->isFloating());

    // 7. Call setFloating(true) on an already docked widget
    auto dock3 = createDockWidget("dock3", new QPushButton("three"));
    dock3->setFloating(true);
    dock3->setFloating(true);

    // 8. Tab 3 together, detach the middle one, reattach the middle one, it should go to the middle.
    m->addDockWidgetAsTab(dock3);
    dock2->setFloating(true);
    QVERIFY(dock2->isFloating());
    dock2->setFloating(false);
    QVERIFY(!dock2->isFloating());
    QVERIFY(dock2->isTabbed());
    QCOMPARE(dock2->dptr()->frame()->indexOfDockWidget(dock2), 1);

    // 10. Float dock1, and dock it to main window as tab. This tests Option_AlwaysShowsTabs.
    dock1->setFloating(true);
    dock2->setFloating(true);
    dock3->setFloating(true);

    m->addDockWidgetAsTab(dock1);
    QVERIFY(!dock1->isFloating());
    QVERIFY(dock1->isTabbed());
    dock1->setFloating(true);
    dock1->setFloating(false);
    QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 1);
    // Cleanup
    m->addDockWidgetAsTab(dock2);
    m->addDockWidgetAsTab(dock3);
    m->deleteLater();
    auto window = m.release();
    Testing::waitForDeleted(window);
}

void TestDocks::tst_setFloatingWhenSideBySide()
{
    // Tests DockWidget::setFloating(false|true) when side-by-side (it should put it where it was)
    EnsureTopLevelsDeleted e;

    {
        // 1. Create a MainWindow with two docked dock-widgets, then float the first one.
        auto m = createMainWindow();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);

        QPointer<Frame> frame1 = dock1->dptr()->frame();
        dock1->setFloating(true);
        QVERIFY(dock1->isFloating());
        auto fw = dock1->floatingWindow();
        QVERIFY(fw);

        //2. Put it back, via setFloating(). It should return to its place.
        dock1->setFloating(false);

        QVERIFY(!dock1->isFloating());
        QVERIFY(!dock1->isTabbed());

        Testing::waitForDeleted(fw);
    }

    {
        // 2. Tests a case where restoring a dock widget wouldn't make it use all its available space
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dock3 = createDockWidget("dock3", new QPushButton("three"));
        auto dropArea = m->dropArea();
        MultiSplitter *layout = dropArea;
        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);
        m->addDockWidget(dock3, KDDockWidgets::Location_OnRight);
        auto f2 = dock2->dptr()->frame();
        Item *item2 = layout->itemForFrame(f2);
        QVERIFY(item2);
        dock2->close();
        dock3->close();
        Testing::waitForDeleted(f2);
        dock2->show();
        Testing::waitForResize(dock2);

        QCOMPARE(item2->geometry(), dock2->dptr()->frame()->QWidgetAdapter::geometry());
        layout->checkSanity();

        // Cleanup
        dock3->deleteLater();
        Testing::waitForDeleted(dock3);
    }
}

void TestDocks::tst_dockWindowWithTwoSideBySideFramesIntoCenter()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setInternalFlags(KDDockWidgets::Config::InternalFlag_NoAeroSnap);
    KDDockWidgets::Config::self().setFlags({});

    auto m = createMainWindow();
    auto fw = createFloatingWindow();
    auto dock2 = createDockWidget("doc2", Qt::red);
    nestDockWidget(dock2, fw->dropArea(), nullptr, KDDockWidgets::Location_OnLeft);
    QCOMPARE(fw->frames().size(), 2);
    QVERIFY(fw->dropArea()->checkSanity());

    auto fw2 = createFloatingWindow();
    fw2->move(fw->x() + fw->width() + 100, fw->y());

    // QtQuick is a bit more async than QWidgets. Wait for the move.
    Testing::waitForEvent(fw2->windowHandle(), QEvent::Move);

    auto da2 = fw2->dropArea();
    const QPoint dragDestPos = da2->mapToGlobal(da2->QWidgetAdapter::rect().center());

    dragFloatingWindowTo(fw, dragDestPos);
    QVERIFY(fw2->dropArea()->checkSanity());

    QCOMPARE(fw2->frames().size(), 1);
    auto f2 = fw2->frames().constFirst();
    QCOMPARE(f2->dockWidgetCount(), 3);
    QVERIFY(Testing::waitForDeleted(fw));
    delete fw2;
}

void TestDocks::tst_tabTitleChanges()
{
    // Tests that the tab's title changes if the dock widget's title changes
    EnsureTopLevelsDeleted e;
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto dw2 = new DockWidgetType(QStringLiteral("2"));

    dw1->addDockWidgetAsTab(dw2);

    TabBar *tb = dw1->dptr()->frame()->tabWidget()->tabBar();
    QCOMPARE(tb->text(0), QStringLiteral("1"));
    dw1->setTitle(QStringLiteral("other"));
    QCOMPARE(tb->text(0), QStringLiteral("other"));

    delete dw1->window();
}

void TestDocks::tst_dockWidgetGetsFocusWhenDocked()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_TitleBarIsFocusable);

    // We drag dw2 onto dw2 and drop it

    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto dw2 = new DockWidgetType(QStringLiteral("2"));
    auto le1 = new FocusableWidget();
    auto le2 = new FocusableWidget();
    dw1->setWidget(le1);
    dw2->setWidget(le2);
    dw2->show();
    dw1->show();
    QTest::qWait(200);

    auto fw1 = dw1->floatingWindow();
    QPointer<FloatingWindow> fw2 = dw2->floatingWindow();

    // Focus dock widget 1 first
    QVERIFY(!dw1->isFocused());
    dw1->window()->activateWindow();
    le1->setFocus(Qt::MouseFocusReason);
    QTest::qWait(200);
    QVERIFY(dw1->isFocused());

    QVERIFY(fw1->isActiveWindow());
    dragFloatingWindowTo(fw2, fw1->dropArea(), DropIndicatorOverlayInterface::DropLocation_Left);
    Testing::waitForEvent(fw1, QEvent::WindowActivate);

    /// We dropped into floating window 1, it should still be active
    QVERIFY(fw1->isActiveWindow());

    // DockWidget 2 was dropped, it should now be focused
    QVERIFY(!dw1->isFocused());
    QVERIFY(dw2->isFocused());

    delete fw1;
    delete fw2;
}

void TestDocks::tst_isFocused()
{
    EnsureTopLevelsDeleted e;

    // 1. Create 2 floating windows
    auto dock1 = createDockWidget(QStringLiteral("dock1"), new FocusableWidget());
    auto dock2 = createDockWidget(QStringLiteral("dock2"), new FocusableWidget());

    QTest::qWait(200); // macOS is flaky here, needs dock2 to be shown first before focusing dock1, otherwise dock1 looses again

    dock1->window()->move(400, 200);

    // 2. Raise dock1 and focus its line edit
    dock1->raise();
    dock1->widget()->setFocus(Qt::OtherFocusReason);
    Testing::waitForEvent(dock1->widget(), QEvent::FocusIn);

    QVERIFY(dock1->isFocused());
    QVERIFY(!dock2->isFocused());

    // 3. Raise dock2 and focus its line edit
    dock2->raiseAndActivate();
    if (!dock2->window()->windowHandle()->isActive())
        Testing::waitForEvent(dock2->window()->windowHandle(), QEvent::WindowActivate);

    dock2->widget()->setFocus(Qt::OtherFocusReason);
    Testing::waitForEvent(dock2->widget(), QEvent::FocusIn);

    QVERIFY(!dock1->isFocused());
    QVERIFY(dock2->widget()->hasFocus());
    QVERIFY(dock2->isFocused());

    // 4. Tab dock1, it's current tab now
    auto oldFw1 = dock1->window();
    dock2->addDockWidgetAsTab(dock1);
    delete oldFw1;
    QVERIFY(dock1->isFocused());
    QVERIFY(!dock2->isFocused());

    // 5. Set dock2 as current tab again
    dock2->raise();
    QVERIFY(!dock1->isFocused());
    QVERIFY(dock2->isFocused());

    // 6. Create dock3, focus it
    auto dock3 = createDockWidget(QStringLiteral("dock3"), new FocusableWidget());
    auto oldFw3 = dock3->window();
    dock3->raise();
    dock3->widget()->setFocus(Qt::OtherFocusReason);
    Testing::waitForEvent(dock2->widget(), QEvent::FocusIn);
    QVERIFY(!dock1->isFocused());
    QVERIFY(!dock2->isFocused());
    QVERIFY(dock3->isFocused());

    // 4. Add dock3 to the 1st window, nested, focus 2 again
    dock2->addDockWidgetToContainingWindow(dock3, Location_OnLeft);
    delete oldFw3;
    dock2->raise();
    dock2->widget()->setFocus(Qt::OtherFocusReason);
    Testing::waitForEvent(dock2->widget(), QEvent::FocusIn);
    QVERIFY(!dock1->isFocused());
    QVERIFY(dock2->isFocused());
    QVERIFY(!dock3->isFocused());
    delete dock2->window();
}

void TestDocks::tst_setWidget()
{
    EnsureTopLevelsDeleted e;
    auto dw = new DockWidgetType(QStringLiteral("FOO"));
    auto button1 = new QPushButton("button1");
    auto button2 = new QPushButton("button2");
    dw->setWidget(button1);
    dw->setWidget(button2);
    delete button1;
    delete dw;
}

void TestDocks::tst_floatingLastPosAfterDoubleClose()
{
    EnsureTopLevelsDeleted e;
    auto d1 = new DockWidgetType(QStringLiteral("a"));
    QVERIFY(d1->dptr()->lastPositions().lastFloatingGeometry().isNull());
    QVERIFY(!d1->isVisible());
    d1->close();
    QVERIFY(d1->dptr()->lastPositions().lastFloatingGeometry().isNull());
    delete d1;
}

void TestDocks::tst_0_data()
{
    QTest::addColumn<int>("thickness");
    QTest::newRow("2") << 2;
    QTest::newRow("1") << 1;
    QTest::newRow("0") << 0;
}

void TestDocks::tst_0()
{
    QFETCH(int, thickness);
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setSeparatorThickness(thickness);

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    m->resize(QSize(502, 500));
    m->show();

    auto d1 = createDockWidget("1", new QTextEdit());
    auto d2 = createDockWidget("2", new QTextEdit());

    m->addDockWidget(d1, Location_OnLeft);
    m->addDockWidget(d2, Location_OnRight);
}

void TestDocks::tst_honourGeometryOfHiddenWindow()
{
    EnsureTopLevelsDeleted e;

    auto d1 = new DockWidgetType("1");
    d1->setWidget(new QTextEdit());

    QVERIFY(!d1->isVisible());

    // Clear had a bug where it saved the position of all dock widgets being closed
    DockRegistry::self()->clear();

    const QRect suggestedGeo(150, 150, 250, 250);
    d1->setGeometry(suggestedGeo);

    d1->show();
    Testing::waitForEvent(d1, QEvent::Show);

    QCOMPARE(d1->window()->windowHandle()->geometry(), suggestedGeo);
    delete d1->window();
}

void TestDocks::tst_registry()
{
    EnsureTopLevelsDeleted e;
    auto dr = DockRegistry::self();

    QCOMPARE(dr->dockwidgets().size(), 0);
    auto dw = new DockWidgetType(QStringLiteral("dw1"));
    auto guest = new QWidgetOrQuick();
    dw->setWidget(guest);
    QCOMPARE(dr->dockWidgetForGuest(nullptr), nullptr);
    QCOMPARE(dr->dockWidgetForGuest(guest), dw);
    delete dw;
}

void TestDocks::tst_dockWindowWithTwoSideBySideFramesIntoRight()
{
    EnsureTopLevelsDeleted e;

    auto fw = createFloatingWindow();
    auto dock2 = createDockWidget("doc2", Qt::red);
    nestDockWidget(dock2, fw->dropArea(), nullptr, KDDockWidgets::Location_OnTop); // No we stack on top, unlike in previous test
    QCOMPARE(fw->frames().size(), 2);

    auto fw2 = createFloatingWindow();
    fw2->move(fw->x() + fw->width() + 100, fw->y());

    dragFloatingWindowTo(fw, fw2->dropArea(), DropIndicatorOverlayInterface::DropLocation_Right); // Outer right instead of Left
    QCOMPARE(fw2->frames().size(), 3);
    QVERIFY(fw2->dropArea()->checkSanity());

    fw2->deleteLater();
    Testing::waitForDeleted(fw2);
}

void TestDocks::tst_dockWindowWithTwoSideBySideFramesIntoLeft()
{
    EnsureTopLevelsDeleted e;

    auto fw = createFloatingWindow();
    fw->setObjectName("fw1");

    auto dock2 = createDockWidget("doc2", Qt::red);
    nestDockWidget(dock2, fw->dropArea(), nullptr, KDDockWidgets::Location_OnLeft);
    QCOMPARE(fw->frames().size(), 2);

    auto fw2 = createFloatingWindow();
    fw2->setObjectName("fw2");
    fw2->move(fw->x() + fw->width() + 100, fw->y());

    QVERIFY(fw2->dropArea()->checkSanity());
    dragFloatingWindowTo(fw, fw2->dropArea(), DropIndicatorOverlayInterface::DropLocation_Left);
    QCOMPARE(fw2->frames().size(), 3);

    QVERIFY(fw2->dropArea()->checkSanity());

    ///Cleanup
    fw2->deleteLater();
    Testing::waitForDeleted(fw2);
}

void TestDocks::tst_posAfterLeftDetach()
{
    {
        EnsureTopLevelsDeleted e;
        auto fw = createFloatingWindow();
        auto dock2 = createDockWidget("doc2", Qt::red);
        nestDockWidget(dock2, fw->dropArea(), nullptr, KDDockWidgets::Location_OnRight);
        QVERIFY(fw->dropArea()->checkSanity());
        // When dragging the right one there was a bug where it jumped
        const QPoint globalSrc = dock2->mapToGlobal(QPoint(0, 0));
        const int offset = 10;
        const QPoint globalDest = globalSrc + QPoint(offset, 0);
        QVERIFY(dock2->isVisible());
        drag(dock2, globalDest);
        QVERIFY(fw->dropArea()->checkSanity());
        const QPoint actualEndPos = dock2->mapToGlobal(QPoint(0, 0));
        QVERIFY(actualEndPos.x() - globalSrc.x() < offset + 5); // 5px so we have margin for window system fluctuations. The actual bug was a very big jump like 50px, so a 5 margin is fine to test that the bug doesn't happen

        delete dock2;
        fw->deleteLater();
        Testing::waitForDeleted(fw);
    }

    {
        EnsureTopLevelsDeleted e;
        auto fw = createFloatingWindow();
        auto dock2 = createDockWidget("doc2", Qt::red);
        nestDockWidget(dock2, fw->dropArea(), nullptr, KDDockWidgets::Location_OnRight);
        QVERIFY(fw->dropArea()->checkSanity());

        const int originalX = dock2->mapToGlobal(QPoint(0, 0)).x();
        dock2->dptr()->frame()->titleBar()->makeWindow();
        const int finalX = dock2->mapToGlobal(QPoint(0, 0)).x();

        QVERIFY(finalX - originalX < 10); // 10 or some other small number that is less than say 200

        delete dock2;
        fw->deleteLater();
        Testing::waitForDeleted(fw);
    }
}

void TestDocks::tst_preventClose()
{
    EnsureTopLevelsDeleted e;

    auto nonClosableWidget = new NonClosableWidget();
    auto dock1 = new DockWidgetType("1");
    dock1->setWidget(nonClosableWidget);

    // 1. Test a floating dock widget
    dock1->resize(200, 200);
    dock1->show();
    QVERIFY(dock1->isVisible());
    dock1->close();
    QVERIFY(dock1->isVisible());

    // 2. Morph it into a FlatingWindow
    dock1->dptr()->morphIntoFloatingWindow();
    dock1->close();
    QVERIFY(dock1->isVisible());
    dock1->dptr()->frame()->titleBar()->onCloseClicked();
    QVERIFY(dock1->isVisible());
    auto fw = dock1->floatingWindow();
    fw->close();
    QVERIFY(dock1->isVisible());

    dock1->deleteLater();
    QVERIFY(Testing::waitForDeleted(dock1));
}

void TestDocks::tst_propagateMinSize()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dropArea = m->dropArea();

    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    auto dock3 = createDockWidget("dock3", new QPushButton("three"));

    nestDockWidget(dock1, dropArea, nullptr, KDDockWidgets::Location_OnRight);
    nestDockWidget(dock2, dropArea, nullptr, KDDockWidgets::Location_OnRight);
    nestDockWidget(dock3, dropArea, nullptr, KDDockWidgets::Location_OnRight);

    // TODO finish this when the 3 dock widgets have proper sizes
    //QTest::qWait(50000);

}

void TestDocks::tst_createFloatingWindow()
{
    EnsureTopLevelsDeleted e;

    auto dock = createDockWidget("doc1", Qt::green);
    QVERIFY(dock);
    QVERIFY(dock->isFloating());

    QCOMPARE(dock->uniqueName(), QLatin1String("doc1")); // 1.0 objectName() is inherited

    QPointer<FloatingWindow> window = dock->floatingWindow();
    QVERIFY(window); // 1.1 DockWidget creates a FloatingWindow and is reparented
    QVERIFY(window->dropArea()->checkSanity());
    dock->deleteLater();
    QVERIFY(Testing::waitForDeleted(dock));
    QVERIFY(Testing::waitForDeleted(window)); // 1.2 Floating Window is destroyed when DockWidget is destroyed
    QVERIFY(!window);
}

void TestDocks::tst_addAndReadd()
{
    EnsureTopLevelsDeleted e;

    // 1. This just tests some crash I got.
    // Make a dock widget float and immediately reattach it
    auto m = createMainWindow();

    auto dock1 = createDockWidget("dock1", new QPushButton("1"));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    dock1->setFloating(true);
    m->layoutWidget()->checkSanity();
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    dock1->dptr()->frame()->titleBar()->makeWindow();
    m->layoutWidget()->checkSanity();
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    dock1->dptr()->frame()->titleBar()->makeWindow();
    m->layoutWidget()->checkSanity();

    auto fw = dock1->floatingWindow();
    QVERIFY(fw);
    auto dropArea = m->dropArea();
    dragFloatingWindowTo(fw, dropArea, DropIndicatorOverlayInterface::DropLocation_Right);
    QVERIFY(dock1->dptr()->frame()->titleBar()->isVisible());
    fw->titleBar()->makeWindow();
    m->layoutWidget()->checkSanity();

    //Cleanup
    delete dock1;
    Testing::waitForDeleted(fw);
}

void TestDocks::tst_addToSmallMainWindow1()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new MyWidget2());
    auto dock2 = createDockWidget("dock2", new MyWidget2());
    auto dock3 = createDockWidget("dock3", new MyWidget2());
    auto dock4 = createDockWidget("dock4", new MyWidget2());

    const int mainWindowLength = 400;

    m->windowHandle()->resize(mainWindowLength, mainWindowLength);
    QTest::qWait(100);

    dock1->resize(800, 800);
    dock2->resize(800, 800);
    dock3->resize(800, 800);

    // Add as tabbed:
    m->addDockWidgetAsTab(dock1);

    QCOMPARE(m->height(), mainWindowLength);

    QTest::qWait(300);
    if (dock1->height() > mainWindowLength) {
        qDebug() << "dock1->height=" << dock1->height()
                 << "; mainWindowLength=" << mainWindowLength;
        QVERIFY(false);
    }

    QVERIFY(dock1->width() <= mainWindowLength);

    //Add in area:
    m->addDockWidget(dock2, Location_OnLeft);
    m->addDockWidget(dock3, Location_OnTop, dock2);
    m->addDockWidget(dock4, Location_OnBottom);

    auto dropArea = m->dropArea();

    QVERIFY(dropArea->checkSanity());
    QVERIFY(dock2->width() < mainWindowLength);
    QVERIFY(dock3->height() < m->height());
    QVERIFY(dock4->height() < m->height());
}

void TestDocks::tst_addToSmallMainWindow2()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dropArea = m->dropArea();
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(100, 100)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(100, 100)));
    m->addDockWidgetAsTab(dock1);
    m->windowHandle()->resize(osWindowMinWidth(), 200);

    Testing::waitForResize(m.get());

    QVERIFY(qAbs(m->width() - osWindowMinWidth()) < 15); // Not very important verification. Anyway, using 15 to account for margins and what not.
    m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);
#ifdef KDDOCKWIDGETS_QTWIDGETS
    QVERIFY(Testing::waitForResize(m.get()));
#else
    QTest::qWait(100);
#endif

    QVERIFY(dropArea->width() > osWindowMinWidth());
    QMargins margins = m->centerWidgetMargins();
    QCOMPARE(dropArea->width(), m->width() - margins.left() - margins.right());
    QVERIFY(m->dropArea()->checkSanity());
}

void TestDocks::tst_addToSmallMainWindow3()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dropArea = m->dropArea();
    auto dock1 = createDockWidget("dock1", new MyWidget2());
    auto dock2 = createDockWidget("dock2", new MyWidget2());
    m->addDockWidgetAsTab(dock1);
    m->windowHandle()->resize(osWindowMinWidth(), 200);
    QTest::qWait(200);
    QVERIFY(qAbs(m->width() - osWindowMinWidth()) < 15); // Not very important verification. Anyway, using 15 to account for margins and what not.

    auto fw = dock2->dptr()->morphIntoFloatingWindow();
    QVERIFY(fw->isVisible());
    QVERIFY(dropArea->checkSanity());
    dragFloatingWindowTo(fw, dropArea, DropIndicatorOverlayInterface::DropLocation_Right);
    QVERIFY(m->dropArea()->checkSanity());
    delete fw;
}

void TestDocks::tst_addToSmallMainWindow4()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(100, 100), MainWindowOption_None);

    QTest::qWait(100);
    QCOMPARE(m->height(), 100);

    auto dropArea = m->dropArea();
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(50, 50)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(50, 50)));
    MultiSplitter *layout = dropArea;
    m->addDockWidget(dock1, KDDockWidgets::Location_OnBottom);
    Testing::waitForResize(m.get());

    m->addDockWidget(dock2, KDDockWidgets::Location_OnBottom);
    Testing::waitForResize(m.get());
    QVERIFY(m->dropArea()->checkSanity());

    const int item2MinHeight =
        layout->itemForFrame(dock2->dptr()->frame())->minLength(Qt::Vertical);
    QCOMPARE(dropArea->height(),
             dock1->dptr()->frame()->height() + item2MinHeight + Item::separatorThickness);
}

void TestDocks::tst_addToSmallMainWindow5()
{
    EnsureTopLevelsDeleted e;
    // Test test shouldn't spit any warnings

    auto m = createMainWindow(QSize(100, 100), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(50, 240)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(50, 240)));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnBottom);
    m->addDockWidget(dock2, KDDockWidgets::Location_OnBottom);
    QVERIFY(m->dropArea()->checkSanity());
}

void TestDocks::tst_fairResizeAfterRemoveWidget()
{
    // 1. Add 3 dock widgets horizontally, remove the middle one, make sure
    // both left and right widgets get a share of the new available space

    EnsureTopLevelsDeleted e;

    DockWidgetBase *dock1 = createDockWidget("dock1", new QPushButton("one"));
    DockWidgetBase *dock2 = createDockWidget("dock2", new QPushButton("two"));
    DockWidgetBase *dock3 = createDockWidget("dock3", new QPushButton("three"));

    dock1->addDockWidgetToContainingWindow(dock2, Location_OnRight);
    dock1->addDockWidgetToContainingWindow(dock3, Location_OnRight, dock2);

    auto fw = dock1->floatingWindow();

    QPointer<Frame> frame2 = dock2->dptr()->frame();

    const int oldWidth1 = dock1->dptr()->frame()->width();
    const int oldWidth2 = dock2->dptr()->frame()->width();
    const int oldWidth3 = dock3->dptr()->frame()->width();
    MultiSplitter *layout = fw->dropArea();
    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->visibleCount(), 3);
    QCOMPARE(layout->placeholderCount(), 0);

    delete dock2;
    QVERIFY(Testing::waitForResize(dock1));
    QVERIFY(!frame2);

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->visibleCount(), 2);
    QCOMPARE(layout->placeholderCount(), 0);

    const int delta1 = (dock1->dptr()->frame()->width() - oldWidth1);
    const int delta3 = (dock3->dptr()->frame()->width() - oldWidth3);

    qDebug() << "old1=" << oldWidth1
             << "; old3=" << oldWidth3
             << "; to spread=" << oldWidth2
             << "; Delta1=" << delta1
             << "; Delta3=" << delta3;

    QVERIFY(delta1 > 0);
    QVERIFY(delta3 > 0);
    QVERIFY(qAbs(delta3 - delta1) <= 1); // Both dock1 and dock3 should have increased by the same amount

    delete dock1->window();
}

void TestDocks::tst_invalidJSON_data()
{
    // Be sure that the main windows in the json are called "MyMainWindow1" and the dock widgets
    // dock-x where x starts at 0
    QTest::addColumn<QString>("layoutFileName");
    QTest::addColumn<int>("numDockWidgets");
    QTest::addColumn<QString>("expectedWarning");
    QTest::addColumn<bool>("expectedResult");
    QTest::newRow("unsupported-serialization-version") << "unsupported-serialization-version.json"
                                                       << 10
                                                       << "Serialization format is too old"
                                                       << false;
    QTest::newRow("invalid") << "invalid.json" << 29 << "" << false;
    QTest::newRow("overlapping-item") << "overlapping-item.json" << 2 << "Unexpected pos" << true;
}

void TestDocks::tst_invalidJSON()
{
    QFETCH(QString, layoutFileName);
    QFETCH(int, numDockWidgets);
    QFETCH(QString, expectedWarning);
    QFETCH(bool, expectedResult);

    const QString absoluteLayoutFileName = QStringLiteral(":/layouts/%1").arg(layoutFileName);

    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(800, 500), MainWindowOption_None, "MyMainWindow1");
    for (int i = 0; i < numDockWidgets; ++i) {
        createDockWidget(QStringLiteral("dock-%1").arg(i), new QPushButton("one"));
    }

    SetExpectedWarning sew(expectedWarning);

    LayoutSaver restorer;
    QCOMPARE(restorer.restoreFromFile(absoluteLayoutFileName), expectedResult);
}

void TestDocks::tst_invalidPlaceholderPosition_data()
{
    QTest::addColumn<bool>("restore1First");
    QTest::newRow("restore1First") << true;
    QTest::newRow("restore2First") << false;
}

void TestDocks::tst_invalidPlaceholderPosition()
{
    QFETCH(bool, restore1First);

    // Tests a bug I saw: 3 widgets stacked, close the top one, then the second top one
    // result: the bottom most one didn't have it's top separator at y=0

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dock3 = createDockWidget("3", new QPushButton("3"));

    MultiSplitter *layout = m->multiSplitter();

    // Stack: 1, 2, 3 vertically
    m->addDockWidget(dock3, Location_OnTop);
    m->addDockWidget(dock2, Location_OnTop);
    m->addDockWidget(dock1, Location_OnTop);

    auto frame1 = dock1->dptr()->frame();
    auto frame2 = dock2->dptr()->frame();
    auto frame3 = dock3->dptr()->frame();
    QCOMPARE(frame1->QWidgetAdapter::y(), 0);

    // Close 1
    dock1->close();
    Testing::waitForResize(frame2);

    // Check that frame2 moved up to y=1
    QCOMPARE(frame2->QWidgetAdapter::y(), 0);

    // Close 2
    dock2->close();
    Testing::waitForResize(dock3);

    QVERIFY(layout->checkSanity());
    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 2);

    // Check that frame3 moved up to y=1
    QCOMPARE(frame3->QWidgetAdapter::y(), 0);

    // Now restore:
    auto toRestore1 = restore1First ? dock1 : dock2;
    auto toRestore2 = restore1First ? dock2 : dock1;

    toRestore1->show();
    QCOMPARE(layout->placeholderCount(), 1);
    QVERIFY(dock3->isVisible());
    QVERIFY(!dock3->size().isNull());

    toRestore2->show();

    Testing::waitForResize(frame3);
    QVERIFY(layout->checkSanity());
    QCOMPARE(layout->count(), 3);
    QCOMPARE(layout->placeholderCount(), 0);
    layout->checkSanity();

    dock1->deleteLater();
    dock2->deleteLater();
    QVERIFY(Testing::waitForDeleted(dock2));
}

void TestDocks::tst_setVisibleFalseWhenSideBySide_data()
{
    QTest::addColumn<bool>("useSetVisible");
    QTest::newRow("false") << false;
    // QTest::newRow("true") << true; // We don't support closing dock widgets via setVisible(false). (Yet ? Maybe never).
}

void TestDocks::tst_setVisibleFalseWhenSideBySide()
{
    QFETCH(bool, useSetVisible);

    auto setVisible = [useSetVisible] (DockWidgetBase *dw, bool visible) {
        if (useSetVisible)
            dw->setVisible(visible);
        else if (visible)
            dw->show();
        else
            dw->close();
    };

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);

    const QRect oldGeo = dock1->geometry();
    auto oldParent = dock1->parentWidget();

    // 1. Just toggle visibility and check that stuff remained sane
    QVERIFY(dock1->titleBar()->isVisible());
    setVisible(dock1, false);

    QVERIFY(!dock1->titleBar());
    QVERIFY(!dock1->isTabbed());
    QVERIFY(dock1->isFloating());

    setVisible(dock1, true);
    QVERIFY(dock1->titleBar()->isVisible());
    QVERIFY(!dock1->isTabbed());
    QVERIFY(!dock1->isFloating());
    QCOMPARE(dock1->geometry(), oldGeo);
    QCOMPARE(dock1->parentWidget(), oldParent);

    // 2. Check that the parent frame also is hidden now
    //auto fw1 = dock1->window();
    setVisible(dock1, false);
    QVERIFY(!dock1->dptr()->frame());
    delete dock1;
}

void TestDocks::tst_restoreSimplest()
{
   EnsureTopLevelsDeleted e;
    // Tests restoring a very simple layout, composed of just 1 docked widget
   auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
   auto layout = m->multiSplitter();
   auto dock1 = createDockWidget("one", new QTextEdit());
   m->addDockWidget(dock1, Location_OnTop);

   LayoutSaver saver;
   QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreSimplest.json")));
   QTest::qWait(200);
   QVERIFY(layout->checkSanity());
   QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreSimplest.json")));
   QVERIFY(layout->checkSanity());
}

void TestDocks::tst_restoreNonClosable()
{
    // Tests that restoring state also restores the Option_NotClosable option

    {
        // Basic case:

        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("one", new QTextEdit(), DockWidgetBase::Option_NotClosable);
        QCOMPARE(dock1->options(), DockWidgetBase::Option_NotClosable);

        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();
        QVERIFY(saver.restoreLayout(saved));
        QCOMPARE(dock1->options(), DockWidgetBase::Option_NotClosable);
    }

    {
        // Case from issue #137
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);

        auto dock1 = createDockWidget("1", new QTextEdit());
        auto dock2 = createDockWidget("2", new QTextEdit(), DockWidgetBase::Option_NotClosable);
        auto dock3 = createDockWidget("3", new QTextEdit());

        m->addDockWidget(dock1, Location_OnLeft);
        m->addDockWidget(dock2, Location_OnLeft);
        m->addDockWidget(dock3, Location_OnLeft);

        QCOMPARE(dock2->options(), DockWidgetBase::Option_NotClosable);
        dock2->setFloating(true);
        QCOMPARE(dock2->options(), DockWidgetBase::Option_NotClosable);

        TitleBar *tb = dock2->dptr()->frame()->actualTitleBar();
        QVERIFY(tb->isVisible());
        QVERIFY(!tb->closeButtonEnabled());

        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();

        QVERIFY(saver.restoreLayout(saved));
        QCOMPARE(dock2->options(), DockWidgetBase::Option_NotClosable);

        tb = dock2->dptr()->frame()->actualTitleBar();
        QVERIFY(tb->isVisible());

        QVERIFY(!tb->closeButtonEnabled());
    }
}

void TestDocks::tst_restoreRestoresMainWindowPosition()
{
    // Tests that MainWindow position is restored by LayoutSaver
    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        const QPoint originalPos = m->pos();

        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();

        m->move(originalPos + QPoint(100, 100));

        saver.restoreLayout(saved);
        QCOMPARE(originalPos, m->pos());
    }
#ifdef KDDOCKWIDGETS_QTQUICK
// Tests the ApplicationWindow {} case
    {
        QQmlApplicationEngine engine(":/main2.qml");

        const MainWindowBase::List mainWindows = DockRegistry::self()->mainwindows();
        QCOMPARE(mainWindows.size(), 1);
        MainWindowBase *mainWindow = mainWindows.first();
        QVERIFY(mainWindow->isVisible());

        QCOMPARE(mainWindow->pos(), QPoint(0, 0));

        QWindow *window = mainWindow->windowHandle();

        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();

        const QPoint originalPos = window->position();
        window->setPosition(originalPos + QPoint(200, 200));
        QCOMPARE(window->position(), originalPos + QPoint(200, 200));

        QVERIFY(saver.restoreLayout(saved));

        QCOMPARE(window->position(), originalPos);

        delete mainWindow;
    }
#endif
}

void TestDocks::tst_resizeViaAnchorsAfterPlaceholderCreation()
{
    EnsureTopLevelsDeleted e;

    // Stack 1, 2, 3, close 2, close 2
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        MultiSplitter *layout = m->multiSplitter();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dock3 = createDockWidget("dock3", new QPushButton("three"));
        m->addDockWidget(dock3, Location_OnTop);
        m->addDockWidget(dock2, Location_OnTop);
        m->addDockWidget(dock1, Location_OnTop);
        QCOMPARE(layout->separators().size(), 2);
        dock2->close();
        Testing::waitForResize(dock3);
        QCOMPARE(layout->separators().size(), 1);
        layout->checkSanity();

        // Cleanup:
        dock2->deleteLater();
        Testing::waitForDeleted(dock2);
    }

    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dock3 = createDockWidget("dock3", new QPushButton("three"));
        auto dock4 = createDockWidget("dock4", new QPushButton("four"));
        m->addDockWidget(dock1, Location_OnRight);
        m->addDockWidget(dock2, Location_OnRight);
        m->addDockWidget(dock3, Location_OnRight);
        m->addDockWidget(dock4, Location_OnRight);

        MultiSplitter *layout = m->multiSplitter();

        Item *item1 = layout->itemForFrame(dock1->dptr()->frame());
        Item *item2 = layout->itemForFrame(dock2->dptr()->frame());
        Item *item3 = layout->itemForFrame(dock3->dptr()->frame());
        Item *item4 = layout->itemForFrame(dock4->dptr()->frame());

        const auto separators = layout->separators();
        QCOMPARE(separators.size(), 3);

        Separator *anchor1 = separators[0];
        int boundToTheRight = layout->rootItem()->maxPosForSeparator(anchor1);
        int expectedBoundToTheRight = layout->size().width() -
                                      3*Item::separatorThickness -
                                      item2->minLength(Qt::Horizontal) -
                                      item3->minLength(Qt::Horizontal) -
                                      item4->minLength(Qt::Horizontal);

        QCOMPARE(boundToTheRight, expectedBoundToTheRight);

        dock3->close();
        Testing::waitForResize(dock2);

        QVERIFY(!item1->isPlaceholder());
        QVERIFY(!item2->isPlaceholder());
        QVERIFY(item3->isPlaceholder());
        QVERIFY(!item4->isPlaceholder());

        boundToTheRight = layout->rootItem()->maxPosForSeparator(anchor1);
        expectedBoundToTheRight = layout->size().width() -
                                  2*Item::separatorThickness -
                                  item2->minLength(Qt::Horizontal) -
                                  item4->minLength(Qt::Horizontal) ;

        QCOMPARE(boundToTheRight, expectedBoundToTheRight);
        dock3->deleteLater();
        Testing::waitForDeleted(dock3);
    }
}

void TestDocks::tst_rectForDropCrash()
{
    // Tests a crash I got in MultiSplitterLayout::rectForDrop() (asserts being hit)
    EnsureTopLevelsDeleted e;

    auto m = createMainWindow();
    m->resize(QSize(500, 500));
    m->show();

    auto layout = m->multiSplitter();

    auto w1 = new MyWidget2(QSize(400,400));
    auto w2 = new MyWidget2(QSize(400,400));

    auto d1 = createDockWidget("1", w1);
    auto d2 = createDockWidget("2", w2);

    m->addDockWidget(d1, Location_OnTop);
    Item *centralItem = m->dropArea()->centralFrame();
    {
        WindowBeingDragged wbd2(d2->floatingWindow());
        layout->rectForDrop(&wbd2, Location_OnTop, centralItem);
    }
    layout->checkSanity();
}

void TestDocks::tst_restoreAfterResize()
{
    // Tests a crash I got when the layout received a resize event *while* restoring

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), {}, "tst_restoreAfterResize");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);
    auto layout = m->multiSplitter();
    const QSize oldContentsSize = layout->size();
    const QSize oldWindowSize = m->size();
    LayoutSaver saver;
    QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreAfterResize.json")));
    m->resize(1000, 1000);
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreAfterResize.json")));
    QCOMPARE(oldContentsSize, layout->size());
    QCOMPARE(oldWindowSize, m->size());
}

void TestDocks::tst_restoreWithNonClosableWidget()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), {}, "tst_restoreWithNonClosableWidget");
    auto dock1 = createDockWidget("1", new NonClosableWidget(), DockWidgetBase::Option_NotClosable);
    m->addDockWidget(dock1, Location_OnLeft);
    auto layout = m->multiSplitter();

    LayoutSaver saver;
    QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreWithNonClosableWidget.json")));
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreWithNonClosableWidget.json")));
    QVERIFY(layout->checkSanity());
}

void TestDocks::tst_restoreNestedAndTabbed()
{
    // Just a more involved test

    EnsureTopLevelsDeleted e;
    QPoint oldFW4Pos;
    QRect oldGeo;
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None, "tst_restoreNestedAndTabbed");
        m->move(500, 500);
        oldGeo = m->geometry();
        auto layout = m->multiSplitter();
        auto dock1 = createDockWidget("1", new QTextEdit());
        auto dock2 = createDockWidget("2", new QTextEdit());
        auto dock3 = createDockWidget("3", new QTextEdit());
        auto dock4 = createDockWidget("4", new QTextEdit());
        auto dock5 = createDockWidget("5", new QTextEdit());
        dock4->addDockWidgetAsTab(dock5);
        oldFW4Pos = dock4->window()->pos();

        m->addDockWidget(dock1, Location_OnLeft);
        m->addDockWidget(dock2, Location_OnRight);
        dock2->addDockWidgetAsTab(dock3);
        dock2->setAsCurrentTab();
        QCOMPARE(dock2->dptr()->frame()->currentTabIndex(), 0);
        QCOMPARE(dock4->dptr()->frame()->currentTabIndex(), 1);

        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreNestedAndTabbed.json")));
        QVERIFY(layout->checkSanity());

        delete dock4->window();
        // Let it be destroyed, we'll restore a new one
    }

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None, "tst_restoreNestedAndTabbed");
    auto layout = m->multiSplitter();
    auto dock1 = createDockWidget("1", new QTextEdit());
    auto dock2 = createDockWidget("2", new QTextEdit());
    auto dock3 = createDockWidget("3", new QTextEdit());
    auto dock4 = createDockWidget("4", new QTextEdit());
    auto dock5 = createDockWidget("5", new QTextEdit());

    LayoutSaver saver;
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreNestedAndTabbed.json")));
    QVERIFY(layout->checkSanity());

    auto fw4 = dock4->floatingWindow();
    QVERIFY(fw4);
    QCOMPARE(dock4->window(), dock5->window());
    QCOMPARE(fw4->pos(), oldFW4Pos);

    QCOMPARE(dock1->window(), m.get());
    QCOMPARE(dock2->window(), m.get());
    QCOMPARE(dock3->window(), m.get());

    QCOMPARE(dock2->dptr()->frame()->currentTabIndex(), 0);
    QCOMPARE(dock4->dptr()->frame()->currentTabIndex(), 1);

    QCOMPARE(m->geometry(), oldGeo);
}

void TestDocks::tst_restoreCrash()
{
    EnsureTopLevelsDeleted e;

    {
        // Create a main window, with a left dock, save it to disk.
        auto m = createMainWindow({}, {}, "tst_restoreCrash");
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        m->addDockWidget(dock1, Location_OnLeft);
        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreCrash.json")));
    }

    // Restore
    auto m = createMainWindow({}, {}, "tst_restoreCrash");
    auto layout = m->multiSplitter();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    QVERIFY(dock1->isFloating());
    QVERIFY(layout->checkSanity());

    LayoutSaver saver;
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreCrash.json")));
    QVERIFY(layout->checkSanity());
    QVERIFY(!dock1->isFloating());
}

void TestDocks::tst_restoreSideBySide()
{
    // Save a layout that has a floating window with nesting
    EnsureTopLevelsDeleted e;

    QSize item2MinSize;
    {
        EnsureTopLevelsDeleted e1;
        // MainWindow:
        auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_restoreTwice");
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        m->addDockWidgetAsTab(dock1);
        auto layout = m->multiSplitter();

        // FloatingWindow:
        auto dock2 = createDockWidget("2", new QPushButton("2"));
        auto dock3 = createDockWidget("3", new QPushButton("3"));
        dock2->addDockWidgetToContainingWindow(dock3, Location_OnRight);
        auto fw2 = dock2->floatingWindow();
        item2MinSize = fw2->layoutWidget()->itemForFrame(dock2->dptr()->frame())->minSize();
        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreSideBySide.json")));
        QVERIFY(layout->checkSanity());
    }

    {
        auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_restoreTwice");
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        auto dock2 = createDockWidget("2", new QPushButton("2"));
        auto dock3 = createDockWidget("3", new QPushButton("3"));

        LayoutSaver restorer;
        QVERIFY(restorer.restoreFromFile(QStringLiteral("layout_tst_restoreSideBySide.json")));

        DockRegistry::self()->checkSanityAll();

        QCOMPARE(dock1->window(), m.get());
        QCOMPARE(dock2->window(), dock3->window());
    }
}

void TestDocks::tst_restoreWithCentralFrameWithTabs()
{
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_restoreTwice");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    m->addDockWidgetAsTab(dock1);
    m->addDockWidgetAsTab(dock2);

    QCOMPARE(DockRegistry::self()->frames().size(), 1);

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    QVERIFY(saver.restoreLayout(saved));

    QCOMPARE(DockRegistry::self()->frames().size(), 1);
}

void TestDocks::tst_restoreWithPlaceholder()
{
    // Float dock1, save and restore, then unfloat and see if dock2 goes back to where it was

    EnsureTopLevelsDeleted e;
    {
        auto m = createMainWindow(QSize(500, 500), {}, "tst_restoreWithPlaceholder");

        auto dock1 = createDockWidget("1", new QPushButton("1"));
        m->addDockWidget(dock1, Location_OnLeft);
        auto layout = m->multiSplitter();
        dock1->setFloating(true);

        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_restoreWithPlaceholder.json")));

        dock1->close();

        QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreWithPlaceholder.json")));
        QVERIFY(layout->checkSanity());

        QVERIFY(dock1->isFloating());
        QVERIFY(dock1->isVisible());
        QCOMPARE(layout->count(), 1);
        QCOMPARE(layout->placeholderCount(), 1);

        dock1->setFloating(false); // Put it back. Should go back because the placeholder was restored.

        QVERIFY(!dock1->isFloating());
        QVERIFY(dock1->isVisible());
        QCOMPARE(layout->count(), 1);
        QCOMPARE(layout->placeholderCount(), 0);

    }

    // Try again, but on a different main window
    auto m = createMainWindow(QSize(500, 500), {}, "tst_restoreWithPlaceholder");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto layout = m->multiSplitter();

    LayoutSaver saver;
    QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_restoreWithPlaceholder.json")));
    QVERIFY(layout->checkSanity());

    QVERIFY(dock1->isFloating());
    QVERIFY(dock1->isVisible());
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 1);

    dock1->setFloating(false); // Put it back. Should go back because the placeholder was restored.

    QVERIFY(!dock1->isFloating());
    QVERIFY(dock1->isVisible());
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);
}

void TestDocks::tst_restoreWithAffinity()
{
    EnsureTopLevelsDeleted e;

    auto m1 = createMainWindow(QSize(500, 500));
    m1->setAffinities({ "a1" });
    auto m2 = createMainWindow(QSize(500, 500));
    m2->setAffinities({ "a2" });

    auto dock1 = createDockWidget("1", new QPushButton("1"), {}, {}, true, "a1");
    m1->addDockWidget(dock1, Location_OnLeft);

    auto dock2 = createDockWidget("2", new QPushButton("2"), {}, {}, true, "a2");
    dock2->setFloating(true);
    dock2->show();

    LayoutSaver saver;
    saver.setAffinityNames({"a1"});
    const QByteArray saved1 = saver.serializeLayout();

    QPointer<FloatingWindow> fw2 = dock2->floatingWindow();
    saver.restoreLayout(saved1);

    // Restoring affinity 1 shouldn't close affinity 2
    QVERIFY(!fw2.isNull());
    QVERIFY(dock2->isVisible());

    // Close all and restore again
    DockRegistry::self()->clear();
    saver.restoreLayout(saved1);

    // dock2 continues closed
    QVERIFY(!dock2->isVisible());

    // dock1 was restored
    QVERIFY(dock1->isVisible());
    QVERIFY(!dock1->isFloating());
    QCOMPARE(dock1->window(), m1.get());

    delete dock2->window();
}

void TestDocks::tst_marginsAfterRestore()
{
    EnsureTopLevelsDeleted e;
    {
        EnsureTopLevelsDeleted e1;
        // MainWindow:
        auto m = createMainWindow(QSize(500, 500), {}, "tst_marginsAfterRestore");
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        m->addDockWidget(dock1, Location_OnLeft);
        auto layout = m->multiSplitter();

        LayoutSaver saver;
        QVERIFY(saver.saveToFile(QStringLiteral("layout_tst_marginsAfterRestore.json")));
        QVERIFY(saver.restoreFromFile(QStringLiteral("layout_tst_marginsAfterRestore.json")));
        QVERIFY(layout->checkSanity());

        dock1->setFloating(true);

        auto fw = dock1->floatingWindow();
        QVERIFY(fw);
        layout->addWidget(fw->dropArea(), Location_OnRight);

        layout->checkSanity();
    }
}

void TestDocks::tst_restoreWithNewDockWidgets()
{
    // Tests that if the LayoutSaver doesn't know about some dock widget
    // when it saves the layout, then it won't close it when restoring layout
    // it will just be ignored.
    EnsureTopLevelsDeleted e;
    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    QVERIFY(!saved.isEmpty());

    auto dock1 = createDockWidget("dock1", new QPushButton("dock1"));
    dock1->show();

    QVERIFY(saver.restoreLayout(saved));
    QVERIFY(dock1->isVisible());

    delete dock1->window();
}

void TestDocks::tst_restoreWithDockFactory()
{
    // Tests that restore the layout with a missing dock widget will recreate the dock widget using a factory

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);
    auto layout = m->multiSplitter();

    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->placeholderCount(), 0);
    QCOMPARE(layout->visibleCount(), 1);

    LayoutSaver saver;
    QByteArray saved = saver.serializeLayout();
    QVERIFY(!saved.isEmpty());
    QPointer<Frame> f1 = dock1->dptr()->frame();
    delete dock1;
    Testing::waitForDeleted(f1);
    QVERIFY(!f1);

    // Directly deleted don't leave placeolders. We could though.
    QCOMPARE(layout->count(), 0);

    {
        // We don't know how to create the dock widget
        SetExpectedWarning expectedWarning("Couldn't find dock widget");
        QVERIFY(saver.restoreLayout(saved));
        QCOMPARE(layout->count(), 0);
    }

    // Now try with a factory func
    DockWidgetFactoryFunc func = [] (const QString &) {
        return createDockWidget("1", new QPushButton("1"), {}, {}, /*show=*/ false);
    };

    KDDockWidgets::Config::self().setDockWidgetFactoryFunc(func);
    QVERIFY(saver.restoreLayout(saved));
    QCOMPARE(layout->count(), 1);
    QCOMPARE(layout->visibleCount(), 1);
    layout->checkSanity();
}

void TestDocks::tst_restoreWithDockFactory2()
{
    // Teste that the factory function can do id remapping.
    // For example, if id "foo" is missing, the factory can return a
    // dock widget with id "bar" if it feels like it

    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);

    auto dock1 = createDockWidget("dw1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);
    dock1->setFloating(true);

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    delete dock1;

    DockWidgetFactoryFunc func = [] (const QString &) {
        // A factory func which does id remapping
        return createDockWidget("dw2", new QPushButton("w"), {}, {}, /*show=*/ false);
    };

    KDDockWidgets::Config::self().setDockWidgetFactoryFunc(func);
    saver.restoreLayout(saved);
}

void TestDocks::tst_addDockWidgetToMainWindow()
{
    EnsureTopLevelsDeleted e;
     auto m = createMainWindow();
     auto dock1 = createDockWidget("dock1", new QPushButton("one"));
     auto dock2 = createDockWidget("dock2", new QPushButton("two"));

     m->addDockWidget(dock1, Location_OnRight, nullptr);
     m->addDockWidget(dock2, Location_OnTop, dock1);
     QVERIFY(m->dropArea()->checkSanity());

     QCOMPARE(dock1->window(), m.get());
     QCOMPARE(dock2->window(), m.get());
     QVERIFY(dock1->dptr()->frame()->QWidgetAdapter::y()
             > dock2->dptr()->frame()->QWidgetAdapter::y());
     QCOMPARE(dock1->dptr()->frame()->QWidgetAdapter::x(),
              dock2->dptr()->frame()->QWidgetAdapter::x());
}

void TestDocks::tst_addDockWidgetToContainingWindow()
{
    { // Test with a floating window
        EnsureTopLevelsDeleted e;

        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dock3 = createDockWidget("dock3", new QPushButton("three"));

        dock1->addDockWidgetToContainingWindow(dock2, Location_OnRight);
        dock1->addDockWidgetToContainingWindow(dock3, Location_OnTop, dock2);

        QCOMPARE(dock1->window(), dock2->window());
        QCOMPARE(dock2->window(), dock3->window());

        QVERIFY(dock3->dptr()->frame()->QWidgetAdapter::y()
                < dock2->dptr()->frame()->QWidgetAdapter::y());
        QVERIFY(dock1->dptr()->frame()->QWidgetAdapter::x()
                < dock2->dptr()->frame()->QWidgetAdapter::x());
        QCOMPARE(dock2->dptr()->frame()->QWidgetAdapter::x(),
                 dock3->dptr()->frame()->QWidgetAdapter::x());
    }

    { // Also test with a main window
        EnsureTopLevelsDeleted e;

        auto m = createMainWindow();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        m->addDockWidget(dock1, Location_OnRight, nullptr);
        dock1->addDockWidgetToContainingWindow(dock2, Location_OnRight);

        QCOMPARE(dock1->window(), dock2->window());
        QCOMPARE(m.get(), dock2->window());
    }
}

void TestDocks::tst_notClosable()
{
    EnsureTopLevelsDeleted e;
    {
        auto dock1 = createDockWidget("dock1", new QPushButton("one"), DockWidgetBase::Option_NotClosable);
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        dock1->addDockWidgetAsTab(dock2);

        auto fw = dock1->floatingWindow();
        QVERIFY(fw);
        TitleBar *titlebarFW = fw->titleBar();
        TitleBar *titleBarFrame = fw->frames().at(0)->titleBar();
        QVERIFY(titlebarFW->isCloseButtonVisible());
        QVERIFY(!titlebarFW->isCloseButtonEnabled());
        QVERIFY(!titleBarFrame->isCloseButtonVisible());
        QVERIFY(!titleBarFrame->isCloseButtonEnabled());

        dock1->setOptions(DockWidgetBase::Option_None);
        QVERIFY(titlebarFW->isCloseButtonVisible());
        QVERIFY(titlebarFW->isCloseButtonEnabled());
        QVERIFY(!titleBarFrame->isCloseButtonVisible());
        QVERIFY(!titleBarFrame->isCloseButtonEnabled());

        dock1->setOptions(DockWidgetBase::Option_NotClosable);
        QVERIFY(titlebarFW->isCloseButtonVisible());
        QVERIFY(!titlebarFW->isCloseButtonEnabled());
        QVERIFY(!titleBarFrame->isCloseButtonVisible());
        QVERIFY(!titleBarFrame->isCloseButtonEnabled());

        auto window = dock1->window();
        window->deleteLater();
        Testing::waitForDeleted(window);
    }

    {
        // Now dock dock1 into dock1 instead

        auto dock1 = createDockWidget("dock1", new QPushButton("one"), DockWidgetBase::Option_NotClosable);
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        dock2->dptr()->morphIntoFloatingWindow();
        dock2->addDockWidgetAsTab(dock1);

        auto fw = dock1->floatingWindow();
        QVERIFY(fw);
        TitleBar *titlebarFW = fw->titleBar();
        TitleBar *titleBarFrame = fw->frames().at(0)->titleBar();

        QVERIFY(titlebarFW->isCloseButtonVisible());
        QVERIFY(!titleBarFrame->isCloseButtonVisible());
        QVERIFY(!titleBarFrame->isCloseButtonEnabled());

        auto window = dock2->window();
        window->deleteLater();
        Testing::waitForDeleted(window);
    }
}

void TestDocks::tst_dragOverTitleBar()
{
    // Tests that dragging over the title bar is returning DropLocation_None

    EnsureTopLevelsDeleted e;
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("Two"));

    DropArea *da = dock1->floatingWindow()->dropArea();
    FloatingWindow *fw1 = dock1->floatingWindow();
    FloatingWindow *fw2 = dock2->floatingWindow();
    WindowBeingDragged wbd(fw2, fw2);

    const QPoint titleBarPoint = fw1->titleBar()->mapToGlobal(QPoint(5, 5));

    auto loc = da->hover(&wbd, titleBarPoint);
    QCOMPARE(loc, DropIndicatorOverlayInterface::DropLocation_None);

    delete fw1;
    delete fw2;
}

void TestDocks::tst_setFloatingGeometry()
{
    EnsureTopLevelsDeleted e;
    auto dock1 = createDockWidget("dock1", new MyWidget("one"));

    QVERIFY(dock1->isVisible());
    const QRect requestedGeo = QRect(70, 70, 400, 400);
    dock1->setFloatingGeometry(requestedGeo);
    QCOMPARE(dock1->window()->geometry(), requestedGeo);

    dock1->close();
    QVERIFY(!dock1->isVisible());

    const QRect requestedGeo2 = QRect(80, 80, 400, 400);
    dock1->setFloatingGeometry(requestedGeo2);
    dock1->show();

    QCOMPARE(dock1->window()->geometry(), requestedGeo2);
}

void TestDocks::tst_setFloatingAfterDraggedFromTabToSideBySide()
{
    EnsureTopLevelsDeleted e;
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dropArea = m->dropArea();
        auto layout = dropArea;

        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        dock1->addDockWidgetAsTab(dock2);

        // Move from tab to bottom
        m->addDockWidget(dock2, KDDockWidgets::Location_OnBottom);

        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 0);

        dock2->setFloating(true);
        dock2->setFloating(false);
        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 0);
        QVERIFY(!dock2->isFloating());
    }

    {
        // 2. Try again, but now detach from tab before putting it on the bottom. What was happening was that MultiSplitterLayout::addWidget()
        // called with a MultiSplitter as widget wasn't setting the layout items for the dock widgets
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        auto dropArea = m->dropArea();
        auto layout = dropArea;

        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        dock1->addDockWidgetAsTab(dock2);
        Item *oldItem2 = dock2->dptr()->lastPositions().lastItem();
        QCOMPARE(oldItem2, layout->itemForFrame(dock2->dptr()->frame()));


        // Detach tab
        dock1->dptr()->frame()->detachTab(dock2);
        QVERIFY(layout->checkSanity());
        auto fw2 = dock2->floatingWindow();
        QVERIFY(fw2);
        QCOMPARE(dock2->dptr()->lastPositions().lastItem(), oldItem2);
        Item *item2 = fw2->dropArea()->itemForFrame(dock2->dptr()->frame());
        QVERIFY(item2);
        QCOMPARE(item2->hostWidget()->asQObject(), fw2->dropArea());
        QVERIFY(!layout->itemForFrame(dock2->dptr()->frame()));

        // Move from tab to bottom
        layout->addWidget(fw2->dropArea(), KDDockWidgets::Location_OnRight, nullptr);
        QVERIFY(layout->checkSanity());
        QVERIFY(dock2->dptr()->lastPositions().lastItem());
        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 0);

        dock2->setFloating(true);
        QVERIFY(layout->checkSanity());

        dock2->setFloating(false);

        QCOMPARE(layout->count(), 2);
        QCOMPARE(layout->placeholderCount(), 0);
        QVERIFY(!dock2->isFloating());
        QVERIFY(layout->checkSanity());

        Testing::waitForDeleted(fw2);
    }
}

void TestDocks::tst_setFloatingAFrameWithTabs()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dropArea = m->dropArea();
    auto layout = dropArea;
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    dock1->addDockWidgetAsTab(dock2);

    // Make it float
    dock1->dptr()->frame()->titleBar()->onFloatClicked();

    auto fw = dock1->floatingWindow();
    QVERIFY(fw);
    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 1);

    auto frame1 = dock1->dptr()->frame();
    QVERIFY(frame1->layoutItem());

    // Attach it again
    dock1->dptr()->frame()->titleBar()->onFloatClicked();

    QCOMPARE(layout->count(), 2);
    QCOMPARE(layout->placeholderCount(), 0);
    QCOMPARE(dock1->window(), m.get());

    Testing::waitForDeleted(fw);
}

void TestDocks::tst_tabBarWithHiddenTitleBar_data()
{
    QTest::addColumn<bool>("hiddenTitleBar");
    QTest::addColumn<bool>("tabsAlwaysVisible");

    QTest::newRow("false-false") << false << false;
    QTest::newRow("true-false") << true << false;

    QTest::newRow("false-true") << false << true;
    QTest::newRow("true-true") << true << true;

}

void TestDocks::tst_tabBarWithHiddenTitleBar()
{
    EnsureTopLevelsDeleted e;
    QFETCH(bool, hiddenTitleBar);
    QFETCH(bool, tabsAlwaysVisible);

    const auto originalFlags = KDDockWidgets::Config::self().flags();

    auto newFlags = originalFlags;

    if (hiddenTitleBar)
        newFlags = newFlags | KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible;

    if (tabsAlwaysVisible)
        newFlags = newFlags | KDDockWidgets::Config::Flag_AlwaysShowTabs;

    KDDockWidgets::Config::self().setFlags(newFlags);

    auto m = createMainWindow();

    auto d1 = createDockWidget("1", new QTextEdit());
    auto d2 = createDockWidget("2", new QTextEdit());
    m->addDockWidget(d1, Location_OnTop);

    if (tabsAlwaysVisible) {
        if (hiddenTitleBar)
            QVERIFY(!d1->dptr()->frame()->titleBar()->isVisible());
        else
            QVERIFY(d1->dptr()->frame()->titleBar()->isVisible());
    } else {
        QVERIFY(d1->dptr()->frame()->titleBar()->isVisible());
    }

    d1->addDockWidgetAsTab(d2);

    QVERIFY(d2->dptr()->frame()->titleBar()->isVisible() ^ hiddenTitleBar);

    d2->close();
    m->layoutWidget()->checkSanity();
    delete d2;
    if (tabsAlwaysVisible) {
        if (hiddenTitleBar)
            QVERIFY(!d1->dptr()->frame()->titleBar()->isVisible());
        else
            QVERIFY(d1->dptr()->frame()->titleBar()->isVisible());
    } else {
        QVERIFY(d1->dptr()->frame()->titleBar()->isVisible());
    }
}

void TestDocks::tst_toggleDockWidgetWithHiddenTitleBar()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible | KDDockWidgets::Config::Flag_AlwaysShowTabs);
    auto m = createMainWindow();

    auto d1 = createDockWidget("1", new QTextEdit());
    m->addDockWidget(d1, Location_OnTop);

    QVERIFY(!d1->dptr()->frame()->titleBar()->isVisible());

    d1->toggleAction()->setChecked(false);
    auto f1 = d1->dptr()->frame();
    Testing::waitForDeleted(f1);
    d1->toggleAction()->setChecked(true);
    QVERIFY(d1->dptr()->frame());
    QVERIFY(!d1->dptr()->frame()->titleBar()->isVisible());
}

void TestDocks::tst_availableSizeWithPlaceholders()
{
    // Tests MultiSplitterLayout::available() with and without placeholders. The result should be the same.

    EnsureTopLevelsDeleted e;
    QVector<DockDescriptor> docks1 = {
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartHidden },
        };

    QVector<DockDescriptor> docks2 = {
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnBottom, -1, nullptr, InitialVisibilityOption::StartVisible },
        };

    QVector<DockDescriptor> empty;

    auto m1 = createMainWindow(docks1);
    auto m2 = createMainWindow(docks2);
    auto m3 = createMainWindow(empty);

    auto layout1 = m1->multiSplitter();
    auto layout2 = m2->multiSplitter();
    auto layout3 = m3->multiSplitter();

    auto f20 = docks2.at(0).createdDock->dptr()->frame();

    docks2.at(0).createdDock->close();
    docks2.at(1).createdDock->close();
    docks2.at(2).createdDock->close();
    QVERIFY(Testing::waitForDeleted(f20));

    QCOMPARE(layout1->size(), layout2->size());
    QCOMPARE(layout1->size(), layout3->size());

    QCOMPARE(layout1->availableSize(), layout2->availableSize());
    QCOMPARE(layout1->availableSize(), layout3->availableSize());

    // Now show 1 widget in m1 and m3
    docks1.at(0).createdDock->show();
    m3->addDockWidget(docks2.at(0).createdDock, Location_OnBottom); // just steal from m2

    QCOMPARE(layout1->size(), layout3->size());

    Frame *f10 = docks1.at(0).createdDock->dptr()->frame();
    Item *item10 = layout1->itemForFrame(f10);
    Item *item30 = layout3->itemForFrame(docks2.at(0).createdDock->dptr()->frame());

    QCOMPARE(item10->geometry(), item30->geometry());
    QCOMPARE(item10->guestWidget()->minSize(), item10->guestWidget()->minSize());
    QCOMPARE(item10->minSize(), item30->minSize());
    QCOMPARE(layout1->availableSize(), layout3->availableSize());

    layout1->checkSanity();
    layout2->checkSanity();
    layout3->checkSanity();

    // Cleanup
    docks1.at(0).createdDock->deleteLater();
    docks1.at(1).createdDock->deleteLater();
    docks1.at(2).createdDock->deleteLater();
    docks2.at(0).createdDock->deleteLater();
    docks2.at(1).createdDock->deleteLater();
    docks2.at(2).createdDock->deleteLater();
    QVERIFY(Testing::waitForDeleted(docks2.at(2).createdDock));
}

void TestDocks::tst_anchorFollowingItselfAssert()
{
    // 1. Tests that we don't assert in Anchor::setFollowee()
    //  ASSERT: "this != m_followee" in file ../src/multisplitter/Anchor.cpp
    EnsureTopLevelsDeleted e;
    QVector<DockDescriptor> docks = {
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnTop, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnLeft, -1, nullptr, InitialVisibilityOption::StartVisible },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartHidden },
        {Location_OnRight, -1, nullptr, InitialVisibilityOption::StartVisible } };

    auto m = createMainWindow(docks);
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;
    layout->checkSanity();

    auto dock1 = docks.at(1).createdDock;
    auto dock2 = docks.at(2).createdDock;
    dock2->setFloating(true);
    auto fw2 = dock2->floatingWindow();
    dropArea->addWidget(fw2->dropArea(), Location_OnLeft, dock1->dptr()->frame());
    dock2->setFloating(true);
    fw2 = dock2->floatingWindow();
    dropArea->addWidget(fw2->dropArea(), Location_OnRight, dock1->dptr()->frame());

    docks.at(0).createdDock->deleteLater();
    docks.at(4).createdDock->deleteLater();
    Testing::waitForDeleted(docks.at(4).createdDock);
}

void TestDocks::tst_positionWhenShown()
{
    // Tests that when showing a dockwidget it shows in the same position as before
    EnsureTopLevelsDeleted e;
    auto window = createMainWindow();
    auto dock1 = new DockWidgetType("1");
    dock1->show();
    dock1->window()->windowHandle()->setPosition(100, 100);
    QTest::qWait(1000);
    QCOMPARE(dock1->window()->windowHandle()->geometry().topLeft(), QPoint(100, 100));

    dock1->close();
    dock1->show();
    QCOMPARE(dock1->window()->windowHandle()->geometry().topLeft(), QPoint(100, 100));

    // Cleanup
    window->layoutWidget()->checkSanity();
    dock1->deleteLater();
    QVERIFY(Testing::waitForDeleted(dock1));
}

void TestDocks::tst_moreTitleBarCornerCases()
{
    {
        EnsureTopLevelsDeleted e;
        auto dock1 = createDockWidget("dock1", new QPushButton("foo1"));
        auto dock2 = createDockWidget("dock2", new QPushButton("foo2"));
        dock1->show();
        dock2->show();
        auto fw2 = dock2->window();
        dock1->addDockWidgetToContainingWindow(dock2, Location_OnLeft);
        QVERIFY(dock1->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(dock2->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(dock1->dptr()->frame()->titleBar() != dock2->dptr()->frame()->titleBar());
        auto fw = dock1->floatingWindow();
        QVERIFY(fw->titleBar()->isVisible());
        QVERIFY(fw->titleBar() != dock1->dptr()->frame()->titleBar());
        QVERIFY(fw->titleBar() != dock2->dptr()->frame()->titleBar());
        delete fw;
        delete fw2;
    }

    {
        EnsureTopLevelsDeleted e;
        auto dock1 = createDockWidget("dock1", new QPushButton("foo1"));
        auto dock2 = createDockWidget("dock2", new QPushButton("foo2"));
        dock1->show();
        dock2->show();
        auto fw1 = dock1->floatingWindow();
        auto fw2 = dock2->floatingWindow();
        fw1->dropArea()->drop(fw2, Location_OnRight, nullptr);
        QVERIFY(fw1->titleBar()->isVisible());
        QVERIFY(dock1->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(dock2->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(dock1->dptr()->frame()->titleBar() != dock2->dptr()->frame()->titleBar());
        QVERIFY(fw1->titleBar() != dock1->dptr()->frame()->titleBar());
        QVERIFY(fw1->titleBar() != dock2->dptr()->frame()->titleBar());
        delete fw1;
        delete fw2;
    }

    {
        // Tests that restoring a single floating dock widget doesn't make it show two title-bars
        // As reproduced myself... and fixed in this commit

        EnsureTopLevelsDeleted e;
        auto dock1 = createDockWidget("dock1", new QPushButton("foo1"));
        dock1->show();

        auto fw1 = dock1->floatingWindow();
        QVERIFY(!dock1->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(fw1->titleBar()->isVisible());

        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();
        saver.restoreLayout(saved);

        delete fw1; // the old window

        fw1 = dock1->floatingWindow();
        QVERIFY(dock1->isVisible());
        QVERIFY(!dock1->dptr()->frame()->titleBar()->isVisible());
        QVERIFY(fw1->titleBar()->isVisible());
        delete dock1->window();
    }
}



void TestDocks::tst_isInMainWindow()
{
    EnsureTopLevelsDeleted e;
    auto dw = new DockWidgetType(QStringLiteral("FOO"));
    dw->show();
    auto fw = dw->window();
    QVERIFY(!dw->isInMainWindow());
    auto m1 = createMainWindow(QSize(2560, 809), MainWindowOption_None, "MainWindow1");
    m1->addDockWidget(dw, KDDockWidgets::Location_OnLeft);
    QVERIFY(dw->isInMainWindow());
    delete fw;

    // Also test after creating the MainWindow, as the FloatingWindow will get parented to it
    auto dw2 = new DockWidgetType(QStringLiteral("2"));
    dw2->show();
    QVERIFY(!dw2->isInMainWindow());
    delete dw2->window();
}

void TestDocks::tst_sizeConstraintWarning()
{
    // Tests that we don't get the warning: MultiSplitterLayout::checkSanity: Widget has height= 122 but minimum is 144 KDDockWidgets::Item
    // Code autogenerated by the fuzzer:
    EnsureTopLevelsDeleted e;
    SetExpectedWarning sew("Dock widget already exists in the layout");

    auto window = createMainWindow();
    QList<DockWidgetBase *> listDockWidget;
    {
       auto dock = new DockWidgetType("foo-0");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-1");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-2");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-3");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-4");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-5");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-6");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-7");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-8");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-9");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-10");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-11");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-12");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-13");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-14");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-15");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-16");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-17");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }
    {
       auto dock = new DockWidgetType("foo-18");
       dock->setWidget(new QTextEdit());
       listDockWidget.append(dock);
    }

    auto dropArea = window->dropArea();
    window->addDockWidget(listDockWidget.at(0), static_cast<Location>(2));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(1), static_cast<Location>(1));
    dropArea->checkSanity();

    listDockWidget.at(2 - 1)->addDockWidgetAsTab(listDockWidget.at(2));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(3-1), static_cast<Location>(2), listDockWidget.at(3), static_cast<InitialVisibilityOption>(1));
    dropArea->checkSanity();

    listDockWidget.at(4 - 1)->addDockWidgetAsTab(listDockWidget.at(4));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(5), static_cast<Location>(1));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(6), static_cast<Location>(1));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(7), static_cast<Location>(4));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(8-1), static_cast<Location>(1), listDockWidget.at(8), static_cast<InitialVisibilityOption>(1));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(9), static_cast<Location>(2));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(10-1), static_cast<Location>(2), listDockWidget.at(10), static_cast<InitialVisibilityOption>(1));
    dropArea->checkSanity();

    listDockWidget.at(11 - 1)->addDockWidgetAsTab(listDockWidget.at(11));
    dropArea->checkSanity();

    listDockWidget.at(12 - 1)->addDockWidgetAsTab(listDockWidget.at(12));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(13), static_cast<Location>(4));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(14), static_cast<Location>(2));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(15), static_cast<Location>(3));
    dropArea->checkSanity();

    window->addDockWidget(listDockWidget.at(16), static_cast<Location>(4));
    dropArea->checkSanity();

    listDockWidget.at(17 - 1)->addDockWidgetAsTab(listDockWidget.at(17));
    dropArea->checkSanity();
    listDockWidget.at(18 - 1)->addDockWidgetAsTab(listDockWidget.at(18));
    dropArea->checkSanity();

    auto docks = DockRegistry::self()->dockwidgets();
    auto lastDock = docks.last();
    for (auto dock: docks)
        dock->deleteLater();

    Testing::waitForDeleted(lastDock);
}

void TestDocks::tst_stuckSeparator()
{
    const QString absoluteLayoutFileName = QStringLiteral(":/layouts/stuck-separator.json");

    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(2560, 809), MainWindowOption_None, "MainWindow1");
    const int numDockWidgets = 26;
    DockWidgetBase *dw25 = nullptr;
    for (int i = 0; i < numDockWidgets; ++i) {
        auto createdDw = createDockWidget(QStringLiteral("dock-%1").arg(i));
        if (i == 25)
            dw25 = createdDw;
    }

    LayoutSaver restorer;
    QVERIFY(restorer.restoreFromFile(absoluteLayoutFileName));

    Frame *frame25 = dw25->dptr()->frame();
    ItemBoxContainer *root = m1->multiSplitter()->rootItem();
    Item *item25 = root->itemForWidget(frame25);
    ItemBoxContainer *container25 = item25->parentBoxContainer();
    Separator::List separators = container25->separators();
    QCOMPARE(separators.size(), 1);

    Separator *separator25 = separators.constFirst();
    const int sepMin = container25->minPosForSeparator_global(separator25);
    const int sepMax = container25->maxPosForSeparator_global(separator25);

    QVERIFY(sepMin <= sepMax);

    for (auto dw : DockRegistry::self()->dockwidgets()) {
        delete dw;
    }
}

void TestDocks::tst_titlebar_getter()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(1000, 1000), MainWindowOption_HasCentralFrame);
    m->resize(QSize(500, 500));
    m->show();

    auto w1 = new MyWidget2(QSize(400, 400));
    auto d1 = createDockWidget("1", w1);

    m->addDockWidget(d1, Location_OnTop);

    QVERIFY(d1->titleBar()->isVisible());
    d1->setFloating(true);
    QVERIFY(d1->floatingWindow());
    QVERIFY(d1->floatingWindow()->isVisible());
    QVERIFY(d1->titleBar()->isVisible());
}

void TestDocks::tst_dockNotFillingSpace()
{
     EnsureTopLevelsDeleted e;
     auto m = createMainWindow(QSize(1000, 1000));
     m->resize(QSize(500, 500));
     m->show();

     auto d1 = createDockWidget("1", new QTextEdit());
     auto d2 = createDockWidget("2", new QTextEdit());
     auto d3 = createDockWidget("3", new QTextEdit());

     m->addDockWidget(d1, Location_OnTop);
     m->addDockWidget(d2, Location_OnBottom);
     m->addDockWidget(d3, Location_OnBottom);

     Frame *frame2 = d2->dptr()->frame();
     d1->close();
     d2->close();
     Testing::waitForDeleted(frame2);

     auto layout = m->multiSplitter();
     QVERIFY(layout->checkSanity());

     delete d1;
     delete d2;
}

void TestDocks::tst_lastFloatingPositionIsRestored()
{
    EnsureTopLevelsDeleted e;

    auto m1 = createMainWindow();
    auto dock1 = createDockWidget("dock1");
    dock1->show();
    QPoint targetPos = QPoint(340, 340);
    dock1->window()->windowHandle()->setFramePosition(targetPos);
    QCOMPARE(dock1->window()->windowHandle()->frameGeometry().topLeft(), targetPos);
    auto oldFw = dock1->window();
    Testing::waitForEvent(dock1->window(), QEvent::Move);

    LayoutSaver saver;
    QByteArray saved = saver.serializeLayout();

    dock1->window()->move(0, 0);
    dock1->close();
    delete oldFw;

    saver.restoreLayout(saved);
    QCOMPARE(dock1->window()->windowHandle()->frameGeometry().topLeft(), targetPos);

    // Adjsut to what we got without the frame
    targetPos = dock1->window()->geometry().topLeft();

    // Now dock it:
    m1->addDockWidget(dock1, Location_OnTop);
    QCOMPARE(dock1->dptr()->lastPositions().lastFloatingGeometry().topLeft(), targetPos);

    dock1->setFloating(true);
    QCOMPARE(dock1->window()->geometry().topLeft(), targetPos);

    saver.restoreLayout(saved);
    QCOMPARE(dock1->window()->geometry().topLeft(), targetPos);

    // Dock again and save:
    m1->addDockWidget(dock1, Location_OnTop);
    saved = saver.serializeLayout();
    dock1->setFloating(true);
    dock1->window()->move(0, 0);
    saver.restoreLayout(saved);
    QVERIFY(!dock1->isFloating());
    dock1->setFloating(true);
    QCOMPARE(dock1->window()->geometry().topLeft(), targetPos);
    delete dock1->window();
}

void TestDocks::tst_titleBarFocusedWhenTabsChange()
{
     EnsureTopLevelsDeleted e;
     KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_TitleBarIsFocusable);

     auto le1 = new FocusableWidget();
     le1->setObjectName("le1");
     auto dock1 = createDockWidget(QStringLiteral("dock1"), le1);
     auto dock2 = createDockWidget(QStringLiteral("dock2"), new FocusableWidget());
     auto dock3 = createDockWidget(QStringLiteral("dock3"), new FocusableWidget());
     auto oldFw1 = dock1->window();
     auto oldFw2 = dock2->window();
     auto oldFw3 = dock3->window();

     auto m1 = createMainWindow(QSize(2560, 809), MainWindowOption_None, "MainWindow1");

     m1->addDockWidget(dock1, Location_OnLeft);
     m1->addDockWidget(dock2, Location_OnRight);
     delete oldFw1;
     delete oldFw2;
     dock2->addDockWidgetAsTab(dock3);
     delete oldFw3;

     TitleBar *titleBar1 = dock1->titleBar();
     dock1->widget()->setFocus(Qt::MouseFocusReason);

     QVERIFY(dock1->isFocused() || Testing::waitForEvent(dock1->widget(), QEvent::FocusIn));
     QVERIFY(titleBar1->isFocused());

     auto frame2 = dock2->dptr()->frame();

     TabWidget *tb2 = frame2->tabWidget();
     QCOMPARE(tb2->currentIndex(), 1); // Was the last to be added

     auto tabBar2 = tb2->tabBar();
     const QRect rect0 = tabBar2->rectForTab(0);
     const QPoint globalPos = tabBar2->asWidget()->mapToGlobal(rect0.topLeft()) + QPoint(5, 5);
     Tests::clickOn(globalPos, tabBar2->asWidget());

     QVERIFY(!titleBar1->isFocused());
     QVERIFY(dock2->titleBar()->isFocused());

     // Test that clicking on a tab that is already current will also set focus
     dock1->setFocus(Qt::MouseFocusReason);
     QVERIFY(dock1->titleBar()->isFocused());
     QVERIFY(!dock2->titleBar()->isFocused());

#ifdef KDDOCKWIDGETS_QTWIDGETS
     // TODO: Not yet ready for QtQuick. The TitleBar.qml is clicked, but we check the C++ TitleBar for focus
     Tests::clickOn(globalPos, tabBar2->asWidget());
     QVERIFY(!dock1->titleBar()->isFocused());
     QVERIFY(dock2->titleBar()->isFocused());
#endif
}

#ifdef KDDOCKWIDGETS_QTWIDGETS

void TestDocks::tst_tabsNotClickable()
{
    // Well, not a great unit-test, as it's only repro when it's Windows sending the native event
    // Can't repro with fabricated events. Uncomment the WAIT and test different configs manually
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_Default  | KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible);

    auto dock1 = createDockWidget("dock1", new QWidget());
    auto dock2 = createDockWidget("dock2", new QWidget());
    dock1->addDockWidgetAsTab(dock2);

    auto frame = qobject_cast<FrameWidget *>(dock1->dptr()->frame());
    QCOMPARE(frame->currentIndex(), 1);

    QTest::qWait(500); // wait for window to get proper geometry

    const QPoint clickPoint = frame->tabBar()->mapToGlobal(frame->tabBar()->tabRect(0).center());
    QCursor::setPos(clickPoint); // Just for visual debug when needed

    pressOn(clickPoint, frame->tabBar());
    releaseOn(clickPoint, frame->tabBar());

   // WAIT // Uncomment for MANUAL test. Also test by adding Flag_AlwaysShowTabs

    QCOMPARE(frame->currentIndex(), 0);

    delete frame->window();
}

void TestDocks::tst_mainWindowAlwaysHasCentralWidget()
{
    EnsureTopLevelsDeleted e;

    auto m = createMainWindow();

    QWidget *central = m->centralWidget();
    auto dropArea = m->dropArea();
    QVERIFY(dropArea);

    QPointer<Frame> centralFrame = static_cast<Frame*>(dropArea->centralFrame()->guestAsQObject());
    QVERIFY(central);
    QVERIFY(dropArea);
    QCOMPARE(dropArea->count(), 1);
    QVERIFY(centralFrame);
    QCOMPARE(centralFrame->dockWidgetCount(), 0);

    // Add a tab
    auto dock = createDockWidget("doc1", Qt::green);
    m->addDockWidgetAsTab(dock);
    QCOMPARE(dropArea->count(), 1);
    QCOMPARE(centralFrame->dockWidgetCount(), 1);

    qDebug() << "Central widget width=" << central->size() << "; mainwindow="
             << m->size();

    // Detach tab
    QPoint globalPressPos = dragPointForWidget(centralFrame.data(), 0);
    QTabBar *tabBar = static_cast<FrameWidget*>(centralFrame.data())->tabBar();
    QVERIFY(tabBar);
    qDebug() << "Detaching tab from dropArea->size=" << dropArea->QWidget::size() << "; dropArea=" << dropArea;
    drag(tabBar, globalPressPos, m->geometry().bottomRight() + QPoint(30, 30));

    QVERIFY(centralFrame);
    QCOMPARE(dropArea->count(), 1);
    QCOMPARE(centralFrame->dockWidgetCount(), 0);
    QVERIFY(dropArea->checkSanity());

    delete dock->window();
}

void TestDocks::tst_dockableMainWindows()
{
    EnsureTopLevelsDeleted e;

     auto m1 = createMainWindow();
     auto dock1 = createDockWidget("dock1", new QPushButton("foo"));
     m1->addDockWidget(dock1, Location_OnTop);

     auto m2 = new KDDockWidgets::MainWindow("mainwindow-dockable");
     auto m2Container = createDockWidget("mainwindow-dw", m2);
     auto menubar = m2->menuBar();
     menubar->addMenu("File");
     menubar->addMenu("View");
     menubar->addMenu("Help");
     m2Container->show();

     auto dock21 = createDockWidget("dock21", new QPushButton("foo"));
     auto dock22 = createDockWidget("dock22", new QPushButton("foo"));
     m2->addDockWidget(dock21, Location_OnLeft);
     m2->addDockWidget(dock22, Location_OnRight);

     auto fw = m2Container->floatingWindow();
     TitleBar *fwTitleBar = fw->titleBar();

     QVERIFY(fw->hasSingleFrame());
     QVERIFY(fw->hasSingleDockWidget());

     // Check that the inner-inner dock widgets have a visible title-bar
     QVERIFY(dock21->titleBar()->isVisible());
     QVERIFY(dock22->titleBar()->isVisible());
     QVERIFY(dock21->titleBar() != fwTitleBar);
     QVERIFY(dock22->titleBar() != fwTitleBar);

     const QPoint startPoint = fwTitleBar->mapToGlobal(QPoint(5, 5));
     const QPoint destination = startPoint + QPoint(20, 20);

     // Check that we don't get the "Refusing to itself" warning. not actually dropping anywhere
     drag(fwTitleBar, startPoint, destination);

     // The FloatingWindow has a single DockWidget, so it shows the title bar, while the Frame doesn't
     QVERIFY(fwTitleBar->isVisible());
     QVERIFY(!m2Container->dptr()->frame()->titleBar()->isVisible());

     fw->dropArea()->addDockWidget(dock1, Location::Location_OnLeft, nullptr);
     // Now the FloatingWindow has two dock widgets, so our main window dock widget also shows the title bar
     QVERIFY(fwTitleBar->isVisible());
     QVERIFY(m2Container->dptr()->frame()->titleBar()->isVisible());

     // Put it how it was, FloatingWindow is single dock again
     auto frame1 = dock1->dptr()->frame();
     dock1->close();
     Testing::waitForDeleted(frame1);
     QVERIFY(fwTitleBar->isVisible());
     QVERIFY(!m2Container->dptr()->frame()->titleBar()->isVisible());

     // Repeat, but instead of closing dock1, we float it
     fw->dropArea()->addDockWidget(dock1, Location::Location_OnLeft, nullptr);
     QVERIFY(fwTitleBar->isVisible());
     QVERIFY(m2Container->dptr()->frame()->titleBar()->isVisible());
     frame1 = dock1->dptr()->frame();
     frame1->titleBar()->onFloatClicked();
     QVERIFY(fwTitleBar->isVisible());

     QVERIFY(!m2Container->dptr()->frame()->titleBar()->isVisible());

     fw->dropArea()->addDockWidget(dock1, Location::Location_OnLeft, nullptr);
}

// No need to port to QtQuick
void TestDocks::tst_floatingWindowDeleted()
{
    // Tests a case where the empty floating dock widget wouldn't be deleted
    // Doesn't repro QTBUG-83030 unfortunately, as we already have an event loop running
    // but let's leave this here nonetheless
    class MyMainWindow : public KDDockWidgets::MainWindow {
    public:

        MyMainWindow()
            : KDDockWidgets::MainWindow("tst_floatingWindowDeleted", MainWindowOption_None)
        {
            auto dock1 = new KDDockWidgets::DockWidget(QStringLiteral("DockWidget #1"));
            auto myWidget = new QWidget();
            dock1->setWidget(myWidget);
            dock1->resize(600, 600);
            dock1->show();

            auto dock2 = new KDDockWidgets::DockWidget(QStringLiteral("DockWidget #2"));
            myWidget = new QWidget();
            dock2->setWidget(myWidget);
            dock2->resize(600, 600);
            dock2->show();

            dock1->addDockWidgetAsTab(dock2);
        }
    };

    MyMainWindow m;
}

void TestDocks::tst_addToSmallMainWindow6()
{
    EnsureTopLevelsDeleted e;
    // Test test shouldn't spit any warnings

    QWidget container;
    auto lay = new QVBoxLayout(&container);
    MainWindow m("MyMainWindow_tst_addToSmallMainWindow8", MainWindowOption_None);
    lay->addWidget(&m);
    container.resize(100, 100);
    Testing::waitForResize(&container);
    container.show();
    Testing::waitForResize(&m);
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(50, 240)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(50, 240)));
    m.addDockWidget(dock1, KDDockWidgets::Location_OnBottom);
    m.addDockWidget(dock2, KDDockWidgets::Location_OnBottom);
    Testing::waitForResize(&m);
    QVERIFY(m.dropArea()->checkSanity());
}


void TestDocks::tst_closeRemovesFromSideBar()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto fw1 = dw1->window();
    m1->addDockWidget(dw1, Location_OnBottom);
    m1->moveToSideBar(dw1);

    QVERIFY(!dw1->isOverlayed());
    QVERIFY(!dw1->isVisible());
    QVERIFY(dw1->isInSideBar());

    SideBar *sb = m1->sideBarForDockWidget(dw1);
    QVERIFY(sb);

    // Overlay it:
    sb->toggleOverlay(dw1);
    QVERIFY(dw1->isOverlayed());
    QVERIFY(dw1->isVisible());
    QCOMPARE(dw1->sideBarLocation(), sb->location());
    QVERIFY(dw1->isInMainWindow());
    QVERIFY(!dw1->isFloating());

    // Close it while it's overlayed:
    dw1->close();
    QVERIFY(!dw1->isInMainWindow());
    QVERIFY(!dw1->isOverlayed());
    QVERIFY(!dw1->isVisible());
    QCOMPARE(dw1->sideBarLocation(), SideBarLocation::None);

    delete fw1;
}

void TestDocks::tst_restoreSideBar()
{
    SideBarLocation loc;
    QByteArray serialized; // serialization after having 1 sidebar visible
    QByteArray beforeSideBarSerialized; // serialization without any sidebar visible

    {
        LayoutSaver saver;
        EnsureTopLevelsDeleted e;
        KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
        auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
        auto dw1 = new DockWidgetType(QStringLiteral("1"));
        auto fw1 = dw1->window();
        m1->addDockWidget(dw1, Location_OnBottom);
        beforeSideBarSerialized = saver.serializeLayout();
        QVERIFY(!m1->anySideBarIsVisible());
        m1->moveToSideBar(dw1);
        QVERIFY(m1->anySideBarIsVisible());

        QVERIFY(!dw1->isOverlayed());
        QVERIFY(!dw1->isVisible());
        loc = dw1->sideBarLocation();
        QVERIFY(loc != SideBarLocation::None);

        serialized = saver.serializeLayout();

        m1.reset();
        delete fw1;
    }

    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    auto fw1 = dw1->window();
    m1->addDockWidget(dw1, Location_OnBottom);
    QVERIFY(!m1->anySideBarIsVisible());
    QVERIFY(!dw1->isOverlayed());
    QVERIFY(dw1->isVisible());
    QVERIFY(!dw1->isFloating());
    QVERIFY(dw1->isInMainWindow());

    LayoutSaver restorer;
    restorer.restoreLayout(serialized);

    QVERIFY(!dw1->isOverlayed());
    QVERIFY(!dw1->isVisible());
    QVERIFY(!dw1->isInMainWindow());
    QVERIFY(m1->anySideBarIsVisible());

    QCOMPARE(loc, dw1->sideBarLocation());

    restorer.restoreLayout(beforeSideBarSerialized);
    QVERIFY(!dw1->isOverlayed());
    QVERIFY(dw1->isVisible());
    QVERIFY(!dw1->isFloating());
    QVERIFY(dw1->isInMainWindow());
    QVERIFY(!m1->anySideBarIsVisible());

    delete fw1;
}

void TestDocks::tst_toggleActionOnSideBar()
{
    // When a dock widget is in the sidebar and we use DockWidget::toggleAction() then it should
    // toggle visibility without removing it from the sidebar

    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
    auto dw1 = new DockWidgetType("1");
    m1->addDockWidget(dw1, Location_OnBottom);
    dw1->moveToSideBar();

    QVERIFY(!dw1->isVisible());
    QVERIFY(!dw1->isOverlayed());
    QVERIFY(dw1->isInSideBar());
    QVERIFY(!dw1->isInMainWindow());

    QAction *action = dw1->toggleAction();
    action->trigger();

    QVERIFY(dw1->isVisible());
    QVERIFY(dw1->isOverlayed());
    QVERIFY(dw1->isInMainWindow());

    QVERIFY(dw1->isInSideBar());
    action->trigger();

    QVERIFY(!dw1->isOverlayed());
    QVERIFY(!dw1->isInMainWindow());

    QVERIFY(dw1->isInSideBar());
    QVERIFY(!dw1->isInMainWindow());
}

void TestDocks::tst_deleteOnCloseWhenOnSideBar()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    QPointer<DockWidgetBase> dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)), DockWidgetBase::Option_DeleteOnClose);
    m->addDockWidget(dock1, Location_OnLeft);

    dock1->moveToSideBar();
    QVERIFY(dock1);
    QVERIFY(dock1->isInSideBar());

    QTest::qWait(500);
    QVERIFY(dock1);
}

void TestDocks::tst_sidebarOverlayGetsHiddenOnClick()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);

    {
        // Case #1 click on another dockwidget should hide the overlay

        auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
        auto dw1 = new DockWidgetType(QStringLiteral("1"));
        auto dw2 = new DockWidgetType(QStringLiteral("2"));

        m1->addDockWidget(dw1, Location_OnBottom);
        m1->addDockWidget(dw2, Location_OnBottom);

        m1->moveToSideBar(dw1);
        m1->overlayOnSideBar(dw1);

        QVERIFY(dw1->isOverlayed());

        Tests::clickOn(dw2->mapToGlobal(dw2->rect().bottomLeft() + QPoint(5, -5)), dw2);
        QVERIFY(!dw1->isOverlayed());

        auto widget2 = new MyWidget("foo");
        dw2->setWidget(widget2);
        m1->overlayOnSideBar(dw1);
        QVERIFY(dw1->isOverlayed());

        Tests::clickOn(widget2->mapToGlobal(widget2->rect().bottomLeft() + QPoint(5, -5)), widget2);
        QVERIFY(!dw1->isOverlayed());

        m1.reset();
        delete dw1;
    }

    {
        // Case #1 click on empty main window space, should hide the overlay

        auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
        auto dw1 = new DockWidgetType(QStringLiteral("1"));

        m1->addDockWidget(dw1, Location_OnBottom);

        m1->moveToSideBar(dw1);
        m1->overlayOnSideBar(dw1);

        QVERIFY(dw1->isOverlayed());

        const QPoint localPt(100, 250);
        Tests::clickOn(m1->mapToGlobal(m1->rect().topLeft() + localPt), m1->childAt(localPt));
        QVERIFY(!dw1->isOverlayed());
    }
}

void TestDocks::tst_floatRemovesFromSideBar()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);

    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    m1->addDockWidget(dw1, Location_OnBottom);

    m1->moveToSideBar(dw1);
    m1->overlayOnSideBar(dw1);

    QVERIFY(dw1->isOverlayed());
    QVERIFY(!dw1->isFloating());
    QVERIFY(dw1->isInMainWindow());

    dw1->setFloating(true);
    QVERIFY(!dw1->isOverlayed());
    QVERIFY(dw1->isFloating());
    QVERIFY(!dw1->isInMainWindow());

    QCOMPARE(dw1->sideBarLocation(), SideBarLocation::None);

    // Also test a crash I got
    m1->addDockWidget(dw1, Location_OnBottom);

    auto tb = dw1->titleBar();
    QVERIFY(tb->isVisible());

    tb->onFloatClicked();
}

void TestDocks::tst_overlayedGeometryIsSaved()
{
    // Tests that after resizing an overlayed widget, and then hide+show, its size is preserved
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);

    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    m1->addDockWidget(dw1, Location_OnBottom);

    m1->moveToSideBar(dw1, SideBarLocation::North);
    m1->overlayOnSideBar(dw1);

    Frame *frame = dw1->dptr()->frame();
    QVERIFY(frame->isOverlayed());
    QCOMPARE(dw1->sideBarLocation(), SideBarLocation::North);
    QVERIFY(frame->height() > 0);

    const int newHeight = frame->height() + 300;
    frame->setHeight(newHeight);

    m1->toggleOverlayOnSideBar(dw1);
    m1->toggleOverlayOnSideBar(dw1);

    frame = dw1->dptr()->frame();
    QCOMPARE(frame->height(), newHeight);
}

void TestDocks::tst_overlayCrash()
{
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);

    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None, "MW1");
    auto dw1 = new DockWidgetType(QStringLiteral("1"));
    m1->addDockWidget(dw1, Location_OnBottom);

    auto dw2 = new DockWidgetType(QStringLiteral("2"));
    m1->addDockWidget(dw2, Location_OnBottom);

    m1->moveToSideBar(dw1);
    m1->toggleOverlayOnSideBar(dw1);

    dw1->close();

    auto tb = dw2->titleBar();
    QVERIFY(tb->isVisible());

    pressOn(tb->mapToGlobal(QPoint(5, 5)), tb);
}

void TestDocks::tst_embeddedMainWindow()
{
    EnsureTopLevelsDeleted e;
    // Tests a MainWindow which isn't a top-level window, but is embedded in another window
    EmbeddedWindow *window = createEmbeddedMainWindow(QSize(800, 800));

    auto dock1 = createDockWidget("1", new QPushButton("1"));
    window->mainWindow->addDockWidget(dock1, Location_OnTop);
    dock1->setFloating(true);
    auto dropArea = window->mainWindow->dropArea();
    auto fw = dock1->floatingWindow();

    dragFloatingWindowTo(fw, dropArea, DropIndicatorOverlayInterface::DropLocation_Left);

    auto layout = dropArea;
    QVERIFY(Testing::waitForDeleted(fw));
    QCOMPARE(layout->count(), 2); // 2, as it has the central frame
    QCOMPARE(layout->visibleCount(), 2);
    layout->checkSanity();

    delete window;
}

void TestDocks::tst_restoreEmbeddedMainWindow()
{
    EnsureTopLevelsDeleted e;
    // Tests a MainWindow which isn't a top-level window, but is embedded in another window
    EmbeddedWindow *window = createEmbeddedMainWindow(QSize(800, 800));

    auto dock1 = createDockWidget("1", new QPushButton("1"));
    window->mainWindow->addDockWidget(dock1, Location_OnTop);

    const QPoint originalPos(250, 250);
    const QSize originalSize = window->size();
    window->move(originalPos);

    LayoutSaver saver;
    QByteArray saved = saver.serializeLayout();
    QVERIFY(!saved.isEmpty());

    window->resize(555, 555);
    const QPoint newPos(500, 500);
    window->move(newPos);
    QVERIFY(saver.restoreLayout(saved));

    QCOMPARE(window->pos(), originalPos);
    QCOMPARE(window->size(), originalSize);
    window->mainWindow->layoutWidget()->checkSanity();

    delete window;
}

void TestDocks::tst_negativeAnchorPositionWhenEmbedded_data()
{
    QTest::addColumn<bool>("embedded");

    QTest::newRow("false") << false;
    QTest::newRow("true") << true;
}

void TestDocks::tst_negativeAnchorPositionWhenEmbedded()
{
    QFETCH(bool, embedded);
    EnsureTopLevelsDeleted e;

    MainWindowBase *m = nullptr;
    if (embedded) {
        auto em = createEmbeddedMainWindow(QSize(500, 500));
        m = em->mainWindow;
    } else {
        m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame).release();
        m->resize(QSize(500, 500));
        m->show();
    }
    auto layout = m->multiSplitter();

    auto w1 = new MyWidget2(QSize(400,400));
    auto w2 = new MyWidget2(QSize(400,400));
    auto d1 = createDockWidget("1", w1);
    auto d2 = createDockWidget("2", w2);
    auto d3 = createDockWidget("3", w2);

    m->addDockWidget(d1, Location_OnLeft);
    m->addDockWidget(d2, Location_OnLeft);
    m->addDockWidget(d3, Location_OnLeft);

    layout->checkSanity();

    delete m->window();
}

void TestDocks::tst_restoreResizesLayout()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidget(dock1, Location_OnLeft);

    LayoutSaver saver;
    QVERIFY(saver.saveToFile("layout_tst_restoreResizesLayout.json"));

    // Now resize the window, and then restore. The layout should have the new size

    auto layout = m->multiSplitter();
    m->resize(1050, 1050);
    QCOMPARE(m->size(), QSize(1050, 1050));

    LayoutSaver restorer(RestoreOption_RelativeToMainWindow);
    QVERIFY(restorer.restoreFromFile("layout_tst_restoreResizesLayout.json"));
    QVERIFY(layout->checkSanity());

    QCOMPARE(m->dropArea()->QWidgetAdapter::size(), layout->size());
    QVERIFY(layout->checkSanity());
}

void TestDocks::tst_restoreNonRelativeFloatingWindowGeometry()
{
    // Tests that disabling RelativeFloatingWindowGeometry works

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));

    // Also test that invisible dock doesn't change size
    auto dock2 = createDockWidget("2", new QPushButton("2"), {}, {}, /*show=*/false);

    LayoutSaver saver(RestoreOption_RelativeToMainWindow);
    saver.dptr()->m_restoreOptions.setFlag(InternalRestoreOption::RelativeFloatingWindowGeometry,
                                           false);

    const QByteArray saved = saver.serializeLayout();

    const QSize floatingWindowSize = dock1->window()->size();
    const QSize floatingWindowSize2 = dock2->window()->size();

    m->resize(m->width() * 2, m->height());
    saver.restoreLayout(saved);

    QVERIFY(dock2->isFloating());
    QVERIFY(!dock2->isOpen());

    QCOMPARE(dock1->window()->size(), floatingWindowSize);
    QCOMPARE(dock2->window()->size(), floatingWindowSize2);
}

void TestDocks::tst_maximumSizePolicy()
{
    EnsureTopLevelsDeleted e;

    auto widget = new MyWidget2();
    const int maxHeight = 250;
    widget->setMinimumSize(QSize(200, 200));
    widget->setSizeHint(QSize(250, maxHeight));
    widget->setSizePolicy({QSizePolicy::Preferred, QSizePolicy::Maximum});

    auto dock1 = createDockWidget("dock1", widget);
    dock1->show();
    dock1->window()->resize(QSize(500, 500));
    auto oldFw = Tests::make_qpointer(dock1->window());
    dock1->close();
    dock1->show();
    auto oldFw2 = dock1->window();


    const int tolerance = 50;
    QVERIFY(dock1->window()->height() <= maxHeight + tolerance); // +tolerance as the floating window is a bit bigger, due to margins etc.
    QVERIFY(dock1->height() <= maxHeight);

    auto m1 = createMainWindow();
    auto dock2 = createDockWidget("dock2", new QPushButton("foo"));
    m1->addDockWidget(dock2, Location_OnTop);
    m1->resize(2000, 3000);

    // Make the floating window big, and see if the suggested highlight is still small
    dock1->window()->resize(QSize(dock1->width(), 800));

    {
        WindowBeingDragged wbd1(dock1->floatingWindow());
        const QRect highlightRect = m1->multiSplitter()->rectForDrop(&wbd1, Location_OnBottom, nullptr);
        QVERIFY(highlightRect.height() <= maxHeight + tolerance);
    }

    // Now drop it, and check too
    m1->addDockWidget(dock1, Location_OnBottom);
    QVERIFY(dock1->height() <= maxHeight);

    delete oldFw.data();
    delete oldFw2;
}

void TestDocks::tst_minSizeChanges()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(600, 600), MainWindowOption_None);
    m->show();
    auto w1 = new MyWidget2(QSize(400,400));
    auto w2 = new MyWidget2(QSize(400,400));

    auto d1 = new DockWidgetType("1");
    d1->setWidget(w1);
    auto d2 = new DockWidgetType("2");
    d2->setWidget(w2);

    m->addDockWidget(d1, Location_OnTop);
    m->addDockWidget(d2, Location_OnTop, nullptr, InitialVisibilityOption::StartHidden);
    auto layout = m->multiSplitter();

    // 1. d2 is a placeholder, let's change its min size before showing it
    w2->setMinimumSize(QSize(800, 800));
    d2->show();

    Item *item2 = layout->itemForFrame(d2->dptr()->frame());

    QVERIFY(layout->checkSanity());

    Testing::waitForResize(m.get());

    QVERIFY(item2->width() >= 800);
    QVERIFY(item2->height() >= 800);
    QVERIFY(m->height() >= 1200);

    // 2. d1 is visible, let's change its min size
    w1->setMinimumSize(QSize(800, 800));

    Testing::waitForResize(m.get());
    layout->checkSanity();

    QVERIFY(m->height() >= 1600);

    // add a small one to the middle
    auto w3 = new MyWidget2(QSize(100,100));
    auto d3 = new DockWidgetType("3");
    d3->setWidget(w3);
    m->addDockWidget(d3, Location_OnTop, d1);
}

void TestDocks::tst_maxSizePropagates()
{
    // Tests that the DockWidget gets the min and max size of its guest widget
    EnsureTopLevelsDeleted e;
    auto dock1 = new DockWidgetType("dock1");

    auto w = new MyWidget2(QSize(200, 200));
    w->setMinimumSize(120, 120);
    w->setMaximumSize(500, 500);
    dock1->setWidget(w);
    dock1->show();

    QCOMPARE(Widget_qwidget::widgetMinSize(dock1), Widget_qwidget::widgetMinSize(w));
    QCOMPARE(dock1->maximumSize(), w->maximumSize());

    w->setMinimumSize(121, 121);
    w->setMaximumSize(501, 501);

    Testing::waitForEvent(w, QEvent::LayoutRequest);

    QCOMPARE(Widget_qwidget::widgetMinSize(dock1), Widget_qwidget::widgetMinSize(w));
    QCOMPARE(dock1->maximumSize(), w->maximumSize());

    // Now let's see if our Frame also has proper size-constraints
    Frame *frame = dock1->dptr()->frame();
    QCOMPARE(frame->maximumSize().expandedTo(w->maximumSize()), frame->maximumSize());

    delete dock1->window();
}

void TestDocks::tst_maxSizedFloatingWindow()
{
    // Tests that FloatingWindows get a proper max-size, if its dock widget has one
    EnsureTopLevelsDeleted e;
    auto dock1 = new DockWidgetType("dock1");
    auto dock2 = new DockWidgetType("dock2");
    auto w = new MyWidget("foo");
    w->setMinimumSize(120, 100);
    w->setMaximumSize(300, 300);
    dock1->setWidget(w);

    dock1->show();
    dock2->show();

    auto window1 = dock1->window();
    auto window2 = dock2->window();
    Testing::waitForEvent(window1, QEvent::LayoutRequest);

    QVERIFY(window1->maximumSize().width() < 500);
    QVERIFY(window1->maximumSize().height() < 500);
    QVERIFY(window2->maximumSize().width() > 500);
    QVERIFY(window2->maximumSize().height() > 500);

    auto hasMax = [window1] {
        const QSize max = window1->maximumSize();
        return max.width() < 500 && max.height() < 500;
    };

    // Adding side-by-side, we don't honour max size (yet)
    dock1->addDockWidgetToContainingWindow(dock2, Location_OnBottom);
    Testing::waitForEvent(window1, QEvent::LayoutRequest);
    QVERIFY(window1->maximumSize().width() > 500);
    QVERIFY(window1->maximumSize().height() > 500);

    // Close dw2, we have a single dock widget again, we honour max-size
    dock2->close();
    Testing::waitForEvent(window1, QEvent::LayoutRequest);
    QVERIFY(hasMax());

    dock1->addDockWidgetAsTab(dock2);
    Testing::waitForEvent(window1, QEvent::LayoutRequest);
    QVERIFY(!hasMax());

    dock2->close();
    Testing::waitForEvent(window1, QEvent::LayoutRequest);
    QVERIFY(hasMax());
}

void TestDocks::tst_maxSizeHonouredWhenAnotherDropped()
{
    // dock1 is docked, and has small max-height.
    // When dropping dock2, which is small too, dock2 should occupy all the height except dock1's max-height
    // i.e. dock2 should expand and eat all available space

    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dock1 = new DockWidgetType("dock1");

    auto w = new MyWidget2(QSize(400,400));
    w->setMinimumSize(120, 100);
    w->setMaximumSize(300, 150);
    dock1->setWidget(w);
    m1->addDockWidget(dock1, Location_OnLeft);

    auto dock2 = new DockWidgetType("dock2");
    m1->addDockWidget(dock2, Location_OnBottom);

    auto root = m1->multiSplitter()->rootItem();
    Separator *separator = root->separators().constFirst();
    const int min1 = root->minPosForSeparator_global(separator);
    const int max2 = root->maxPosForSeparator_global(separator);

    QVERIFY(separator->position() >= min1);
    QVERIFY(separator->position() <= max2);
    const int item1MaxHeight = dock1->dptr()->frame()->maxSizeHint().height();
    QVERIFY(dock1->dptr()->frame()->height() <= item1MaxHeight);
    root->dumpLayout();
    QCOMPARE(dock2->dptr()->frame()->height(),
             root->height() - item1MaxHeight - Item::separatorThickness);
}

void TestDocks::tst_addToHiddenMainWindow()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(1000, 1000), MainWindowOption_HasCentralFrame, {}, false);
    auto w1 = new MyWidget2(QSize(400,400));
    auto w2 = new MyWidget2(QSize(400,400));
    auto d1 = createDockWidget("1", w1);
    auto d2 = createDockWidget("2", w2);

    m->addDockWidget(d1, Location_OnTop);
    m->addDockWidget(d2, Location_OnTop);

    QVERIFY(!m->isVisible());
    d1->setFloating(true);
    d2->setFloating(false);
    m->layoutWidget()->checkSanity();
}

void TestDocks::tst_maxSizePropagates2()
{
    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dock1 = new DockWidgetType("dock1");

    auto w = new MyWidget2(QSize(200, 200));
    w->setMinimumSize(120, 120);
    w->setMaximumSize(300, 500);
    dock1->setWidget(w);
    dock1->show();

    auto dock2 = new DockWidgetType("dock2");
    auto dock3 = new DockWidgetType("dock3");
    auto dock4 = new DockWidgetType("dock4");
    m1->addDockWidget(dock2, Location_OnLeft);
    m1->addDockWidget(dock3, Location_OnRight);
    m1->addDockWidget(dock4, Location_OnBottom, dock3);
    m1->addDockWidget(dock1, Location_OnLeft, dock4);

    Frame *frame1 = dock1->dptr()->frame();

    Layouting::ItemBoxContainer *root = m1->multiSplitter()->rootItem();
    Item *item1 = root->itemForWidget(frame1);
    auto vertSep1 = root->separators().constFirst();
    const int min1 = root->minPosForSeparator_global(vertSep1);

    ItemBoxContainer *container1 = item1->parentBoxContainer();
    auto innerVertSep1 = container1->separators().constFirst();
    const int minInnerSep = container1->minPosForSeparator_global(innerVertSep1);
    const int maxInnerSep = container1->maxPosForSeparator_global(innerVertSep1);

    root->requestSeparatorMove(vertSep1, -(vertSep1->position() - min1));
    QVERIFY(frame1->width() <= frame1->maxSizeHint().width());

    container1->requestSeparatorMove(innerVertSep1, -(innerVertSep1->position() - minInnerSep));
    QVERIFY(frame1->width() <= frame1->maxSizeHint().width());

    container1->requestSeparatorMove(innerVertSep1, maxInnerSep - innerVertSep1->position());
    QVERIFY(frame1->width() <= frame1->maxSizeHint().width());
}

void TestDocks::tst_maxSizeHonouredWhenDropped()
{
    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow();
    auto dock1 = new DockWidgetType("dock1");
    auto dock2 = new DockWidgetType("dock2");
    m1->addDockWidget(dock1, Location_OnTop);
    m1->resize(2000, 2000);

    auto w2 = new MyWidget2(QSize(400,400));
    dock2->setWidget(w2);
    const int maxWidth = 200;
    w2->setMaximumSize(maxWidth, 200);
    m1->addDockWidget(dock2, Location_OnLeft);
    const int droppedWidth = dock2->dptr()->frame()->width();
    QVERIFY(droppedWidth < maxWidth + 50); // +50 to cover any margins and waste by QTabWidget

    // Try again, but now dropping a multisplitter
    dock2->setFloating(true);
    auto fw = dock2->floatingWindow();

    m1->dropArea()->drop(fw, Location_OnLeft, nullptr);
    QCOMPARE(dock2->dptr()->frame()->width(), droppedWidth);
}

void TestDocks::tst_fixedSizePolicy()
{
    // tests that KDDW also takes into account QSizePolicy::Fixed for calculating the max size hint.
    // Since QPushButton for example doesn't set QWidget::maximumSize(), but instead uses sizeHint()
    // + QSizePolicy::Fixed.
    EnsureTopLevelsDeleted e;
    auto button = new QPushButton("one");
    auto dock1 = createDockWidget("dock1", button);
    Frame *frame = dock1->dptr()->frame();

    // Just a precondition from the test. If QPushButton ever changes, replace with a QWidget and set fixed size policy
    QCOMPARE(button->sizePolicy().verticalPolicy(), QSizePolicy::Fixed);

    const int buttonMaxHeight = button->sizeHint().height();

    QCOMPARE(dock1->sizeHint(), button->sizeHint());
    QCOMPARE(dock1->sizePolicy().verticalPolicy(), button->sizePolicy().verticalPolicy());
    QCOMPARE(dock1->sizePolicy().horizontalPolicy(), button->sizePolicy().horizontalPolicy());

    QCOMPARE(frame->maxSizeHint().height(), qMax(buttonMaxHeight, Layouting::Item::hardcodedMinimumSize.height()));

    delete dock1->window();
}

#endif

void TestDocks::tst_floatingAction()
{
    // Tests DockWidget::floatAction()
    EnsureTopLevelsDeleted e;

    {
        // 1. Create a MainWindow with two docked dock-widgets, then float the first one.
        auto m = createMainWindow();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);

        auto action = dock1->floatAction();
        QVERIFY(!dock1->isFloating());
        QVERIFY(!action->isChecked());
        QVERIFY(action->isEnabled());
        QCOMPARE(action->toolTip(), tr("Detach"));

        action->toggle();

        QVERIFY(dock1->isFloating());
        QVERIFY(action->isChecked());
        QVERIFY(action->isEnabled());
        QCOMPARE(action->toolTip(), tr("Dock"));

        auto fw = dock1->floatingWindow();
        QVERIFY(fw);

        //2. Put it back, via setFloating(). It should return to its place.
        action->toggle();

        QVERIFY(!dock1->isFloating());
        QVERIFY(!action->isChecked());
        QVERIFY(action->isEnabled());
        QVERIFY(!dock1->isTabbed());
        QCOMPARE(action->toolTip(), tr("Detach"));;

        Testing::waitForDeleted(fw);
    }

    {
        // 1. Create a MainWindow with one docked dock-widgets, and one floating.
        auto m = createMainWindow();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);

        //The floating window action should be disabled as it has no previous place
        auto action = dock2->floatAction();
        QVERIFY(dock2->isFloating());
        QVERIFY(action->isChecked());
        QVERIFY(!action->isEnabled());
        QCOMPARE(action->toolTip(), tr("Dock"));

        m->addDockWidget(dock2, KDDockWidgets::Location_OnRight);

        QVERIFY(!dock2->isFloating());
        QVERIFY(!action->isChecked());
        QVERIFY(action->isEnabled());
        QCOMPARE(action->toolTip(), tr("Detach"));

        action->toggle();
        QVERIFY(dock2->isFloating());
        QVERIFY(action->isChecked());
        QVERIFY(action->isEnabled());
        QCOMPARE(action->toolTip(), tr("Dock"));

        auto fw = dock2->floatingWindow();
        QVERIFY(fw);

        //2. Put it back, via setFloating(). It should return to its place.
        action->toggle();

        QVERIFY(!dock1->isFloating());
        QVERIFY(!action->isChecked());
        QVERIFY(action->isEnabled());
        QVERIFY(!dock1->isTabbed());
        QCOMPARE(action->toolTip(), tr("Detach"));

        Testing::waitForDeleted(fw);
    }
    {
        // 3. A floating window with two tabs
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        bool dock1IsFloating = dock1->floatAction()->isChecked();
        bool dock2IsFloating = dock2->floatAction()->isChecked();

        connect(dock1->floatAction(), &QAction::toggled, [&dock1IsFloating] (bool t) {
            Q_ASSERT(dock1IsFloating != t);
            dock1IsFloating = t;
        });

        connect(dock2->floatAction(), &QAction::toggled, [&dock2IsFloating] (bool t) {
            Q_ASSERT(dock2IsFloating != t);
            dock2IsFloating = t;
        });

        auto fw2 = dock2->floatingWindow();
        QVERIFY(dock1->isFloating());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock1->floatAction()->isChecked());
        QVERIFY(dock2->floatAction()->isChecked());

        dock1->addDockWidgetAsTab(dock2);
        QVERIFY(!dock1->isFloating());
        QVERIFY(!dock2->isFloating());
        QVERIFY(!dock1->floatAction()->isChecked());
        QVERIFY(!dock2->floatAction()->isChecked());

        dock2->setFloating(true);

        QVERIFY(dock1->isFloating());
        QVERIFY(dock1->floatAction()->isChecked());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock2->floatAction()->isChecked());

        QVERIFY(dock1IsFloating);
        QVERIFY(dock2IsFloating);

        delete fw2;
        delete dock1->window();
        delete dock2->window();
    }

    {
        // If the dock widget is alone then it's floating, but we suddenly dock a widget side-by-side
        // to it, then both aren't floating anymore. This test tests if the signal was emitted

        auto dock1 = createDockWidget("one", new QPushButton("one"));
        auto dock2 = createDockWidget("two", new QPushButton("two"));

        QVERIFY(dock1->isFloating());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock1->floatAction()->isChecked());
        QVERIFY(dock2->floatAction()->isChecked());
        auto oldFw2 = dock2->window();

        QSignalSpy spy1(dock1->floatAction(), &QAction::toggled);
        QSignalSpy spy2(dock2->floatAction(), &QAction::toggled);

        QSignalSpy spy11(dock1, &DockWidgetBase::isFloatingChanged);
        QSignalSpy spy21(dock2, &DockWidgetBase::isFloatingChanged);

        dock1->addDockWidgetToContainingWindow(dock2, Location_OnRight);

        QCOMPARE(spy1.count(), 1);
        QCOMPARE(spy2.count(), 1);
        QCOMPARE(spy11.count(), 1);
        QCOMPARE(spy21.count(), 1);

        QVERIFY(!dock1->isFloating());
        QVERIFY(!dock2->isFloating());

        QVERIFY(!dock2->floatAction()->isChecked());
        QVERIFY(!dock1->floatAction()->isChecked());

        delete dock1->window();
        delete oldFw2->window();
    }

    {
        // Like before, but now we use addMultiSplitter()

        auto dock1 = createDockWidget("one", new QPushButton("one"));
        auto dock2 = createDockWidget("two", new QPushButton("two"));

        QVERIFY(dock1->isFloating());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock1->floatAction()->isChecked());
        QVERIFY(dock2->floatAction()->isChecked());
        auto oldFw2 = dock2->floatingWindow();

        QSignalSpy spy1(dock1->floatAction(), &QAction::toggled);
        QSignalSpy spy2(dock2->floatAction(), &QAction::toggled);

        QSignalSpy spy11(dock1, &DockWidgetBase::isFloatingChanged);
        QSignalSpy spy21(dock2, &DockWidgetBase::isFloatingChanged);

        auto dropArea1 = dock1->floatingWindow()->dropArea();
        dropArea1->drop(oldFw2, Location_OnRight, nullptr);

        QCOMPARE(spy1.count(), 1);
        QCOMPARE(spy2.count(), 1);
        QCOMPARE(spy11.count(), 1);
        QCOMPARE(spy21.count(), 1);

        QVERIFY(!dock1->isFloating());
        QVERIFY(!dock2->isFloating());
        QVERIFY(!dock2->floatAction()->isChecked());
        QVERIFY(!dock1->floatAction()->isChecked());

        // Let's now remove dock1, dock2 should be floating
        dock1->setFloating(true);
        QVERIFY(dock1->isFloating());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock2->floatAction()->isChecked());
        QVERIFY(dock1->floatAction()->isChecked());

        delete dock1->window();
        delete dock2->window();
        delete oldFw2->window();
    }

    {
        // Same test as before, but now tab instead of side-by-side
        auto dock1 = createDockWidget("one", new QPushButton("one"));
        auto dock2 = createDockWidget("two", new QPushButton("two"));

        QVERIFY(dock1->isFloating());
        QVERIFY(dock2->isFloating());
        QVERIFY(dock1->floatAction()->isChecked());
        QVERIFY(dock2->floatAction()->isChecked());
        auto oldFw2 = dock2->window();

        QSignalSpy spy1(dock1->floatAction(), &QAction::toggled);
        QSignalSpy spy2(dock2->floatAction(), &QAction::toggled);
        QSignalSpy spy11(dock1, &DockWidgetBase::isFloatingChanged);
        QSignalSpy spy21(dock2, &DockWidgetBase::isFloatingChanged);
        dock1->addDockWidgetAsTab(dock2);

        QCOMPARE(spy1.count(), 1);

        // On earlier Qt versions this is flaky, but technically correct.
        // Windows can get hidden while being reparented and floating changes momentarily.
        QVERIFY(spy2.count() == 1 || spy2.count() == 3);
        QVERIFY(spy21.count() == 1 || spy21.count() == 3);
        QCOMPARE(spy11.count(), 1);

        QVERIFY(!dock1->isFloating());
        QVERIFY(!dock2->isFloating());

        QVERIFY(!dock2->floatAction()->isChecked());
        QVERIFY(!dock1->floatAction()->isChecked());

        delete dock1->window();
        delete oldFw2->window();
    }
}

void TestDocks::tst_raise()
{
    // Tests DockWidget::raise();
    EnsureTopLevelsDeleted e;

    auto dock1 = createDockWidget("1");
    auto dock2 = createDockWidget("2");
    auto fw2 = Tests::make_qpointer(dock2->window());
    dock1->addDockWidgetAsTab(dock2);
    dock1->setAsCurrentTab();
    QVERIFY(dock1->isCurrentTab());
    QVERIFY(!dock2->isCurrentTab());
    dock2->raise();
    QVERIFY(!dock1->isCurrentTab());
    QVERIFY(dock2->isCurrentTab());

    if (qApp->platformName() != QLatin1String("offscreen")) { // offscreen qpa doesn't seem to keep Window Z.
        auto dock3 = createDockWidget("3");
        dock3->window()->setGeometry(dock1->window()->geometry());
        dock3->window()->setObjectName("3");
        dock1->window()->setObjectName("1");
        dock3->raise();
        QTest::qWait(200);

        if (qApp->QGuiApplication::topLevelAt(dock3->window()->geometry().topLeft() + QPoint(50, 50)) != dock3->windowHandle()) {
            qDebug() << "Failing before raise" << qApp->widgetAt(dock3->window()->geometry().topLeft() + QPoint(50, 50))->window() << dock3->window()
                     << dock1->window()->geometry() << dock3->window()->geometry();
            QVERIFY(false);
        }

        dock1->raise();
        QTest::qWait(200);
        QVERIFY(dock1->isCurrentTab());

        if (qApp->QGuiApplication::topLevelAt(dock3->window()->geometry().topLeft() + QPoint(50, 50)) != dock1->windowHandle()) {
            qDebug() << "Failing after raise" << qApp->widgetAt(dock3->window()->geometry().topLeft() + QPoint(50, 50))->window() << dock1->window()
                     << dock1->window()->geometry() << dock3->window()->geometry();
            QVERIFY(false);
        }

        delete dock3->window();
    }

    delete fw2;
    delete dock1->window();
}

void TestDocks::tst_invalidLayoutAfterRestore()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    auto dock3 = createDockWidget("dock3", new QPushButton("three"));
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;
    // Stack 1, 2, 3
    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);
    m->addDockWidget(dock3, Location_OnRight);

    const int oldContentsWidth = layout->width();

    auto f1 = dock1->dptr()->frame();
    dock3->close();
    dock2->close();
    dock1->close();
    QVERIFY(Testing::waitForDeleted(f1));

    dock3->show();
    dock2->show();
    dock1->show();
    Testing::waitForEvent(m.get(), QEvent::LayoutRequest); // So MainWindow min size is updated

    Item *item1 = layout->itemForFrame(dock1->dptr()->frame());
    Item *item3 = layout->itemForFrame(dock3->dptr()->frame());
    Item *item4 = dropArea->centralFrame();

    QCOMPARE(layout->count(), 4);
    QCOMPARE(layout->placeholderCount(), 0);

    // Detach dock2
    QPointer<Frame> f2 = dock2->dptr()->frame();
    f2->detachTab(dock2);
    QVERIFY(!f2.data());
    QTest::qWait(200); // Not sure why. Some event we're waiting for. TODO: Investigate
    auto fw2 = dock2->floatingWindow();
    QCOMPARE(layout->minimumSize().width(), 2*Item::separatorThickness + item1->minSize().width() + item3->minSize().width() + item4->minSize().width());

    // Drop left of dock3
    layout->addWidget(fw2->dropArea(), Location_OnLeft, dock3->dptr()->frame());

    QVERIFY(Testing::waitForDeleted(fw2));
    QCOMPARE(layout->width(), oldContentsWidth);
    layout->checkSanity();
}

void TestDocks::tst_dontCloseDockWidgetBeforeRestore()
{
      EnsureTopLevelsDeleted e;
      auto m = createMainWindow();
      auto dock1 = createDockWidget("dock1", new QPushButton("one"));
      auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, DockWidgetBase::LayoutSaverOption::Skip);
      auto dock3 = createDockWidget("dock3", new QPushButton("three"), {}, DockWidgetBase::LayoutSaverOption::Skip);
      auto dock4 = createDockWidget("4", new QPushButton("4"), {}, {}, /*show=*/ false);

      m->addDockWidget(dock1, Location_OnBottom);
      m->addDockWidget(dock2, Location_OnBottom);

      // Dock #3 floats, while #1 and #2 are docked.
      dock3->setFloating(true);
      QVERIFY(dock3->isOpen());
      QVERIFY(dock3->isFloating());
      QVERIFY(!dock3->isInMainWindow());

      LayoutSaver saver;
      const QByteArray saved = saver.serializeLayout();
      dock3->close();

      // Not open anymore
      QVERIFY(!dock3->isOpen());

      QVERIFY(saver.restoreLayout(saved));

      // #3 is still closed, the restore will skip it
      QVERIFY(!dock3->isOpen());
      QVERIFY(!dock3->isInMainWindow());

      auto dock5 = createDockWidget("5", new QPushButton("5"), {}, DockWidgetBase::LayoutSaverOption::Skip);

      dock4->show();
      dock5->show();

      QVERIFY(saver.restoreLayout(saved));
      QVERIFY(!dock4->isOpen());
      QVERIFY(dock5->isOpen()); // #5 is still open, it ignored restore
}

void TestDocks::tst_dontCloseDockWidgetBeforeRestore2()
{
    // In this case we have a floating window with two dock widgets tabbed, both having LayoutSaverOption::Skip
    // Meaning the whole window should be skipped

    EnsureTopLevelsDeleted e;
    auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, DockWidgetBase::LayoutSaverOption::Skip);
    auto dock3 = createDockWidget("dock3", new QPushButton("three"), {}, DockWidgetBase::LayoutSaverOption::Skip);

    dock2->close();
    dock3->close();

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout(); // This layout has 0 docks visible

    dock2->show();
    dock3->show();
    QVERIFY(saver.restoreLayout(saved));
    QVERIFY(dock2->isVisible()); // They're still visible
    QVERIFY(dock3->isVisible());

    // Now tab and restore again
    dock2->addDockWidgetAsTab(dock3);
    QVERIFY(saver.restoreLayout(saved));
    QVERIFY(dock2->isOpen());
    QVERIFY(dock3->isOpen());
    QVERIFY(dock3->isVisible());
    QCOMPARE(dock3->dptr()->frame(), dock2->dptr()->frame());
}

void TestDocks::tst_dontCloseDockWidgetBeforeRestore3()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"), {}, DockWidgetBase::LayoutSaverOption::Skip);
    dock1->close();
    dock2->close();

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout(); // This layout has 0 docks visible

    m->addDockWidget(dock1, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnBottom);

    QVERIFY(saver.restoreLayout(saved));

    QVERIFY(!dock1->isOpen()); // Gets closed by the restore
    QVERIFY(dock2->isOpen()); // Dock2 remains open, it ignores restore
    QVERIFY(dock2->isFloating());
}

void TestDocks::tst_dontCloseDockWidgetBeforeRestore4()
{
    // Tests a case where the dock widget would get an invalid size.
    // Widgets which skip layout restore were be skipping LayoutSaver::onResize()

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow({1000, 1000}, {});
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"), {},
                                  DockWidgetBase::LayoutSaverOption::Skip);

    m->addDockWidget(dock1, Location_OnBottom);
    m->addDockWidget(dock2, Location_OnBottom);

    QTest::qWait(100);
    dock1->close();
    dock2->close();

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();

    QTest::qWait(100);
    dock2->show();

    QVERIFY(saver.restoreLayout(saved));
    QVERIFY(dock2->isOpen());

    QTest::qWait(100);
    FloatingWindow *fw = dock2->floatingWindow();
    DropArea *da = fw->dropArea();
    QVERIFY(da->checkSanity());
    QCOMPARE(da->size(), da->rootItem()->size());
    QVERIFY(qAbs(fw->width() - da->width()) < 30);
}

void TestDocks::tst_closeOnlyCurrentTab()
{
    {
        // Case of a floating window with tabs
        EnsureTopLevelsDeleted e;
        KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_CloseOnlyCurrentTab);

        auto dock1 = createDockWidget("1", new QPushButton("1"));
        auto dock2 = createDockWidget("2", new QPushButton("2"));
        auto dock3 = createDockWidget("3", new QPushButton("3"));

        /// Floating window with 3 tabs
        dock1->addDockWidgetAsTab(dock2);
        dock1->addDockWidgetAsTab(dock3);

        TitleBar *tb = dock1->titleBar();
        QVERIFY(tb->isVisible());
        dock1->setAsCurrentTab();
        Frame *frame = dock1->dptr()->frame();
        QCOMPARE(frame->currentIndex(), 0);

        tb->onCloseClicked();

        QVERIFY(!dock1->isOpen());
        QVERIFY(dock2->isOpen());
        QVERIFY(dock3->isOpen());
    }

    {
        // Case of a floating window with tabs
        EnsureTopLevelsDeleted e;
        KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_CloseOnlyCurrentTab);

        auto m = createMainWindow();
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        auto dock2 = createDockWidget("2", new QPushButton("2"));
        auto dock3 = createDockWidget("3", new QPushButton("3"));

        m->addDockWidget(dock1, Location_OnLeft);
        m->addDockWidget(dock2, Location_OnRight);

        dock2->addDockWidgetAsTab(dock3);
        Frame *frame = dock2->dptr()->frame();
        QCOMPARE(frame->currentIndex(), 1);
        TitleBar *tb = frame->titleBar();
        QVERIFY(tb->isVisible());
        tb->onCloseClicked();

        QVERIFY(!dock3->isOpen());
        QVERIFY(dock2->isOpen());
        QVERIFY(dock1->isOpen());
        QCOMPARE(frame->dockWidgetCount(), 1);
    }

}

void TestDocks::tst_tabWidgetCurrentIndex()
{
    EnsureTopLevelsDeleted e;

    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    auto dock3 = createDockWidget("3", new QPushButton("3"));
    auto fw2 = dock2->window();
    auto fw3 = dock3->window();

    DockWidgetBase *currentDw = nullptr;
    auto frame = dock1->dptr()->frame();
    connect(frame, &Frame::currentDockWidgetChanged, this, [&currentDw] (DockWidgetBase *dw){
        currentDw = dw;
    });

    QCOMPARE(frame->tabWidget()->currentIndex(), 0);
    dock1->addDockWidgetAsTab(dock2);

    QCOMPARE(frame->tabWidget()->currentIndex(), 1);
    QCOMPARE(frame->currentDockWidget(), currentDw);
    QCOMPARE(dock2, currentDw);

    dock2->close();

    QCOMPARE(frame->tabWidget()->currentIndex(), 0);
    QCOMPARE(dock1, currentDw);

    delete fw2;
    delete fw3;
    delete dock2;
    delete dock1->window();
}

void TestDocks::tst_doubleClickTabToDetach()
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    // Tests::doubleClickOn(QWindow) doesn't work anymore on Qt6
    // which refactored mouse delivery.
    return;
#endif

    EnsureTopLevelsDeleted e;

    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));

    auto fw2 = dock2->window();

    dock1->addDockWidgetAsTab(dock2);

    auto frame = dock1->dptr()->frame();
    QCOMPARE(frame->currentIndex(), 1);

    auto tb = frame->tabWidget()->asWidget();

    Tests::doubleClickOn(tb->mapToGlobal(QPoint(20, 20)), frame->window()->windowHandle());

    QVERIFY(dock1->isFloating());
    QVERIFY(dock2->isFloating());
    QVERIFY(dock1->floatingWindow() != dock2->floatingWindow());

    delete fw2;
    delete dock1->window();
    delete dock2->window();
}

void TestDocks::tst_addingOptionHiddenTabbed()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(501, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));
    m->addDockWidget(dock1, Location_OnTop);

    QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 1);
    dock1->addDockWidgetAsTab(dock2, InitialVisibilityOption::StartHidden);
    QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 1);
    dock2->show();
    QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);

    QVERIFY(dock1->dptr()->frame() == dock2->dptr()->frame());
}

void TestDocks::tst_flagDoubleClick()
{
    {
        EnsureTopLevelsDeleted e;
        KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_DoubleClickMaximizes);
        auto m = createMainWindow(QSize(500, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("1", new QPushButton("1"));
        auto dock2 = createDockWidget("2", new QPushButton("2"));
        m->addDockWidget(dock1, Location_OnTop);

        FloatingWindow *fw2 = dock2->floatingWindow();
        QVERIFY(!fw2->isMaximized());
        TitleBar *t2 = dock2->titleBar();
        QPoint pos = t2->mapToGlobal(QPoint(5, 5));
        Tests::doubleClickOn(pos, t2);
        QVERIFY(fw2->isMaximized());
        delete fw2;

        TitleBar *t1 = dock1->titleBar();
        QVERIFY(!t1->isFloating());
        pos = t1->mapToGlobal(QPoint(5, 5));
        Tests::doubleClickOn(pos, t1);
        QVERIFY(t1->isFloating());
        QVERIFY(!dock1->window()->isMaximized());
        delete dock1->window();
    }

    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(500, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("1", new QPushButton("1"));

        m->addDockWidget(dock1, Location_OnTop);

        TitleBar *t1 = dock1->titleBar();
        QVERIFY(!t1->isFloating());
        QPoint pos = t1->mapToGlobal(QPoint(5, 5));
        Tests::doubleClickOn(pos, t1);
        QVERIFY(t1->isFloating());
        QVERIFY(dock1->isFloating());
        QVERIFY(!dock1->window()->isMaximized());

        pos = t1->mapToGlobal(QPoint(5, 5));
        Tests::doubleClickOn(pos, t1);
        QVERIFY(!dock1->isFloating());
    }
}

void TestDocks::tst_maxSizedHonouredAfterRemoved()
{
    EnsureTopLevelsDeleted e;
    auto m1 = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dock1 = new DockWidgetType("dock1");
    dock1->show();

    auto w = new MyWidget("foo");
    w->setMinimumSize(120, 100);
    w->setMaximumSize(300, 150);
    dock1->setWidget(w);
    m1->dropArea()->addMultiSplitter(dock1->floatingWindow()->multiSplitter(), Location_OnLeft);

    auto dock2 = new DockWidgetType("dock2");
    dock2->show();
    m1->dropArea()->addMultiSplitter(dock2->floatingWindow()->multiSplitter(), Location_OnTop);

    auto root = m1->multiSplitter()->rootItem();

    // Wait 1 event loop so we get layout invalidated and get max-size constraints
    QTest::qWait(10);

    auto sep = root->separators().constFirst();
    root->requestEqualSize(sep); // Since we're not calling honourMaxSizes() after a widget changes its max size afterwards yet
    const int sepMin = root->minPosForSeparator_global(sep);
    const int sepMax = root->maxPosForSeparator_global(sep);

    QVERIFY(sep->position() >= sepMin);
    QVERIFY(sep->position() <= sepMax);

    auto dock3 = new DockWidgetType("dock3");
    dock3->show();
    m1->dropArea()->addMultiSplitter(dock3->floatingWindow()->multiSplitter(), Location_OnBottom);

    dock1->setFloating(true);
    m1->dropArea()->addMultiSplitter(dock1->floatingWindow()->multiSplitter(), Location_OnBottom,
                                     dock2->dptr()->frame());

    // Close dock2 and check if dock1's max-size is still honoured
    dock2->close();
    QTest::qWait(100); // wait for the resize, so dock1 gets taller"

    QVERIFY(dock1->dptr()->frame()->height() <= dock1->dptr()->frame()->maxSizeHint().height());
    delete dock2;
}

void TestDocks::tst_addDockWidgetAsTabToDockWidget()
{
    EnsureTopLevelsDeleted e;
    {
        // Dock into a non-morphed floating dock widget
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        dock1->addDockWidgetAsTab(dock2);

        auto window1 = dock1->window();
        auto window2 = dock2->window();
        QCOMPARE(window1, window2);
        QCOMPARE(dock1->dptr()->frame(), dock2->dptr()->frame());
        QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
        dock1->deleteLater();
        dock2->deleteLater();
        Testing::waitForDeleted(dock2);
    }
    {
        // Dock into a morphed dock widget
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        dock1->dptr()->morphIntoFloatingWindow();
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));

        dock1->addDockWidgetAsTab(dock2);

        auto window1 = dock1->window();
        auto window2 = dock2->window();
        QCOMPARE(window1, window2);
        QCOMPARE(dock1->dptr()->frame(), dock2->dptr()->frame());
        QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
        dock1->deleteLater();
        dock2->deleteLater();
        Testing::waitForDeleted(dock2);
    }
    {
        // Dock a morphed dock widget into a morphed dock widget
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        dock1->dptr()->morphIntoFloatingWindow();
        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        dock2->dptr()->morphIntoFloatingWindow();
        auto originalWindow2 = Tests::make_qpointer(dock2->window());

        dock1->addDockWidgetAsTab(dock2);

        auto window1 = dock1->window();
        auto window2 = dock2->window();
        QCOMPARE(window1, window2);
        QCOMPARE(dock1->dptr()->frame(), dock2->dptr()->frame());
        QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
        Testing::waitForDeleted(originalWindow2);
        QVERIFY(!originalWindow2);
        dock1->deleteLater();
        dock2->deleteLater();
        Testing::waitForDeleted(dock2);
    }
    {
        // Dock to an already docked widget
        auto m = createMainWindow();
        auto dropArea = m->dropArea();
        auto dock1 = createDockWidget("dock1", new QPushButton("one"));
        nestDockWidget(dock1, dropArea, nullptr, KDDockWidgets::Location_OnLeft);

        auto dock2 = createDockWidget("dock2", new QPushButton("two"));
        dock1->addDockWidgetAsTab(dock2);
        QCOMPARE(dock1->window(), m.get());
        QCOMPARE(dock2->window(), m.get());
        QCOMPARE(dock1->dptr()->frame(), dock2->dptr()->frame());
        QCOMPARE(dock1->dptr()->frame()->dockWidgetCount(), 2);
    }
}

void TestDocks::tst_closeTabHidesDockWidget()
{
    // Tests that closing some tabbed dock widgets will hide them
    // QtQuick had a bug where they would still be visible
    {
        EnsureTopLevelsDeleted e;
        auto dock1 = createDockWidget("doc1", Qt::green);
        auto dock2 = createDockWidget("doc2", Qt::green);
        auto dock3 = createDockWidget("doc3", Qt::green);

        dock1->addDockWidgetAsTab(dock2);
        dock1->addDockWidgetAsTab(dock3);

        dock1->forceClose();
        QVERIFY(!dock1->isOpen());
        QVERIFY(!dock1->isVisible());

        dock2->forceClose();
        QVERIFY(!dock2->isOpen());
        QVERIFY(!dock2->isVisible());

        dock3->forceClose();
        QVERIFY(!dock3->isOpen());
        QVERIFY(!dock3->isVisible());
    }

    {
        EnsureTopLevelsDeleted e;
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        auto dock1 = createDockWidget("doc1", Qt::green);
        auto dock2 = createDockWidget("doc2", Qt::green);
        auto dock3 = createDockWidget("doc3", Qt::green);
        m->addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
        m->addDockWidget(dock2, KDDockWidgets::Location_OnLeft);
        m->addDockWidget(dock3, KDDockWidgets::Location_OnLeft);

        dock2->addDockWidgetAsTab(dock1);
        dock2->addDockWidgetAsTab(dock3);

        dock1->close();
        QVERIFY(!dock1->isOpen());
        QVERIFY(!dock1->isVisible());
        QCOMPARE(dock1->parent(), nullptr);

        dock2->close();
        QVERIFY(!dock2->isOpen());
        QCOMPARE(dock2->parent(), nullptr);
        QVERIFY(!dock2->isVisible());

        dock3->close();
        QVERIFY(!dock3->isOpen());
        QVERIFY(!dock3->isVisible());
        QCOMPARE(dock3->parent(), nullptr);
        QVERIFY(!dock2->isVisible());
    }
}

void TestDocks::tst_close()
{
    EnsureTopLevelsDeleted e;

    // 1.0 Call QWidget::close() on QDockWidget
    auto dock1 = createDockWidget("doc1", Qt::green);
    QAction *toggleAction = dock1->toggleAction();
    QVERIFY(toggleAction->isChecked());

    QVERIFY(dock1->close());

    QVERIFY(!dock1->isVisible());
    QVERIFY(!dock1->window()->isVisible());
    QCOMPARE(dock1->window(), dock1);
    QVERIFY(!toggleAction->isChecked());

    // 1.1 Reshow with show()
    dock1->show();
    auto fw = dock1->floatingWindow();
    QVERIFY(fw);
    QVERIFY(toggleAction->isChecked());
    QVERIFY(dock1->isVisible());
    QCOMPARE(dock1->window(), fw);
    QVERIFY(toggleAction->isChecked());

    // 1.2 Reshow with toggleAction instead
    QVERIFY(dock1->close());
    QVERIFY(!toggleAction->isChecked());
    QVERIFY(!dock1->isVisible());
    toggleAction->setChecked(true);
    QVERIFY(dock1->isVisible());

    // 1.3 Use hide() instead
    auto fw1 = dock1->floatingWindow();
    QVERIFY(fw1);

    dock1->close(); // TODO: Hide doesn't delete the FloatingWindow

    QVERIFY(Testing::waitForDeleted(fw1));
    QVERIFY(!dock1->isVisible());
    QVERIFY(!dock1->window()->isVisible());
    QCOMPARE(dock1->window(), dock1);
    QVERIFY(!toggleAction->isChecked());

    // 1.4 close a FloatingWindow, via DockWidget::close
    QPointer<FloatingWindow> window = dock1->dptr()->morphIntoFloatingWindow();
    QPointer<Frame> frame1 = dock1->dptr()->frame();
    QVERIFY(dock1->isVisible());
    QVERIFY(dock1->window()->isVisible());
    QVERIFY(frame1->QWidgetAdapter::isVisible());
    QCOMPARE(dock1->window(), window.data());

    QVERIFY(dock1->close());
    QVERIFY(!dock1->dptr()->frame());
    QVERIFY(Testing::waitForDeleted(frame1));
    QVERIFY(Testing::waitForDeleted(window));

    // 1.5 close a FloatingWindow, via FloatingWindow::close
    dock1->show();

    window = dock1->dptr()->morphIntoFloatingWindow();
    frame1 = dock1->dptr()->frame();
    QVERIFY(dock1->isVisible());
    QVERIFY(dock1->window()->isVisible());
    QVERIFY(frame1->QWidgetAdapter::isVisible());
    QCOMPARE(dock1->window(), window.data());

    QVERIFY(window->close());

    QVERIFY(!dock1->dptr()->frame());
    QVERIFY(Testing::waitForDeleted(frame1));
    QVERIFY(Testing::waitForDeleted(window));

    // TODO: 1.6 Test FloatingWindow with two frames
    // TODO: 1.7 Test Frame with two tabs

    // 1.8 Check if space is reclaimed after closing left dock
    DockWidgetBase *centralDock;
    DockWidgetBase *leftDock;
    DockWidgetBase *rightDock;

    auto mainwindow = createSimpleNestedMainWindow(&centralDock, &leftDock, &rightDock);
    auto da = mainwindow->dropArea();

    QVERIFY(da->checkSanity());
    QCOMPARE(leftDock->dptr()->frame()->QWidgetAdapter::x(), 0);

    QCOMPARE(centralDock->dptr()->frame()->QWidgetAdapter::x(),
             leftDock->dptr()->frame()->QWidgetAdapter::geometry().right()
                 + Item::separatorThickness + 1);
    QCOMPARE(rightDock->dptr()->frame()->QWidgetAdapter::x(),
             centralDock->dptr()->frame()->QWidgetAdapter::geometry().right()
                 + Item::separatorThickness + 1);
    leftDock->close();
    QTest::qWait(250); // TODO: wait for some signal
    QCOMPARE(centralDock->dptr()->frame()->QWidgetAdapter::x(), 0);
    QCOMPARE(rightDock->dptr()->frame()->QWidgetAdapter::x(),
             centralDock->dptr()->frame()->QWidgetAdapter::geometry().right()
                 + Item::separatorThickness + 1);

    rightDock->close();
    QTest::qWait(250); // TODO: wait for some signal
    QMargins margins = mainwindow->centerWidgetMargins();
    QCOMPARE(centralDock->dptr()->frame()->width(),
             mainwindow->width() - 0 * 2 - margins.left() - margins.right());
    delete leftDock; delete rightDock; delete centralDock;

    // 1.9 Close tabbed dock, side docks will maintain their position
    mainwindow = createSimpleNestedMainWindow(&centralDock, &leftDock, &rightDock);
    const int leftX = leftDock->dptr()->frame()->QWidgetAdapter::x();
    const int rightX = rightDock->dptr()->frame()->QWidgetAdapter::x();

    centralDock->close();

    QCOMPARE(leftDock->dptr()->frame()->QWidgetAdapter::x(), leftX);
    QCOMPARE(rightDock->dptr()->frame()->QWidgetAdapter::x(), rightX);
    delete leftDock; delete rightDock; delete centralDock;
    delete dock1;


    // 2. Test that closing the single frame of a main window doesn't close the main window itself
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
        QPointer<MainWindowBase> mainWindowPtr = m.get();
        dock1 = createDockWidget("hello", Qt::green);
        m->addDockWidget(dock1, Location_OnLeft);

        // 2.2 Closing should not close the main window
        dock1->close();
        QVERIFY(mainWindowPtr.data());
        delete dock1;
    }

    // 2.1 Test closing the frame instead
    {
        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None); // Remove central frame
        QPointer<MainWindowBase> mainWindowPtr = m.get();
        dock1 = createDockWidget("hello", Qt::green);
        m->addDockWidget(dock1, Location_OnLeft);

        // 2.2 Closing should not close the main window
        dock1->dptr()->frame()->titleBar()->onCloseClicked();
        QVERIFY(mainWindowPtr.data());
        QVERIFY(mainWindowPtr->isVisible());
        delete dock1;
    }

    // 2.2 Repeat, but with a central frame
    {
        auto m = createMainWindow(QSize(800, 500));
        QPointer<MainWindowBase> mainWindowPtr = m.get();
        dock1 = createDockWidget("hello", Qt::green);
        m->addDockWidget(dock1, Location_OnLeft);

        // 2.2 Closing should not close the main window
        dock1->dptr()->frame()->titleBar()->onCloseClicked();
        QVERIFY(mainWindowPtr.data());
        QVERIFY(mainWindowPtr->isVisible());
        delete dock1;
    }
}

void TestDocks::tst_propagateSizeHonoursMinSize()
{
    // Here we dock a widget on the left size, and on the right side.
    // When docking the second one, the 1st one shouldn't be squeezed too much, as it has a min size

    EnsureTopLevelsDeleted e;

    auto m = createMainWindow();
    auto dock1 = createDockWidget("dock1", new QPushButton("one"));
    auto dock2 = createDockWidget("dock2", new QPushButton("two"));
    auto dropArea = m->dropArea();
    int min1 = widgetMinLength(dock1, Qt::Horizontal);
    int min2 = widgetMinLength(dock2, Qt::Horizontal);

    QVERIFY(dock1->width() >= min1);
    QVERIFY(dock2->width() >= min2);

    nestDockWidget(dock1, dropArea, nullptr, KDDockWidgets::Location_OnRight);
    nestDockWidget(dock2, dropArea, nullptr, KDDockWidgets::Location_OnLeft);

    // Calculate again, as the window frame has disappeared
    min1 = widgetMinLength(dock1, Qt::Horizontal);
    min2 = widgetMinLength(dock2, Qt::Horizontal);

    auto l = m->dropArea();
    l->checkSanity();

    if (dock1->width() < min1) {
        qDebug() << "\ndock1->width()=" << dock1->width() << "\nmin1=" << min1
                 << "\ndock min sizes=" << dock1->minimumWidth() << dock1->minimumSizeHint().width()
                 << "\nframe1->width()=" << dock1->dptr()->frame()->width()
                 << "\nframe1->min=" << widgetMinLength(dock1->dptr()->frame(), Qt::Horizontal);
        l->dumpLayout();
        QVERIFY(false);
    }

    QVERIFY(dock2->width() >= min2);

    // Dock on top of center widget:
    m = createMainWindow();

    dock1 = createDockWidget("one", new QTextEdit());
    m->addDockWidgetAsTab(dock1);
    auto dock3 = createDockWidget("three", new QTextEdit());
    m->addDockWidget(dock3, Location_OnTop);
    QVERIFY(m->dropArea()->checkSanity());

    min1 = widgetMinLength(dock1, Qt::Vertical);
    QVERIFY(dock1->height() >= min1);
}

void TestDocks::tst_constraintsPropagateUp()
{
    // Mostly for QtQuick, which doesn't have any layouts, so we need to make the propagation
    // Manually in DockWidgetQuick::minimumSize(), in FrameQuick, etc.

    EnsureTopLevelsDeleted e;
    const int minWidth = 500;
    const int minHeight = 400;
    const QSize minSz = { minWidth, minHeight };
    auto guestWidget = new MyWidget2(QSize(minWidth, minHeight));
    auto dock1 = createDockWidget("dock1", guestWidget);
    auto dock2= createDockWidget("dock2", new MyWidget2(QSize(minWidth, minHeight)));

    QCOMPARE(widgetMinLength(guestWidget, Qt::Vertical), minHeight);
    QCOMPARE(widgetMinLength(guestWidget, Qt::Horizontal), minWidth);
    QCOMPARE(dock1->minimumWidth(), minWidth);
    QCOMPARE(dock1->minimumHeight(), minHeight);
    QCOMPARE(dock1->minimumSize(), minSz);

    auto frame1 = dock1->dptr()->frame();

    QVERIFY(qAbs(widgetMinLength(frame1, Qt::Horizontal) - minWidth) < 10); //10px for styling differences
    QVERIFY(qAbs(widgetMinLength(frame1, Qt::Vertical) - (minHeight + frame1->nonContentsHeight())) < 10); //10px for styling differences

    // Add dock2 side-by side, so the Frame now has a title bar.
    auto oldFw2 = dock2->window();
    dock1->addDockWidgetToContainingWindow(dock2, Location_OnLeft);
    TitleBar *tb = dock1->titleBar();
    QVERIFY(tb->isVisible());
    QVERIFY(qAbs(widgetMinLength(frame1, Qt::Vertical) - (minHeight + frame1->nonContentsHeight())) < 10);
    delete dock1->window();
    delete oldFw2;
}

void TestDocks::tst_constraintsAfterPlaceholder()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_None);
    const int minHeight = 400;
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, minHeight)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(400, minHeight)));
    auto dock3 = createDockWidget("dock3", new MyWidget2(QSize(400, minHeight)));
    auto dropArea = m->dropArea();
    MultiSplitter *layout = dropArea;

    // Stack 3, 2, 1
    m->addDockWidget(dock1, Location_OnTop);
    m->addDockWidget(dock2, Location_OnTop);
    m->addDockWidget(dock3, Location_OnTop);

#ifdef KDDOCKWIDGETS_QTWIDGETS
    QVERIFY(Testing::waitForResize(m.get()));
#endif

    QVERIFY(widgetMinLength(m.get(), Qt::Vertical) > minHeight * 3); // > since some vertical space is occupied by the separators

    // Now close dock1 and check again
    dock1->close();
    Testing::waitForResize(dock2);

    Item *item2 = layout->itemForFrame(dock2->dptr()->frame());
    Item *item3 = layout->itemForFrame(dock3->dptr()->frame());

    QMargins margins = m->centerWidgetMargins();
    const int expectedMinHeight = item2->minLength(Qt::Vertical) +
                                  item3->minLength(Qt::Vertical) +
                                  1 * Item::separatorThickness
                                  + margins.top() + margins.bottom();

    QCOMPARE(m->minimumSizeHint().height(), expectedMinHeight);

    dock1->deleteLater();
    Testing::waitForDeleted(dock1);
}

void TestDocks::tst_dragBySingleTab()
{
    // Tests dragging via a tab when there's only 1 tab, and we're using Flag_AlwaysShowTabs
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AlwaysShowTabs);
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));
    dock1->show();

    auto frame1 = dock1->dptr()->frame();

    QPoint globalPressPos = dragPointForWidget(frame1, 0);
    TabBar *tabBar = frame1->tabWidget()->tabBar();
    QVERIFY(tabBar);
    SetExpectedWarning sew("No window being dragged for"); // because dragging by tab does nothing in this case
    drag(tabBar->asWidget(), globalPressPos, QPoint(0, 0));

    delete dock1;
    Testing::waitForDeleted(frame1);
}

void TestDocks::tst_dragByTabBar_data()
{
    QTest::addColumn<bool>("documentMode");
    QTest::addColumn<bool>("tabsAlwaysVisible");

    QTest::newRow("false-false") << false << false;
    QTest::newRow("true-false") << true << false;
    QTest::newRow("false-true") << false << true;
    QTest::newRow("true-true") << true << true;
}

void TestDocks::tst_dragByTabBar()
{
    QFETCH(bool, documentMode);
    QFETCH(bool, tabsAlwaysVisible);

    EnsureTopLevelsDeleted e;
    auto flags = KDDockWidgets::Config::self().flags() | KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible;
    if (tabsAlwaysVisible)
        flags |= KDDockWidgets::Config::Flag_AlwaysShowTabs;

    KDDockWidgets::Config::self().setFlags(flags);

    auto m = createMainWindow();

    auto dropArea = m->dropArea();
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));

    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(400, 400)));
    auto dock3 = createDockWidget("dock3", new MyWidget2(QSize(400, 400)));
    m->addDockWidgetAsTab(dock1);
    m->resize(osWindowMinWidth(), 200);

    dock2->addDockWidgetAsTab(dock3);
#if defined(KDDOCKWIDGETS_QTWIDGETS)
    if (documentMode)
        static_cast<QTabWidget *>(
            static_cast<FrameWidget *>(dock2->dptr()->frame())->tabWidget()->asWidget())
            ->setDocumentMode(true);
#else
    Q_UNUSED(documentMode);
#endif
    auto fw = dock2->floatingWindow();
    fw->move(m->pos() + QPoint(500, 500));
    QVERIFY(fw->isVisible());
    QVERIFY(!fw->titleBar()->isVisible());

    dragFloatingWindowTo(fw, dropArea, DropIndicatorOverlayInterface::DropLocation_Right);
}

void TestDocks::tst_dock2FloatingWidgetsTabbed()
{
    EnsureTopLevelsDeleted e;

    if (KDDockWidgets::usesNativeTitleBar())
        return; // Unit-tests can't drag via tab, yet

    auto dock1 = createDockWidget("doc1", Qt::green);
    auto fw1 = dock1->floatingWindow();
    fw1->setGeometry(QRect(500, 500, 400, 400));
    QVERIFY(dock1);
    QPointer<Frame> frame1 = dock1->dptr()->frame();

    auto titlebar1 = fw1->titleBar();
    auto dock2 = createDockWidget("doc2", Qt::red);

    QVERIFY(dock1->isFloating());
    QVERIFY(dock2->isFloating());

    QPoint finalPoint = dock2->window()->geometry().center() + QPoint(7, 7);
    drag(titlebar1, titlebar1->mapToGlobal(QPoint(5, 5)), finalPoint, ButtonAction_Press);

    // It morphed into a FloatingWindow
    QPointer<Frame> frame2 = dock2->dptr()->frame();
    if (!dock2->floatingWindow()) {
        qWarning() << "dock2->floatingWindow()=" << dock2->floatingWindow();
        QVERIFY(false);
    }
    QVERIFY(frame2);
    QCOMPARE(frame2->dockWidgetCount(), 1);

    releaseOn(finalPoint, titlebar1);
    QCOMPARE(frame2->dockWidgetCount(), 2); // 2.2 Frame has 2 widgets when one is dropped
    QVERIFY(Testing::waitForDeleted(frame1));

    // 2.3 Detach tab1 to empty space
    QPoint globalPressPos = dragPointForWidget(frame2.data(), 0);
    TabBar *tabBar = frame2->tabWidget()->tabBar();
    QVERIFY(tabBar);
    drag(tabBar->asWidget(), globalPressPos, frame2->window()->geometry().bottomRight() + QPoint(10, 10));

    QVERIFY(frame2->dockWidgetCount() == 1);
    QVERIFY(dock1->floatingWindow());

    // 2.4 Drag the first dock over the second
    frame1 = dock1->dptr()->frame();
    frame2 = dock2->dptr()->frame();
    fw1 = dock1->floatingWindow();
    globalPressPos = fw1->titleBar()->mapToGlobal(QPoint(100,5));
    finalPoint = dock2->window()->geometry().center() + QPoint(7, 7);
    drag(fw1->titleBar(), globalPressPos, finalPoint);

    QCOMPARE(frame2->dockWidgetCount(), 2);

    // 2.5 Detach and drop to the same place, should tab again
    globalPressPos = dragPointForWidget(frame2.data(), 0);
    tabBar = frame2->tabWidget()->tabBar();

    finalPoint = dock2->window()->geometry().center() + QPoint(7, 7);
    drag(tabBar->asWidget(), globalPressPos, finalPoint);
    QCOMPARE(frame2->dockWidgetCount(), 2);

    // 2.6 Drag the tabbed group over a 3rd floating window
    auto dock3 = createDockWidget("doc3", Qt::black);
    QTest::qWait(1000); // Test is flaky otherwise

    auto fw2 = dock2->floatingWindow();
    finalPoint = dock3->window()->geometry().center() + QPoint(7, 7);
    drag(fw2->titleBar(), frame2->mapToGlobal(QPoint(10, 10)), finalPoint);

    QVERIFY(Testing::waitForDeleted(frame1));
    QVERIFY(Testing::waitForDeleted(frame2));
    QVERIFY(dock3->dptr()->frame());
    QCOMPARE(dock3->dptr()->frame()->dockWidgetCount(), 3);

    auto fw3 = dock3->floatingWindow();
    QVERIFY(fw3);
    QVERIFY(fw3->dropArea()->checkSanity());

    // 2.7 Drop the window into a MainWindow
    {
        auto m = createMainWindow();
        m->show();
        m->setGeometry(QRect(500, 300, 300, 300));
        QVERIFY(!dock3->isFloating());
        auto fw3 = dock3->floatingWindow();
        drag(fw3->titleBar(), dock3->window()->mapToGlobal(QPoint(10, 10)), m->geometry().center());
        QVERIFY(!dock3->isFloating());
        QVERIFY(dock3->window() == m.get());
        QCOMPARE(dock3->dptr()->frame()->dockWidgetCount(), 3);
        QVERIFY(m->dropArea()->checkSanity());

        delete dock1;
        delete dock2;
        delete dock3;
        QVERIFY(Testing::waitForDeleted(frame2));
        QVERIFY(Testing::waitForDeleted(fw3));
    }
}

void TestDocks::tst_deleteOnClose()
{
    {
        EnsureTopLevelsDeleted e;
        // Tests that DockWidget::close() deletes itself if Option_DeleteOnClose is set
        QPointer<DockWidgetBase> dock1 = createDockWidget("1", new MyWidget2(QSize(400, 400)), DockWidgetBase::Option_DeleteOnClose);
        dock1->show();
        dock1->close();

        QVERIFY(Testing::waitForDeleted(dock1));
    }

    {
        // Tests that if it's closed via LayoutSaver it's also destroyed when having Option_DeleteOnClose
        EnsureTopLevelsDeleted e;

        QPointer<DockWidgetBase> dock1 = createDockWidget("1", new MyWidget2(QSize(400, 400)), DockWidgetBase::Option_DeleteOnClose, {}, /*show=*/ false);
        QPointer<DockWidgetBase> dock2 = createDockWidget("2", new MyWidget2(QSize(400, 400)), {}, {}, /*show=*/ false);
        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();
        dock1->show();
        dock2->show();
        QVERIFY(dock1->isVisible());
        QVERIFY(dock2->isVisible());

        QVERIFY(saver.restoreLayout(saved));
        QVERIFY(!dock1->isVisible());
        QVERIFY(!dock2->isVisible());

        QVERIFY(Testing::waitForDeleted(dock1));
        QVERIFY(dock2.data());
        delete dock2;
    }
#ifdef KDDOCKWIDGETS_QTWIDGETS // QtQuick doesn't support autohide/pin/sidebar yet
    {
        // Tests that restoring the side-bar-overlay will call the users dock widget factory in case
        // the dock widget was deleted already
        EnsureTopLevelsDeleted e;
        KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AutoHideSupport);
        KDDockWidgets::Config::self().setDockWidgetFactoryFunc([](const QString &name) {
            return createDockWidget(name, new MyWidget2(QSize(400, 400)), DockWidgetBase::Option_DeleteOnClose);
        });

        auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
        QPointer<DockWidgetBase> dock1 = createDockWidget("1", new MyWidget2(QSize(400, 400)), DockWidgetBase::Option_DeleteOnClose);
        m->addDockWidget(dock1, Location_OnLeft);
        m->moveToSideBar(dock1);
        m->overlayOnSideBar(dock1);
        LayoutSaver saver;
        const QByteArray saved = saver.serializeLayout();

        QVERIFY(dock1->isVisible());
        dock1->close();
        QVERIFY(Testing::waitForDeleted(dock1));
        QVERIFY(!dock1.data());

        saver.restoreLayout(saved);
    }
#endif
}

void TestDocks::tst_toggleAction()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));
    auto dock2 = createDockWidget("dock2", new MyWidget2(QSize(400, 400)));
    auto dock3 = createDockWidget("dock3", new MyWidget2(QSize(400, 400)));

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnRight);
    m->addDockWidget(dock3, Location_OnRight);

    auto root = m->multiSplitter()->rootItem();
    QCOMPARE(root->visibleCount_recursive(), 3);
    QVERIFY(dock2->toggleAction()->isChecked());
    QPointer<Frame> frame2 = dock2->dptr()->frame();
    dock2->toggleAction()->toggle();
    QVERIFY(!dock2->toggleAction()->isChecked());

    QVERIFY(!dock2->isVisible());
    QVERIFY(!dock2->isOpen());
    QVERIFY(Testing::waitForDeleted(frame2));

    QCOMPARE(root->visibleCount_recursive(), 2);
}

void TestDocks::tst_redocksToPreviousTabIndex()
{
    // Checks that when reordering tabs with mouse, floating and redocking, they go back to their previous index

    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_AllowReorderTabs);

    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));
    m->addDockWidget(dock0, Location_OnLeft);
    dock0->addDockWidgetAsTab(dock1);

    QCOMPARE(dock0->tabIndex(), 0);
    QCOMPARE(dock1->tabIndex(), 1);

    dock0->setFloating(true);
    QCOMPARE(dock1->tabIndex(), 0);

    dock0->setFloating(false);
    QCOMPARE(dock0->tabIndex(), 0);
    QCOMPARE(dock1->tabIndex(), 1);

    Frame *frame = dock0->dptr()->frame();
    auto tb = dock0->dptr()->frame()->tabWidget()->tabBar();
    tb->moveTabTo(0, 1);

#ifdef KDDOCKWIDGETS_QTWIDGETS
    QCOMPARE(dock0->tabIndex(), 1);
    QCOMPARE(dock1->tabIndex(), 0);

    // Also detach via detachTab(), which is what is called when the user detaches with mouse
    frame->detachTab(dock0);
    dock0->setFloating(false);

    QCOMPARE(dock0->tabIndex(), 1);
    QCOMPARE(dock1->tabIndex(), 0);
#else
    // An XFAIL so we remember to implement this
    QEXPECT_FAIL("", "TabBar::moveTabTo not implemented for QtQuick yet", Continue);
    QVERIFY(false);
    Q_UNUSED(frame);
#endif
}

void TestDocks::tst_toggleTabbed()
{
    // Testing the weird bugs reported in #211

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_None);
    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));

    m->addDockWidget(dock0, Location_OnBottom);
    dock0->addDockWidgetAsTab(dock1);

    QVERIFY(dock1->isCurrentTab());
    QVERIFY(dock0->toggleAction()->isChecked());
    QVERIFY(dock1->toggleAction()->isChecked());
    QVERIFY(dock0->isOpen());
    QVERIFY(dock1->isOpen());

    dock0->close();
    QVERIFY(!dock0->isOpen());
    QVERIFY(dock1->isOpen());
    QVERIFY(dock1->toggleAction()->isChecked());
    QVERIFY(dock1->isCurrentTab());
    Frame *frame = dock1->dptr()->frame();
    TabWidget *tw = frame->tabWidget();
    QCOMPARE(tw->currentIndex(), 0);
    QCOMPARE(tw->numDockWidgets(), 1);
    QCOMPARE(tw->currentDockWidget(), dock1);
    QVERIFY(!dock0->isVisible());
    QVERIFY(frame->QWidgetAdapter::isVisible());

    QVERIFY(dock1->isVisible());
}

void TestDocks::tst_toggleTabbed2()
{
    // Testing the weird bugs reported in #215
    EnsureTopLevelsDeleted e;
    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));
    auto dock1 = createDockWidget("dock1", new MyWidget2(QSize(400, 400)));
    dock0->addDockWidgetAsTab(dock1);

    dock0->setAsCurrentTab();

    Frame *frame1 = dock1->dptr()->frame();
    QCOMPARE(frame1->currentDockWidget(), dock0);
    QCOMPARE(frame1->currentIndex(), 0);

    dock0->setFloating(true);
    Frame *frame0 = dock0->dptr()->frame();

    QCOMPARE(frame0->currentIndex(), 0);
    QCOMPARE(frame1->currentIndex(), 0);

    QCOMPARE(frame0->title(), "dock0");
    QCOMPARE(frame1->title(), "dock1");
}

void TestDocks::tst_resizePropagatesEvenly()
{
    // For github issue #186
    // Usually resizing main window will resize dock widgets evenly, but if you resize multiple
    // times then one dock widget is getting too small. Not repro with all layouts, but the following
    // one reproduced it:

    auto m = createMainWindow(QSize(1000, 1000), MainWindowOption_None);
    auto dock0 = createDockWidget("dock0", new MyWidget2());
    auto dock1 = createDockWidget("dock1", new MyWidget2());
    auto dock2 = createDockWidget("dock2", new MyWidget2());

    m->addDockWidget(dock1, Location_OnLeft);
    m->addDockWidget(dock2, Location_OnTop, dock1);
    m->addDockWidget(dock0, Location_OnRight);

    QVERIFY(qAbs(dock2->height() - dock1->height()) < 2);

    m->resize(m->size() + QSize(0, 500));
    for (int i = 1; i < 10; ++i)
        m->resize(m->size() - QSize(0, i));

    QVERIFY(qAbs(dock2->height() - dock1->height()) < 3);
}

void TestDocks::tst_addMDIDockWidget()
{
    EnsureTopLevelsDeleted e;

    // Test that adding a MDI dock widget doesn't produce any warning
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_MDI);
    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));
    qobject_cast<MDILayoutWidget *>(m->layoutWidget())->addDockWidget(dock0, QPoint(0, 0), {});
}

void TestDocks::tst_redockToMDIRestoresPosition()
{
    // Tests that setFloating(false) puts the dock widget where it was before floating

    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(800, 500), MainWindowOption_MDI);
    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));

    auto layoutWidget = qobject_cast<MDILayoutWidget *>(m->layoutWidget());
    const QPoint initialPoint = QPoint(500, 500);
    layoutWidget->addDockWidget(dock0, initialPoint, {});

    Frame *frame = dock0->DockWidgetBase::d->frame();
    QCOMPARE(frame->QWidgetAdapter::pos(), initialPoint);

    const QSize initialSize = frame->QWidgetAdapter::size();

    dock0->setFloating(true);
    dock0->setFloating(false);
    frame = dock0->DockWidgetBase::d->frame();
    QCOMPARE(frame->QWidgetAdapter::pos(), initialPoint);

    const QPoint anotherPos = QPoint(250, 250);
    dock0->setMDIPosition(anotherPos);

    dock0->setFloating(true);
    dock0->setFloating(false);
    frame = dock0->DockWidgetBase::d->frame();

    Item *item = layoutWidget->itemForFrame(frame);
    QCOMPARE(item->pos(), anotherPos);
    QCOMPARE(item->geometry(), frame->QWidgetAdapter::geometry());
    QCOMPARE(frame->QWidgetAdapter::pos(), anotherPos);
    QCOMPARE(frame->QWidgetAdapter::size(), initialSize);

    const QSize anotherSize = QSize(500, 500);
    dock0->setMDISize(anotherSize);
    QCOMPARE(frame->QWidgetAdapter::size(), anotherSize);
    item = layoutWidget->itemForFrame(frame);
    QCOMPARE(item->geometry(), frame->QWidgetAdapter::geometry());
}

void TestDocks::tst_restoreWithNativeTitleBar()
{
#ifdef Q_OS_WIN // Other OS don't support this
    EnsureTopLevelsDeleted e;
    KDDockWidgets::Config::self().setFlags(KDDockWidgets::Config::Flag_NativeTitleBar);

    auto dock0 = createDockWidget("dock0", new MyWidget2(QSize(400, 400)));
    dock0->window()->move(100, 100);

    QVERIFY(!dock0->titleBar()->isVisible());
    QVERIFY(!dock0->floatingWindow()->titleBar()->isVisible());
    QVERIFY(!dock0->d->frame()->titleBar()->isVisible());

    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    saver.restoreLayout(saved);
    QVERIFY(!dock0->titleBar()->isVisible());
    QVERIFY(!dock0->floatingWindow()->titleBar()->isVisible());
    QVERIFY(!dock0->d->frame()->titleBar()->isVisible());
#endif
}

void TestDocks::tst_closeTabOfCentralFrame()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_closeTabOfCentralFrame");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    m->addDockWidgetAsTab(dock1);
    Frame *frame = dock1->dptr()->frame();
    QVERIFY(frame->options() & FrameOption_IsCentralFrame);
    QVERIFY(frame->QWidgetAdapter::isVisible());
    dock1->close();
    QVERIFY(frame->QWidgetAdapter::isVisible());
}

void TestDocks::tst_centralFrame245()
{
    /*

    Build: -DKDDockWidgets_DEVELOPER_MODE=ON
    Run: ./bin/tst_docks tst_centralFrame245 -platform xcb

    auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralFrame, "tst_centralFrame245");
    auto dock1 = createDockWidget("1", new QPushButton("1"));
    auto dock2 = createDockWidget("2", new QPushButton("2"));

    m->addDockWidgetAsTab(dock1);
    m->addDockWidgetAsTab(dock2);

    QTest::qWait(100000);

*/
}

void TestDocks::tst_persistentCentralWidget()
{
    EnsureTopLevelsDeleted e;
    auto m = createMainWindow(QSize(500, 500), MainWindowOption_HasCentralWidget);
    auto dockwidgets = m->dropArea()->dockWidgets();
    QCOMPARE(dockwidgets.size(), 1);

    auto dw = dockwidgets.constFirst();
    dw->close();
    QVERIFY(dw->isOpen());
    QVERIFY(dw->isPersistentCentralDockWidget());
    dw->setFloating(true);
    QVERIFY(!dw->isFloating());


    LayoutSaver saver;
    const QByteArray saved = saver.serializeLayout();
    QVERIFY(!saved.isEmpty());

    QVERIFY(saver.restoreLayout(saved));
}
