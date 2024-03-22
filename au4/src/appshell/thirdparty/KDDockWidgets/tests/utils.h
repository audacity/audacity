/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KDDOCKWIDGETS_TESTS_UTILS_H
#define KDDOCKWIDGETS_TESTS_UTILS_H

// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#include "Config.h"
#include "DockRegistry_p.h"
#include "DockWidgetBase.h"
#include "DockWidgetBase_p.h"
#include "DropArea_p.h"
#include "DropIndicatorOverlayInterface_p.h"
#include "FloatingWindow_p.h"
#include "KDDockWidgets.h"
#include "TitleBar_p.h"
#include "Utils_p.h"

#ifdef KDDOCKWIDGETS_QTWIDGETS
# include "widgets/TabWidgetWidget_p.h"
# include "widgets/FrameWidget_p.h"
# include "MainWindow.h"

# include <QVBoxLayout>
# include <QWidget>
# include <QToolButton>
# include <QLineEdit>
using FocusableWidget = QLineEdit;
#else
# include "quick/MainWindowQuick_p.h"
# include "quick/TabWidgetQuick_p.h"

# include <QQuickView>
#endif

#include <QPointer>
#include <QVector>
#include <QtTest/QtTest>

#include <memory>

static bool s_pauseBeforePress = false; // for debugging
static bool s_pauseBeforeMove = false; // for debugging
#define DEBUGGING_PAUSE_DURATION 5000 // 5 seconds

// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates

namespace KDDockWidgets {

class FrameWidget;

namespace Tests {

template <typename T>
inline QPointer<T> make_qpointer(T *t)
{
    // To support both QWidget and QtQuick we need QPointer<auto>, so use a function instead.
    return QPointer<T>(t);
}

enum ButtonAction {
    ButtonAction_None,
    ButtonAction_Press = 1,
    ButtonAction_Release = 2
};
Q_DECLARE_FLAGS(ButtonActions, ButtonAction)

struct DockDescriptor {
    Location loc;
    int relativeToIndex;
    QPointer<DockWidgetBase> createdDock;
    KDDockWidgets::InitialVisibilityOption option;
};

inline bool shouldSkipTests()
{
    // Skip mac+offscreen on Qt <= 5.15.0 due to a QPA crash, fixed in 5.15.1
#if defined(Q_OS_MACOS) && QT_VERSION <= QT_VERSION_CHECK(5, 15, 0)
    if (qApp->platformName() == QLatin1String("offscreen"))
        return true;
#endif

    return false;
}

struct EnsureTopLevelsDeleted
{
    EnsureTopLevelsDeleted()
        : m_originalFlags(Config::self().flags())
        , m_originalInternalFlags(Config::self().internalFlags())
        , m_originalSeparatorThickness(Config::self().separatorThickness())
    {
    }

    ~EnsureTopLevelsDeleted()
    {
        qDeleteAll(DockRegistry::self()->floatingWindows(/*includeBeingDeleted=*/ true));
        qDeleteAll(DockRegistry::self()->dockwidgets());

        if (!DockRegistry::self()->isEmpty()) {
            auto dr = DockRegistry::self();
            qWarning() << "There's still top-level widgets present!"
                       << "\nfloatings:" << dr->floatingWindows(/*includeBeingDeleted=*/ true)
                       << "\nmainwindows:" << dr->mainWindowsNames()
                       << "\ndocks:" << dr->dockWidgetNames();
        }

        // Other cleanup, since we use this class everywhere
        Config::self().setDockWidgetFactoryFunc(nullptr);
        Config::self().setInternalFlags(m_originalInternalFlags);
        Config::self().setFlags(m_originalFlags);
        Config::self().setSeparatorThickness(m_originalSeparatorThickness);
    }

    const Config::Flags m_originalFlags;
    const Config::InternalFlags m_originalInternalFlags;
    const int m_originalSeparatorThickness;
};

bool shouldBlacklistWarning(const QString &msg, const QString &category = {});

std::unique_ptr<MainWindowBase> createMainWindow(QSize sz = {1000, 1000},
                                                 KDDockWidgets::MainWindowOptions options = MainWindowOption_HasCentralFrame,
                                                 const QString &name = {}, bool show = true);



std::unique_ptr<KDDockWidgets::MainWindowBase> createMainWindow(QVector<DockDescriptor> &docks);

KDDockWidgets::DockWidgetBase *createDockWidget(const QString &name, QWidgetOrQuick *w,
                                                DockWidgetBase::Options options = {}, DockWidgetBase::LayoutSaverOptions layoutSaverOptions = {},
                                                bool show = true, const QString &affinityName = {});
KDDockWidgets::DockWidgetBase *createDockWidget(const QString &name, QColor color = Qt::black);

void nestDockWidget(DockWidgetBase *dock, DropArea *dropArea, Frame *relativeTo,
                    KDDockWidgets::Location location);

class MyWidget : public QWidgetOrQuick
{
public:
    explicit MyWidget(const QString &, QColor c = Qt::black);
    ~MyWidget() override;


    QSize minimumSizeHint() const override
    {
        return {100, 100};
    }

protected:
#ifdef KDDOCKWIDGETS_QTWIDGETS
    void paintEvent(QPaintEvent *) override;
#endif
private:
    QColor c;
};


#ifdef KDDOCKWIDGETS_QTQUICK
// Don't want to adapt dozens of locations so it compiles for QtQuick, so just typedef.
// the fact it's a button isn't important for the tests anyway

class QPushButton : public MyWidget
{
public:
    // use const char* to silence QtCreator static analyzer warnings when using const char * in tst_docks.cpp
    // We don't have QT_NO_CAST_FROM_ASCII and still it complains, so use an indirection so I can read tst_docks while
    // porting to QtQuick without noise. Once the port is done feel free to change to QString.
    explicit QPushButton(const char *name)
        : MyWidget(QString::fromLatin1(name))
    {
    }

    ~QPushButton();
};

class FocusableWidget : public QWidgetAdapter
{
public:
    explicit FocusableWidget()
        : QWidgetAdapter()
    {
        setFocusPolicy(Qt::StrongFocus);
    }

    ~FocusableWidget();
};

class QTextEdit : public QWidgetAdapter
{
public:
    explicit QTextEdit()
        : QWidgetAdapter()
    {
        setFocusPolicy(Qt::StrongFocus);
    }

    ~QTextEdit();
};

class NonClosableWidget : public QWidgetAdapter
{
public:
    Q_OBJECT
public:
    explicit NonClosableWidget()
        : QWidgetAdapter()
    {
    }

    ~NonClosableWidget();

protected:
    void onCloseEvent(QCloseEvent *ev) override
    {
        ev->ignore(); // don't allow to close
    }
};

class MyWidget2 : public QWidgetAdapter
{
public:

    explicit MyWidget2(QSize minSz = QSize(1, 1))
    {
        setMinimumSize(minSz);
        setSizeHint(minSz);
    }

    ~MyWidget2();

    QSize sizeHint() const
    {
        return m_sizeHint;
    }

    void setSizeHint(QSize s)
    {
        m_sizeHint = s;
    }

    QSize m_sizeHint;
};

class EmbeddedWindow : public QWidgetAdapter
{
public:
    explicit EmbeddedWindow(MainWindowBase *m)
        : mainWindow(m)
    {
    }

    ~EmbeddedWindow() override;

    MainWindowBase *const mainWindow;
};

#else

class EmbeddedWindow : public QWidget
{
public:
    explicit EmbeddedWindow(MainWindowBase *m)
        : mainWindow(m)
    {
    }

    ~EmbeddedWindow() override;

    MainWindowBase *const mainWindow;
};

class NonClosableWidget : public QWidget
{
public:
    Q_OBJECT
public:
    explicit NonClosableWidget(QWidget *parent = nullptr);
    ~NonClosableWidget() override;

protected:
    void closeEvent(QCloseEvent *event) override;
};

namespace {

class MyWidget2 : public QWidget
{
public:

    explicit MyWidget2(QSize minSz = QSize(1,1))
        : m_minSz(minSz)
        , m_sizeHint(minSz)
    {

    }

    QSize sizeHint() const override
    {
        return m_sizeHint;
    }

    QSize minimumSizeHint() const override
    {
        return m_minSz;
    }

    void setMinSize(QSize s)
    {
        m_minSz = s;
        updateGeometry();
    }

    void setSizeHint(QSize s)
    {
        m_sizeHint = s;
    }

    QSize m_minSz;
    QSize m_sizeHint;
};
}

#endif

void doubleClickOn(QPoint globalPos, WidgetType *receiver);
void doubleClickOn(QPoint globalPos, QWindow *receiver);
void pressOn(QPoint globalPos, WidgetType *receiver);
void pressOn(QPoint globalPos, QWindow *receiver);
void releaseOn(QPoint globalPos, WidgetType *receiver);
void clickOn(QPoint globalPos, WidgetType *receiver);
void moveMouseTo(QPoint globalDest, WidgetType *receiver);

inline FloatingWindow *createFloatingWindow()
{
    static int count = 0;
    count++;
    auto dock = createDockWidget(QString("dock %1").arg(count), Qt::green);
    return dock->d->morphIntoFloatingWindow();
}

inline WidgetType *draggableFor(WidgetType *w)
{
    WidgetType *draggable = nullptr;
    if (auto dock = qobject_cast<DockWidgetBase *>(w)) {
        if (auto frame = dock->d->frame())
            draggable = frame->titleBar();
    } else if (auto fw = qobject_cast<FloatingWindow *>(w)) {
        Frame *frame = fw->hasSingleFrame() ? static_cast<Frame*>(fw->frames().first())
                                            : nullptr;

        if ((KDDockWidgets::Config::self().flags() & KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible) && frame && frame->hasTabsVisible()) {
            draggable = frame->tabWidget()->asWidget();
        } else {
            draggable = fw->titleBar();
        }
#ifdef KDDOCKWIDGETS_QTWIDGETS
    } else if (qobject_cast<TabWidgetWidget *>(w)) {
        draggable = w;
#else
    } else if (qobject_cast<TabWidgetQuick *>(w)) {
        draggable = w;
#endif
    } else if (qobject_cast<TitleBar *>(w)) {
        draggable = w;
    }

    qDebug() << "Draggable is" << draggable << "for" << w;
    return draggable;
}

inline void drag(WidgetType *sourceWidget, QPoint pressGlobalPos, QPoint globalDest,
                 ButtonActions buttonActions = ButtonActions(ButtonAction_Press) | ButtonAction_Release)
{
    if (buttonActions & ButtonAction_Press) {
        if (s_pauseBeforePress)
            QTest::qWait(DEBUGGING_PAUSE_DURATION);

        pressOn(pressGlobalPos, sourceWidget);
    }

    activateWindow(sourceWidget->window());

    if (s_pauseBeforeMove)
        QTest::qWait(DEBUGGING_PAUSE_DURATION);

    qDebug() << "Moving sourceWidget to" << globalDest
             << "; sourceWidget->size=" << sourceWidget->size()
             << "; from=" << QCursor::pos();
    moveMouseTo(globalDest, sourceWidget);
    qDebug() << "Arrived at" << QCursor::pos();
    pressGlobalPos = KDDockWidgets::mapToGlobal(sourceWidget, QPoint(10, 10));
    if (buttonActions & ButtonAction_Release)
        releaseOn(globalDest, sourceWidget);
}

inline void drag(WidgetType *sourceWidget, QPoint globalDest,
                 ButtonActions buttonActions = ButtonActions(ButtonAction_Press) | ButtonAction_Release)
{
    Q_ASSERT(sourceWidget && sourceWidget->isVisible());

    WidgetType *draggable = draggableFor(sourceWidget);

    Q_ASSERT(draggable && draggable->isVisible());
    const QPoint pressGlobalPos = KDDockWidgets::mapToGlobal(draggable, QPoint(6, 6));

    drag(draggable, pressGlobalPos, globalDest, buttonActions);
}

inline void dragFloatingWindowTo(FloatingWindow *fw, QPoint globalDest,
                                 ButtonActions buttonActions = ButtonActions(ButtonAction_Press) | ButtonAction_Release)
{
    auto draggable = draggableFor(fw);
    Q_ASSERT(draggable);
    Q_ASSERT(draggable->isVisible());
    drag(draggable, KDDockWidgets::mapToGlobal(draggable, QPoint(10, 10)), globalDest, buttonActions);
}

inline void dragFloatingWindowTo(FloatingWindow *fw, DropArea *target, DropIndicatorOverlayInterface::DropLocation dropLocation)
{
    auto draggable = draggableFor(fw);
    Q_ASSERT(draggable);

    // First we drag over it, so the drop indicators appear:
    drag(draggable, KDDockWidgets::mapToGlobal(draggable, QPoint(10, 10)), target->window()->mapToGlobal(target->window()->rect().center()), ButtonAction_Press);

    // Now we drag over the drop indicator and only then release mouse:
    DropIndicatorOverlayInterface *dropIndicatorOverlay = target->dropIndicatorOverlay();
    const QPoint dropPoint = dropIndicatorOverlay->posForIndicator(dropLocation);

    drag(draggable, QPoint(), dropPoint, ButtonAction_Release);
}

inline EmbeddedWindow *createEmbeddedMainWindow(QSize sz)
{
    static int count = 0;
    count++;
    // Tests a MainWindow which isn't a top-level window, but is embedded in another window
    auto mainwindow = createMainWindow(QSize(600, 600), MainWindowOption_HasCentralFrame).release();

    auto window = new EmbeddedWindow(mainwindow);
#ifdef KDDOCKWIDGETS_QTWIDGETS
    auto lay = new QVBoxLayout(window);
    lay->setContentsMargins(100, 100, 100, 100);
    lay->addWidget(mainwindow);
#else
    // TODO: For QtQuick we need some QML
    qWarning() << "Parent me!";
#endif
    window->show();
    window->resize(sz);
    return window;
}

}

inline bool qpaPassedAsArgument(int argc, char *argv[])
{
    for (int i = 1; i < argc; ++i) {
        if (qstrcmp(argv[i], "-platform") == 0) {
            return true;
        }
    }

    return false;
}

}

Q_DECLARE_METATYPE(KDDockWidgets::Tests::DockDescriptor)


#endif

