/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Window to show debug information. Used for debugging only, for apps that don't support GammaRay.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "DebugWindow_p.h"
#include "DockRegistry_p.h"
#include "FloatingWindow_p.h"
#include "LayoutSaver.h"
#include "LayoutWidget_p.h"
#include "MainWindow.h"
#include "ObjectViewer_p.h"
#include "Qt5Qt6Compat_p.h"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLineEdit>
#include <QSpinBox>
#include <QMessageBox>
#include <QApplication>
#include <QMouseEvent>
#include <QWindow>
#include <QFileDialog>
#include <QAbstractNativeEventFilter>
#include <QTimer>

#ifdef Q_OS_WIN
#include <windows.h>
#include <winuser.h>
#endif

// clazy:excludeall=range-loop

using namespace KDDockWidgets;
using namespace KDDockWidgets::Debug;

class DebugAppEventFilter : public QAbstractNativeEventFilter
{
public:
    DebugAppEventFilter()
    {
    }
    ~DebugAppEventFilter();
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    bool nativeEventFilter(const QByteArray &eventType, void *message, qintptr *) override
#else
    bool nativeEventFilter(const QByteArray &eventType, void *message, long *) override
#endif
    {
#ifdef Q_OS_WIN
        if (eventType != "windows_generic_MSG")
            return false;
        auto msg = static_cast<MSG *>(message);

        if (msg->message == WM_NCCALCSIZE)
            qDebug() << "Got WM_NCCALCSIZE!" << message;
#else
        Q_UNUSED(eventType);
        Q_UNUSED(message);
#endif

        return false; // don't accept anything
    }
};

DebugAppEventFilter::~DebugAppEventFilter()
{
}

DebugWindow::DebugWindow(QWidget *parent)
    : QWidget(parent)
    , m_objectViewer(this)
{
    // qApp->installNativeEventFilter(new DebugAppEventFilter());
    auto layout = new QVBoxLayout(this);
    layout->addWidget(&m_objectViewer);

    auto button = new QPushButton(this);
    button->setText(QStringLiteral("Dump Debug"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, &DebugWindow::dumpDockWidgetInfo);

    auto hlay = new QHBoxLayout();
    layout->addLayout(hlay);

    button = new QPushButton(this);
    auto spin = new QSpinBox(this);
    spin->setMinimum(0);
    button->setText(QStringLiteral("Toggle float"));
    hlay->addWidget(button);
    hlay->addWidget(spin);

    connect(button, &QPushButton::clicked, this, [spin] {
        auto docks = DockRegistry::self()->dockwidgets();
        const int index = spin->value();
        if (index >= docks.size()) {
            QMessageBox::warning(nullptr, QStringLiteral("Invalid index"),
                                 QStringLiteral("Max index is %1").arg(docks.size() - 1));
        } else {
            auto dw = docks.at(index);
            dw->setFloating(!dw->isFloating());
        }
    });

    hlay = new QHBoxLayout();
    layout->addLayout(hlay);
    button = new QPushButton(this);
    auto lineedit = new QLineEdit(this);
    lineedit->setPlaceholderText(tr("DockWidget unique name"));
    button->setText(QStringLiteral("Show"));
    hlay->addWidget(button);
    hlay->addWidget(lineedit);

    connect(button, &QPushButton::clicked, this, [lineedit] {
        auto dw = DockRegistry::self()->dockByName(lineedit->text());
        if (dw) {
            dw->show();
        } else {
            QMessageBox::warning(nullptr, QStringLiteral("Could not find"),
                                 QStringLiteral("Could not find DockWidget with name %1").arg(lineedit->text()));
        }
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Float all visible docks"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        for (auto dw : DockRegistry::self()->dockwidgets()) {
            if (dw->isVisible() && !dw->isFloating()) {
                dw->setFloating(true);
            }
        }
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Show All DockWidgets"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [this] {
        QTimer::singleShot(3000, this, [] {
            const auto docks = DockRegistry::self()->dockwidgets();
            for (auto dw : docks) {
                dw->show();
            }
        });
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Save layout"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        LayoutSaver saver;
        QString message = saver.saveToFile(QStringLiteral("layout.json")) ? QStringLiteral("Saved!")
                                                                          : QStringLiteral("Error!");
        qDebug() << message;
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Restore layout"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        LayoutSaver saver;
        QString message = saver.restoreFromFile(QStringLiteral("layout.json")) ? QStringLiteral("Restored!")
                                                                               : QStringLiteral("Error!");
        qDebug() << message;
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Pick Widget"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [this] {
        qApp->setOverrideCursor(Qt::CrossCursor);
        grabMouse();

        QEventLoop loop;
        m_isPickingWidget = &loop;
        loop.exec();

        releaseMouse();
        m_isPickingWidget = nullptr;
        qApp->restoreOverrideCursor();
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("check sanity"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        const auto mainWindows = DockRegistry::self()->mainwindows();
        for (MainWindowBase *mainWindow : mainWindows) {
            mainWindow->layoutWidget()->checkSanity();
        }

        const auto floatingWindows = DockRegistry::self()->floatingWindows();
        for (FloatingWindow *floatingWindow : floatingWindows) {
            floatingWindow->layoutWidget()->checkSanity();
        }
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Detach central widget"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        const auto mainWindows = DockRegistry::self()->mainwindows();
        if (mainWindows.isEmpty())
            return;
        auto mainwindow = mainWindows.at(0);
        auto centralWidget = mainwindow->centralWidget();
        centralWidget->setParent(nullptr, Qt::Window);
        if (!centralWidget->isVisible()) {
            centralWidget->show();
        }
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Repaint all widgets"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [this] {
        const auto topLevelWidgets = qApp->topLevelWidgets();
        for (auto w : topLevelWidgets)
            repaintWidgetRecursive(w);
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("resize by 1x1"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [] {
        const auto layouts = DockRegistry::self()->layouts();
        for (auto l : layouts) {
            QWidget *tlw = l->window();
            tlw->resize(tlw->size() + QSize(1, 1));
        }
    });

    button = new QPushButton(this);
    button->setText(QStringLiteral("Raise #0 (after 3s timeout)"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, [this] {
        QTimer::singleShot(3000, this, [] {
            const auto docks = DockRegistry::self()->dockwidgets();
            if (!docks.isEmpty())
                docks.constFirst()->raise();
        });
    });

#ifdef Q_OS_WIN
    button = new QPushButton(this);
    button->setText(QStringLiteral("Dump native windows"));
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, &DebugWindow::dumpWindows);
#endif

    resize(800, 800);
}

#ifdef Q_OS_WIN
void DebugWindow::dumpWindow(QWidget *w)
{
    if (w->windowHandle()) {
        HWND hwnd = HWND(w->winId());

        RECT clientRect;
        RECT rect;
        GetWindowRect(hwnd, &rect);
        GetClientRect(hwnd, &clientRect);

        qDebug() << w
                 << QStringLiteral(" ClientRect=%1,%2 %3x%4").arg(clientRect.left).arg(clientRect.top).arg(clientRect.right - clientRect.left + 1).arg(clientRect.bottom - clientRect.top + 1)
                 << QStringLiteral(" WindowRect=%1,%2 %3x%4").arg(rect.left).arg(rect.top).arg(rect.right - rect.left + 1).arg(rect.bottom - rect.top + 1)
                 << "; geo=" << w->geometry()
                 << "; frameGeo=" << w->frameGeometry();
    }

    for (QObject *child : w->children()) {
        if (auto childW = qobject_cast<QWidget *>(child)) {
            dumpWindow(childW);
        }
    }
}


void DebugWindow::dumpWindows()
{
    for (QWidget *w : qApp->topLevelWidgets()) {
        if (w != window())
            dumpWindow(w);
    }
}

#endif

void DebugWindow::repaintWidgetRecursive(QWidget *w)
{
    w->repaint();
    for (QObject *child : w->children()) {
        if (auto childW = qobject_cast<QWidget *>(child)) {
            repaintWidgetRecursive(childW);
        }
    }
}

void DebugWindow::dumpDockWidgetInfo()
{
    const QVector<FloatingWindow *> floatingWindows = DockRegistry::self()->floatingWindows();
    const MainWindowBase::List mainWindows = DockRegistry::self()->mainwindows();
    const DockWidgetBase::List dockWidgets = DockRegistry::self()->dockwidgets();

    for (FloatingWindow *fw : floatingWindows) {
        qDebug() << fw << "; affinities=" << fw->affinities();
        fw->layoutWidget()->dumpLayout();
    }

    for (MainWindowBase *mw : mainWindows) {
        qDebug() << mw << "; affinities=" << mw->affinities();
        mw->layoutWidget()->dumpLayout();
    }

    for (DockWidgetBase *dw : dockWidgets) {
        qDebug() << dw << "; affinities=";
    }
}

void DebugWindow::mousePressEvent(QMouseEvent *event)
{
    if (!m_isPickingWidget)
        QWidget::mousePressEvent(event);

    QWidget *w = qApp->widgetAt(Qt5Qt6Compat::eventGlobalPos(event));
    qDebug() << "Widget at pos" << Qt5Qt6Compat::eventGlobalPos(event) << "is"
             << w << "; parent="
             << (w ? w->parentWidget() : nullptr) << "; geometry="
             << (w ? w->geometry() : QRect());

    if (m_isPickingWidget)
        m_isPickingWidget->quit();
}
