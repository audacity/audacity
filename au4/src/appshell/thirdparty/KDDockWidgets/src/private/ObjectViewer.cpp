/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Tree Widget to show the object tree. Used for debugging only, for apps that don't support GammaRay.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "ObjectViewer_p.h"

#include <QStandardItem>
#include <QApplication>
#include <QHBoxLayout>
#include <QMenu>
#include <QContextMenuEvent>
#include <QItemSelectionModel>
#include <QPainter>
#include <QDebug>
#include <QMetaProperty>
#include <QWindow>
#include <QToolBar>
#include <QShortcut>
#include <QDir>

#ifdef Q_OS_WIN
#include <windows.h>
#endif

using namespace KDDockWidgets::Debug;

enum Role
{
    ObjRole = Qt::UserRole
};


ObjectViewer::ObjectViewer(QWidget *parent)
    : QWidget(parent)
{
    resize(600, 600);

    auto lay = new QHBoxLayout(this);
    lay->addWidget(&m_treeView);
    m_treeView.setModel(&m_model);
    connect(m_treeView.selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ObjectViewer::onSelectionChanged);

    QAction *action = m_menu.addAction(QStringLiteral("Refresh"));
    connect(action, &QAction::triggered, this, &ObjectViewer::refresh);
    action = m_menu.addAction(QStringLiteral("Dump Windows"));
    connect(action, &QAction::triggered, this, &ObjectViewer::dumpWindows);

    action = m_menu.addAction(QStringLiteral("Update"));
    connect(action, &QAction::triggered, this, &ObjectViewer::updateSelectedWidget);

    action = m_menu.addAction(QStringLiteral("Print to png"));
    connect(action, &QAction::triggered, this, &ObjectViewer::dumpSelectedWidgetToPng);

    action = m_menu.addAction(QStringLiteral("Toggle Visible"));
    connect(action, &QAction::triggered, this, &ObjectViewer::toggleVisible);

#ifdef Q_OS_WIN
    action = m_menu.addAction(QStringLiteral("Send WM_NCHITTEST"));
    connect(action, &QAction::triggered, this, &ObjectViewer::sendHitTest);
#endif

    refresh();
    setWindowTitle(QStringLiteral("ObjectViewer"));
}

void ObjectViewer::refresh()
{
    const auto hashCopy = m_itemMap;
    for (auto it = hashCopy.cbegin(), e = hashCopy.cend(); it != e; ++it)
        remove(it.key());

    m_model.clear();

    const auto &topLevelWidgets = qApp->topLevelWidgets();
    for (QWidget *window : topLevelWidgets) {
        add(window, m_model.invisibleRootItem());
    }
}

void ObjectViewer::dumpSelectedWidgetToPng()
{
    if (auto w = selectedWidget()) {
        QPixmap px(w->size());
        w->render(&px);
        px.save(QStringLiteral("px.png"));
        qDebug() << QDir::currentPath();
    }
}

void ObjectViewer::updateSelectedWidget()
{
    if (auto w = selectedWidget())
        w->update();
}

void ObjectViewer::toggleVisible()
{
    if (auto w = selectedWidget())
        w->setVisible(!w->isVisible());
}

#ifdef Q_OS_WIN
void ObjectViewer::sendHitTest()
{
    if (auto w = selectedWidget()) {
        qDebug() << "Sending hit test to" << w;
        ::SendMessage(HWND(w->winId()), WM_NCHITTEST, 0, 0);
    }
}
#endif

void ObjectViewer::dumpWindows()
{
    qDebug() << "Top Level QWidgets:";
    const auto &topLevelWidgets = qApp->topLevelWidgets();
    for (QWidget *w : topLevelWidgets) {
        if (qobject_cast<QMenu *>(w))
            continue;

        qDebug() << "    QWidget=" << w;
    }

    qDebug() << "Top Level Windows:";
    const auto &topLevelWindows = qApp->topLevelWindows();
    for (QWindow *w : topLevelWindows) {
        qDebug() << "    QWindow=" << w << "; parent=" << w->parent() << "; transientParent=" << w->transientParent() << "; hwnd=" << w->winId();
    }
}

QString ObjectViewer::nameForObj(QObject *o) const
{
    QString name = QString::fromLatin1(o->metaObject()->className());
    if (!o->objectName().isEmpty())
        name += QStringLiteral("(%1)").arg(o->objectName());

    if (auto w = qobject_cast<QWidget *>(o)) {
        name += QStringLiteral(" - %1,%2 %3x%4").arg(w->x()).arg(w->y()).arg(w->width()).arg(w->height());

        if (w->isWindow())
            name += QStringLiteral(" ;W");
        if (w->windowHandle() != nullptr)
            name += QStringLiteral(" ;N");
    }

    return name;
}

void ObjectViewer::add(QObject *obj, QStandardItem *parent)
{
    if (obj == this || obj == &m_menu || obj == parentWidget() || !obj) // Ignore our stuff
        return;

    if (m_ignoreMenus && qobject_cast<QMenu *>(obj))
        return;

    if (m_ignoreShortcuts && qobject_cast<QShortcut *>(obj))
        return;

    if (m_ignoreToolBars && qobject_cast<QToolBar *>(obj))
        return;

    connect(obj, &QObject::destroyed, this, &ObjectViewer::remove);
    obj->installEventFilter(this);
    auto item = new QStandardItem(nameForObj(obj));
    item->setData(QVariant::fromValue(obj), ObjRole);
    m_itemMap.insert(obj, item);
    parent->appendRow(item);
    updateItemAppearence(item);

    for (auto child : obj->children())
        add(child, item);
}

void ObjectViewer::remove(QObject *obj)
{
    Q_ASSERT(obj);
    obj->removeEventFilter(this);
    m_itemMap.remove(obj);
}

void ObjectViewer::onSelectionChanged()
{
    QObject *o = selectedObject();
    if (m_selectedObject == o)
        return;

    if (m_selectedObject) {
        m_selectedObject->removeEventFilter(this);
        if (auto w = qobject_cast<QWidget *>(m_selectedObject))
            w->update();
    }

    m_selectedObject = o;

    if (m_selectedObject) {
        printProperties(o);
        m_selectedObject->installEventFilter(this);
        if (m_highlightsWidget) {
            if (auto w = qobject_cast<QWidget *>(o))
                w->update();
        }
    }
}

void ObjectViewer::printProperties(QObject *obj) const
{
    qDebug() << "Printing properties for" << obj;

    auto mo = obj->metaObject();
    const int count = mo->propertyCount();
    for (int i = 0; i < count; ++i) {
        QMetaProperty prop = mo->property(i);
        qDebug() << "    " << prop.name() << prop.read(obj);
    }

    if (auto w = qobject_cast<QWidget *>(obj)) {
        qDebug() << "Is a widget!";
        qDebug() << "Window=" << w->window();
        qDebug() << "flags=" << w->windowFlags();
        qDebug() << "is native?" << (w->windowHandle() != nullptr);
    }
}

QObject *ObjectViewer::selectedObject() const
{
    auto indexes = m_treeView.selectionModel()->selectedIndexes();
    if (indexes.isEmpty())
        return nullptr;
    QModelIndex index = indexes.first();
    QObject *obj = index.data(ObjRole).value<QObject *>();
    return obj;
}

QWidget *ObjectViewer::selectedWidget() const
{
    return qobject_cast<QWidget *>(selectedObject());
}

void ObjectViewer::updateItemAppearence(QStandardItem *item)
{
    Q_ASSERT(item);
    QWidget *widget = widgetForItem(item);
    if (!widget)
        return;

    if (widget->isVisible()) {
        item->setForeground(Qt::black);
    } else {
        item->setForeground(Qt::gray);
    }
}

QObject *ObjectViewer::objectForItem(QStandardItem *item) const
{
    return item->data(ObjRole).value<QObject *>();
}

QWidget *ObjectViewer::widgetForItem(QStandardItem *item) const
{
    return qobject_cast<QWidget *>(objectForItem(item));
}

void ObjectViewer::contextMenuEvent(QContextMenuEvent *ev)
{
    m_menu.exec(ev->globalPos());
}

bool ObjectViewer::eventFilter(QObject *watched, QEvent *event)
{
    auto widget = static_cast<QWidget *>(watched);
    if (event->type() == QEvent::Show || event->type() == QEvent::Hide) {
        updateItemAppearence(m_itemMap.value(watched));
        return false;
    }

    if (m_selectedObject != watched)
        return false;

    if (event->type() != QEvent::Paint || !m_highlightsWidget)
        return false;

    QPainter p(widget);
    p.fillRect(widget->rect(), QBrush(QColor(0, 0, 255, 128)));

    return true;
}
