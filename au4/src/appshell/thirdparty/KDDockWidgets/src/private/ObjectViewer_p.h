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

#ifndef OBJECTVIEWER_H
#define OBJECTVIEWER_H

#include <QWidget>
#include <QStandardItemModel>
#include <QTreeView>
#include <QPointer>
#include <QObject>
#include <QMenu>

QT_BEGIN_NAMESPACE
class QStandardItem;
QT_END_NAMESPACE

namespace KDDockWidgets {
namespace Debug {

class ObjectViewer : public QWidget //clazy:exclude=missing-qobject-macro
{
public:
    explicit ObjectViewer(QWidget *parent = nullptr);

    void refresh();

private:
    void dumpSelectedWidgetToPng();
    void updateSelectedWidget();
    void toggleVisible();
    void dumpWindows();
    QString nameForObj(QObject *o) const;
    void add(QObject *obj, QStandardItem *parent);
    void remove(QObject *obj);
    void onSelectionChanged();
    void printProperties(QObject *) const;
    QObject *selectedObject() const;
    QWidget *selectedWidget() const;
    void updateItemAppearence(QStandardItem *);
    QObject *objectForItem(QStandardItem *) const;
    QWidget *widgetForItem(QStandardItem *) const;

#ifdef Q_OS_WIN
    void sendHitTest();
#endif

    QTreeView m_treeView;
    QStandardItemModel m_model;
    QPointer<QObject> m_selectedObject;
    QMenu m_menu;
    bool m_highlightsWidget = true;
    bool m_ignoreMenus = true;
    bool m_ignoreShortcuts = true;
    bool m_ignoreToolBars = true;
    QHash<QObject *, QStandardItem *> m_itemMap;

protected:
    void contextMenuEvent(QContextMenuEvent *event) override;
    bool eventFilter(QObject *watched, QEvent *event) override;
};
}
}

#endif
