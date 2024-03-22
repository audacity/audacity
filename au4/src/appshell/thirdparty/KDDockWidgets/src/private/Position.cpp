/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file Helper class so dockwidgets can be restored to their previous position.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "Position_p.h"
#include "DockRegistry_p.h"
#include "FloatingWindow_p.h"
#include "LayoutSaver_p.h"
#include "LayoutWidget_p.h"

#include <algorithm>

using namespace KDDockWidgets;

Position::~Position()
{
    m_placeholders.clear();
}

void Position::addPlaceholderItem(Layouting::Item *placeholder)
{
    Q_ASSERT(placeholder);

    // 1. Already exists, nothing to do
    if (containsPlaceholder(placeholder))
        return;

    if (DockRegistry::self()->itemIsInMainWindow(placeholder)) {
        // 2. If we have a MainWindow placeholder we don't need nothing else
        removePlaceholders();
    } else {
        // 3. It's a placeholder to a FloatingWindow. Let's still keep any MainWindow placeholders we have
        // as FloatingWindow are temporary so we might need the MainWindow placeholder later.
        removeNonMainWindowPlaceholders();
    }

    // Make sure our list only contains valid placeholders. We save the result so we can disconnect from the lambda, since the Item might outlive LastPosition
    QMetaObject::Connection connection = QObject::connect(placeholder, &QObject::destroyed, placeholder, [this, placeholder] {
        removePlaceholder(placeholder);
    });

    m_placeholders.push_back(std::unique_ptr<ItemRef>(new ItemRef(connection, placeholder)));

    // NOTE: We use a list instead of simply two variables to keep the placeholders, because
    // a placeholder from a FloatingWindow might become a MainWindow one without we knowing,
    // like when dragging a floating window into a MainWindow. So, isInMainWindow() won't return
    // the same value always, hence we just shove them into a list, instead of giving them meaningful names in separated variables
}

Layouting::Item *Position::layoutItem() const
{
    // Return the layout item that is in a MainWindow, that's where we restore the dock widget to.
    // In the future we might want to restore it to FloatingWindows.

    for (const auto &itemref : m_placeholders) {
        if (DockRegistry::self()->itemIsInMainWindow(itemref->item))
            return itemref->item;
    }

    return nullptr;
}

bool Position::containsPlaceholder(Layouting::Item *item) const
{
    for (const auto &itemRef : m_placeholders)
        if (itemRef->item == item)
            return true;

    return false;
}

void Position::removePlaceholders()
{
    QScopedValueRollback<bool> clearGuard(m_clearing, true);
    m_placeholders.clear();
}

void Position::removePlaceholders(const LayoutWidget *ms)
{
    m_placeholders.erase(std::remove_if(m_placeholders.begin(), m_placeholders.end(), [ms](const std::unique_ptr<ItemRef> &itemref) {
                             return itemref->item->hostWidget() == *ms;
                         }),
                         m_placeholders.end());
}

void Position::removeNonMainWindowPlaceholders()
{
    auto it = m_placeholders.begin();
    while (it != m_placeholders.end()) {
        ItemRef *itemref = it->get();
        if (!DockRegistry::self()->itemIsInMainWindow(itemref->item))
            it = m_placeholders.erase(it);
        else
            ++it;
    }
}

void Position::removePlaceholder(Layouting::Item *placeholder)
{
    if (m_clearing) // reentrancy guard
        return;

    m_placeholders.erase(std::remove_if(m_placeholders.begin(), m_placeholders.end(), [placeholder](const std::unique_ptr<ItemRef> &itemref) {
                             return itemref->item == placeholder;
                         }),
                         m_placeholders.end());
}

void Position::deserialize(const LayoutSaver::Position &lp)
{
    for (const auto &placeholder : std::as_const(lp.placeholders)) {
        LayoutWidget *layout;
        int itemIndex = placeholder.itemIndex;
        if (placeholder.isFloatingWindow) {
            const int index = placeholder.indexOfFloatingWindow;
            if (index == -1) {
                continue; // Skip
            } else {
                auto serializedFw = LayoutSaver::Layout::s_currentLayoutBeingRestored->floatingWindowForIndex(index);
                if (serializedFw.isValid()) {
                    if (FloatingWindow *fw = serializedFw.floatingWindowInstance) {
                        layout = fw->layoutWidget();
                    } else {
                        continue;
                    }
                } else {
                    qWarning() << "Invalid floating window position to restore" << index;
                    continue;
                }
            }
        } else {
            MainWindowBase *mainWindow = DockRegistry::self()->mainWindowByName(placeholder.mainWindowUniqueName);
            layout = mainWindow->layoutWidget();
        }

        const Layouting::Item::List &items = layout->items();
        if (itemIndex >= 0 && itemIndex < items.size()) {
            Layouting::Item *item = items.at(itemIndex);
            addPlaceholderItem(item);
        } else {
            // Shouldn't happen, maybe even assert
            qWarning() << Q_FUNC_INFO << "Couldn't find item index" << itemIndex << "in" << items;
        }
    }

    m_tabIndex = lp.tabIndex;
    m_wasFloating = lp.wasFloating;
}

LayoutSaver::Position Position::serialize() const
{
    LayoutSaver::Position l;

    for (auto &itemRef : m_placeholders) {
        LayoutSaver::Placeholder p;

        Layouting::Item *item = itemRef->item;
        LayoutWidget *layout = DockRegistry::self()->layoutForItem(item);
        const auto itemIndex = layout->items().indexOf(item);

        auto fw = layout->floatingWindow();
        auto mainWindow = layout->mainWindow();
        Q_ASSERT(mainWindow || fw);
        p.isFloatingWindow = fw;

        if (p.isFloatingWindow) {
            p.indexOfFloatingWindow = fw->beingDeleted() ? -1 : DockRegistry::self()->floatingWindows().indexOf(fw); // TODO: Remove once we stop using deleteLater with FloatingWindow. delete would be better
        } else {
            p.mainWindowUniqueName = mainWindow->uniqueName();
            Q_ASSERT(!p.mainWindowUniqueName.isEmpty());
        }

        p.itemIndex = itemIndex;
        l.placeholders.push_back(p);
    }

    l.tabIndex = m_tabIndex;
    l.wasFloating = m_wasFloating;

    return l;
}

ItemRef::ItemRef(const QMetaObject::Connection &conn, Layouting::Item *it)
    : item(it)
    , guard(it)
    , connection(conn)
{
    item->ref();
}

ItemRef::~ItemRef()
{
    if (guard) {
        QObject::disconnect(connection);
        item->unref();
    }
}

LayoutSaver::Position LastPositions::serialize()
{
    LayoutSaver::Position result = lastPosition->serialize();
    result.lastFloatingGeometry = lastFloatingGeometry();
    result.lastOverlayedGeometries = m_lastOverlayedGeometries;
    return result;
}

void LastPositions::deserialize(const LayoutSaver::Position &p)
{
    m_lastFloatingGeometry = p.lastFloatingGeometry;
    m_lastOverlayedGeometries = p.lastOverlayedGeometries;
    lastPosition->deserialize(p);
}
