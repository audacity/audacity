/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#include "preferencepageitem.h"

#include "translation.h"

using namespace au::appshell;

PreferencePageItem::PreferencePageItem(QObject* parent)
    : QObject(parent)
{
}

PreferencePageItem::~PreferencePageItem()
{
    for (PreferencePageItem* child : m_children) {
        child->deleteLater();
    }
}

QString PreferencePageItem::id() const
{
    return m_id;
}

QString PreferencePageItem::title() const
{
    return muse::qtrc("appshell/preferences", m_title.toUtf8());
}

int PreferencePageItem::icon() const
{
    return static_cast<int>(m_icon);
}

QString PreferencePageItem::path() const
{
    return m_path;
}

bool PreferencePageItem::expanded() const
{
    return m_expanded;
}

PreferencePageItem* PreferencePageItem::parentItem() const
{
    return m_parent;
}

void PreferencePageItem::setParentItem(PreferencePageItem* parent)
{
    m_parent = parent;
}

QList<PreferencePageItem*> PreferencePageItem::childrenItems() const
{
    return m_children;
}

bool PreferencePageItem::isEmpty() const
{
    return m_children.isEmpty();
}

PreferencePageItem* PreferencePageItem::childAtRow(const int row) const
{
    if (row < 0 || row >= childCount()) {
        return nullptr;
    }

    return m_children.at(row);
}

void PreferencePageItem::appendChild(PreferencePageItem* child)
{
    if (!child) {
        return;
    }

    child->setParentItem(this);

    m_children.append(child);
}

int PreferencePageItem::childCount() const
{
    return m_children.size();
}

int PreferencePageItem::row() const
{
    if (!parentItem()) {
        return 0;
    }

    return parentItem()->childrenItems().indexOf(const_cast<PreferencePageItem*>(this));
}

void PreferencePageItem::setId(QString id)
{
    if (m_id == id) {
        return;
    }

    m_id = id;
    emit idChanged(m_id);
}

void PreferencePageItem::setTitle(QString title)
{
    if (m_title == title) {
        return;
    }

    m_title = title;
    emit titleChanged(m_title);
}

void PreferencePageItem::setIcon(muse::ui::IconCode::Code icon)
{
    if (m_icon == icon) {
        return;
    }

    m_icon = icon;
    emit iconChanged(this->icon());
}

void PreferencePageItem::setPath(QString path)
{
    if (m_path == path) {
        return;
    }

    m_path = path;
    emit pathChanged(m_path);
}

void PreferencePageItem::setExpanded(bool expanded)
{
    if (m_expanded == expanded) {
        return;
    }

    m_expanded = expanded;
    emit expandedChanged(m_expanded);
}
