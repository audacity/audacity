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
#ifndef AU_APPSHELL_PREFERENCEPAGEITEM_H
#define AU_APPSHELL_PREFERENCEPAGEITEM_H

#include <QObject>
#include <QVariantMap>

#include "ui/view/iconcodes.h"

namespace au::appshell {
class PreferencePageItem : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString id READ id NOTIFY idChanged)
    Q_PROPERTY(QString title READ title NOTIFY titleChanged)
    Q_PROPERTY(int icon READ icon NOTIFY iconChanged)
    Q_PROPERTY(QString path READ path NOTIFY pathChanged)
    Q_PROPERTY(bool expanded READ expanded NOTIFY expandedChanged)

public:
    explicit PreferencePageItem(QObject* parent = nullptr);
    virtual ~PreferencePageItem();

    QString id() const;
    QString title() const;
    int icon() const;
    QString path() const;
    bool expanded() const;

    PreferencePageItem* parentItem() const;
    void setParentItem(PreferencePageItem* parent);

    QList<PreferencePageItem*> childrenItems() const;
    bool isEmpty() const;

    PreferencePageItem* childAtRow(const int row) const;

    void appendChild(PreferencePageItem* child);

    int childCount() const;
    int row() const;

public slots:
    void setTitle(QString title);
    void setId(QString id);
    void setIcon(muse::ui::IconCode::Code icon);
    void setPath(QString path);
    void setExpanded(bool expanded);

signals:
    void idChanged(QString id);
    void titleChanged(QString title);
    void iconChanged(int icon);
    void pathChanged(QString path);
    void expandedChanged(bool expanded);

private:
    QList<PreferencePageItem*> m_children;
    PreferencePageItem* m_parent = nullptr;

    QString m_title;
    QString m_id;
    muse::ui::IconCode::Code m_icon = muse::ui::IconCode::Code::NONE;
    QString m_path;
    bool m_expanded = false;
};
}

#endif // AU_APPSHELL_PREFERENCEPAGEITEM_H
