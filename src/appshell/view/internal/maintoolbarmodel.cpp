/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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

#include "maintoolbarmodel.h"

#include "translation.h"

using namespace au::appshell;

static const QString HOME_PAGE("musescore://home");
static const QString PROJECT_PAGE("audacity://project");
static const QString PUBLISH_PAGE("musescore://publish");
static const QString DEVTOOLS_PAGE("musescore://devtools");

static const QString TITLE_KEY("title");
static const QString URI_KEY("uri");
static const QString IS_TITLE_BOLD_KEY("isTitleBold");

inline QVariantMap buildItem(const QString& title, const QString& uri)
{
    QVariantMap item;
    item[TITLE_KEY] = title;
    item[URI_KEY] = uri;
    item[IS_TITLE_BOLD_KEY] = false;

    return item;
}

MainToolBarModel::MainToolBarModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant MainToolBarModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const QVariantMap& item = m_items.at(index.row());
    switch (role) {
    case TitleRole: return item[TITLE_KEY];
    case UriRole: return item[URI_KEY];
    case IsTitleBoldRole: return item[IS_TITLE_BOLD_KEY];
    }

    return QVariant();
}

int MainToolBarModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> MainToolBarModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { TitleRole, TITLE_KEY.toUtf8() },
        { UriRole, URI_KEY.toUtf8() },
        { IsTitleBoldRole, IS_TITLE_BOLD_KEY.toUtf8() },
    };

    return roles;
}

void MainToolBarModel::load()
{
    beginResetModel();

    m_items.clear();
    m_items << buildItem(muse::qtrc("appshell", "Home"), HOME_PAGE);
    m_items << buildItem(muse::qtrc("appshell", "Project"), PROJECT_PAGE);
    m_items << buildItem(muse::qtrc("appshell", "Publish"), PUBLISH_PAGE);

    if (globalConfiguration()->devModeEnabled()) {
        m_items << buildItem(muse::qtrc("appshell", "DevTools"), DEVTOOLS_PAGE);
    }

    endResetModel();

    updateNotationPageItem();
    context()->currentProjectChanged().onNotify(this, [this]() {
        updateNotationPageItem();
    });
}

void MainToolBarModel::updateNotationPageItem()
{
    for (int i = 0; i < m_items.size(); ++i) {
        QVariantMap& item = m_items[i];

        if (item[URI_KEY] == PROJECT_PAGE) {
            item[IS_TITLE_BOLD_KEY] = context()->currentProject() != nullptr;

            QModelIndex modelIndex = index(i);
            emit dataChanged(modelIndex, modelIndex, { IsTitleBoldRole });

            break;
        }
    }
}
