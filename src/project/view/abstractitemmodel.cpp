/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
#include "abstractitemmodel.h"

#include "uicomponents/view/modelutils.h"

using namespace au::project;

const QString AbstractItemModel::NAME_KEY("name");
const QString AbstractItemModel::SLUG_KEY("slug");
const QString AbstractItemModel::PATH_KEY("path");
const QString AbstractItemModel::THUMBNAIL_URL_KEY("thumbnailUrl");
const QString AbstractItemModel::FILE_SIZE_KEY("fileSize");
const QString AbstractItemModel::DURATION_KEY("duration");
const QString AbstractItemModel::TIME_SINCE_MODIFIED_KEY("timeSinceModified");
const QString AbstractItemModel::IS_CREATE_NEW_KEY("isCreateNew");
const QString AbstractItemModel::IS_NO_RESULTS_FOUND_KEY("isNoResultsFound");
const QString AbstractItemModel::IS_CLOUD_KEY("isCloud");
const QString AbstractItemModel::CLOUD_ITEM_ID_KEY("itemId");
const QString AbstractItemModel::CLOUD_VISIBILITY_KEY("cloudVisibility");
const QString AbstractItemModel::CLOUD_VIEW_COUNT_KEY("cloudViewCount");

AbstractItemModel::AbstractItemModel(QObject* parent)
    : QAbstractListModel(parent)
{
    muse::uicomponents::ModelUtils::connectRowCountChangedSignal(this, &AbstractItemModel::rowCountChanged);
}

QVariant AbstractItemModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= rowCount()) {
        return QVariant();
    }

    QVariantMap item = m_items[index.row()];

    switch (role) {
    case NameRole: return item[NAME_KEY];
    case IsNoResultsFoundRole: return item[IS_NO_RESULTS_FOUND_KEY];
    case ItemRole: return item;
    }

    return QVariant();
}

int AbstractItemModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_items.size());
}

QHash<int, QByteArray> AbstractItemModel::roleNames() const
{
    static const QHash<int, QByteArray> ROLE_NAMES {
        { NameRole, NAME_KEY.toUtf8() },
        { IsNoResultsFoundRole, IS_NO_RESULTS_FOUND_KEY.toUtf8() },
        { ItemRole, "item" }
    };

    return ROLE_NAMES;
}

QList<int> AbstractItemModel::nonProjectItemIndices() const
{
    return {};
}
