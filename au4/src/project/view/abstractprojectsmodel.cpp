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
#include "abstractprojectsmodel.h"

#include "uicomponents/view/modelutils.h"

using namespace au::project;

const QString AbstractProjectsModel::NAME_KEY("name");
const QString AbstractProjectsModel::PATH_KEY("path");
const QString AbstractProjectsModel::SUFFIX_KEY("suffix");
const QString AbstractProjectsModel::FILE_SIZE_KEY("fileSize");
const QString AbstractProjectsModel::THUMBNAIL_URL_KEY("thumbnailUrl");
const QString AbstractProjectsModel::TIME_SINCE_MODIFIED_KEY("timeSinceModified");
const QString AbstractProjectsModel::IS_CREATE_NEW_KEY("isCreateNew");
const QString AbstractProjectsModel::IS_NO_RESULTS_FOUND_KEY("isNoResultsFound");
const QString AbstractProjectsModel::IS_CLOUD_KEY("isCloud");
const QString AbstractProjectsModel::CLOUD_PROJECT_ID_KEY("projectId");
const QString AbstractProjectsModel::CLOUD_VISIBILITY_KEY("cloudVisibility");
const QString AbstractProjectsModel::CLOUD_VIEW_COUNT_KEY("cloudViewCount");

AbstractProjectsModel::AbstractProjectsModel(QObject* parent)
    : QAbstractListModel(parent)
{
    muse::uicomponents::ModelUtils::connectRowCountChangedSignal(this, &AbstractProjectsModel::rowCountChanged);
}

QVariant AbstractProjectsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= rowCount()) {
        return QVariant();
    }

    QVariantMap item = m_items[index.row()];

    switch (role) {
    case NameRole: return item[NAME_KEY];
    case IsNoResultsFoundRole: return item[IS_NO_RESULTS_FOUND_KEY];
    case ProjectRole: return item;
    }

    return QVariant();
}

int AbstractProjectsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_items.size());
}

QHash<int, QByteArray> AbstractProjectsModel::roleNames() const
{
    static const QHash<int, QByteArray> ROLE_NAMES {
        { NameRole, NAME_KEY.toUtf8() },
        { IsNoResultsFoundRole, IS_NO_RESULTS_FOUND_KEY.toUtf8() },
        { ProjectRole, "project" }
    };

    return ROLE_NAMES;
}

QList<int> AbstractProjectsModel::nonProjectItemIndices() const
{
    return {};
}
