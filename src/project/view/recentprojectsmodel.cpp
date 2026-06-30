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
#include "recentprojectsmodel.h"

#include "framework/global/translation.h"
#include "framework/global/dataformatter.h"
#include "framework/global/io/fileinfo.h"

#include "view/recentprojectcontextmenumodel.h"

using namespace muse;
using namespace au::project;

RecentProjectsModel::RecentProjectsModel(QObject* parent)
    : AbstractItemModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void RecentProjectsModel::load()
{
    updateRecentProjects();

    recentFilesController()->recentFilesListChanged().onNotify(this, [this]() {
        updateRecentProjects();
    });
}

void RecentProjectsModel::setRecentProjects(const std::vector<QVariantMap>& items)
{
    if (m_items == items) {
        return;
    }

    beginResetModel();
    clearContextMenuModels();
    m_items = items;
    endResetModel();
}

void RecentProjectsModel::clearContextMenuModels()
{
    for (QVariantMap& item : m_items) {
        auto* model = item[CONTEXT_MENU_MODEL_KEY].value<RecentProjectContextMenuModel*>();
        item[CONTEXT_MENU_MODEL_KEY] = QVariant();
        if (model != nullptr) {
            model->deleteLater();
        }
    }
}

void RecentProjectsModel::updateRecentProjects()
{
    const RecentFilesList& recentProjects = recentFilesController()->recentFilesList();

    std::vector<QVariantMap> items;
    items.reserve(recentProjects.size());

    QVariantMap addItem;
    addItem[NAME_KEY] = muse::qtrc("project", "New project");
    addItem[IS_CREATE_NEW_KEY] = true;
    addItem[IS_NO_RESULTS_FOUND_KEY] = false;
    addItem[IS_CLOUD_KEY] = false;
    items.push_back(addItem);

    for (const RecentFile& file : recentProjects) {
        QVariantMap obj;

        std::string suffix = io::suffix(file.path);

        RetVal<uint64_t> fileSize = fileSystem()->fileSize(file.path);
        QString fileSizeString = (fileSize.ret && fileSize.val > 0) ? DataFormatter::formatFileSize(fileSize.val).toQString() : QString();

        const bool isCloud = audioComService()->isCloudProject(file.path);
        const QString cloudProjectId = isCloud
                                       ? QString::fromStdString(audioComService()->cloudProjectId(file.path).value_or(std::string()))
                                       : QString();

        obj[NAME_KEY] = file.displayName(false);
        obj[PATH_KEY] = file.path.toQString();
        obj[THUMBNAIL_URL_KEY] = obj[PATH_KEY];
        obj[FILE_SIZE_KEY] = fileSizeString;
        obj[IS_CLOUD_KEY] = isCloud;
        obj[CLOUD_ITEM_ID_KEY] = cloudProjectId;
        obj[SHOW_INDICATOR_KEY] = obj[IS_CLOUD_KEY];
        obj[TIME_SINCE_MODIFIED_KEY] = DataFormatter::formatTimeSince(io::FileInfo(file.path).lastModified().date()).toQString();
        obj[IS_CREATE_NEW_KEY] = false;
        obj[IS_NO_RESULTS_FOUND_KEY] = false;

        obj[CONTEXT_MENU_MODEL_KEY] = QVariant::fromValue(
            new RecentProjectContextMenuModel(isCloud, file.path.toQString(), file.displayNameOverride, cloudProjectId, this));

        items.push_back(obj);
    }

    QVariantMap noResultsFoundItem;
    noResultsFoundItem[NAME_KEY] = "";
    noResultsFoundItem[IS_CREATE_NEW_KEY] = false;
    noResultsFoundItem[IS_NO_RESULTS_FOUND_KEY] = true;
    noResultsFoundItem[IS_CLOUD_KEY] = false;
    items.push_back(noResultsFoundItem);

    setRecentProjects(items);
}

QList<int> RecentProjectsModel::nonProjectItemIndices() const
{
    return { 0, rowCount() - 1 };
}
