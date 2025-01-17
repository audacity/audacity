/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity Limited
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
#include "projectpropertiesmodel.h"

#include "translation.h"
#include "log.h"

using namespace muse;
using namespace muse::modularity;
using namespace au::project;

ProjectPropertiesModel::ProjectPropertiesModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ProjectPropertiesModel::init()
{
    m_project = globalContext()->currentProject();

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        m_project = globalContext()->currentProject();
    });

    if (!m_project) {
        return;
    }

    // if (project) {
    //     m_projectMetaInfo = project->metaInfo();
    // }

    thumbnailCreator()->captureThumbnailRequested().onReceive(this, [this](const muse::io::path_t& response) {
        captureThumbnail(response.toQString());
    });

    load();
}

void ProjectPropertiesModel::onThumbnailCreated(bool success)
{
    thumbnailCreator()->onThumbnailCreated(success);
}

void ProjectPropertiesModel::load()
{
    beginResetModel();

    QVariantMap additionalProperties = m_projectMetaInfo.additionalTags;

    m_properties = {
        //! TODO AU4: add properties according to
        //! /au4/src/project/types/projectmeta.h
        // { WORK_TITLE_TAG, muse::qtrc("project", "Title"), m_projectMetaInfo.title, true },
        // { AUDIO_COM_URL_TAG, muse::qtrc("project", "Audio.com URL"), m_projectMetaInfo.audioComUrl, true }
    };

    for (const QString& propertyName : additionalProperties.keys()) {
        if (!isStandardTag(propertyName)) {
            m_properties.append({ "", propertyName, additionalProperties[propertyName].toString() });
        }
    }

    endResetModel();
}

QVariant ProjectPropertiesModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const Property& property = m_properties[index.row()];

    switch (role) {
    case PropertyName:
        return property.name;
    case PropertyValue:
        return property.value;
    case IsStandardProperty:
        return property.isStandardProperty;
    case IsMultiLineEdit:
        return property.isMultiLineEdit;
    }

    return QVariant();
}

bool ProjectPropertiesModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return false;
    }

    Property& property = m_properties[index.row()];

    switch (role) {
    case PropertyName:
        if (!value.canConvert<QString>()) {
            return false;
        }
        property.name = value.toString();
        return true;
    case PropertyValue:
        if (!value.canConvert<QString>()) {
            return false;
        }
        property.value = value.toString();
        return true;
    default:
        return false;
    }
}

int ProjectPropertiesModel::rowCount(const QModelIndex&) const
{
    return m_properties.size();
}

QHash<int, QByteArray> ProjectPropertiesModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { PropertyName, "propertyName" },
        { PropertyValue, "propertyValue" },
        { IsStandardProperty, "isStandardProperty" },
        { IsMultiLineEdit, "isMultiLineEdit" }
    };

    return roles;
}

QString ProjectPropertiesModel::filePath() const
{
    return m_project->path().toQString();
}

QString ProjectPropertiesModel::version() const
{
    NOT_IMPLEMENTED;
    return QString();
}

QString ProjectPropertiesModel::revision() const
{
    NOT_IMPLEMENTED;
    return QString();
}

QString ProjectPropertiesModel::apiLevel() const
{
    NOT_IMPLEMENTED;
    return QString();
}

void ProjectPropertiesModel::newProperty()
{
    int destinationIndex = m_properties.size();
    beginInsertRows(QModelIndex(), destinationIndex, destinationIndex);

    Property property = { "", "", "" };
    m_properties.append(property);

    endInsertRows();

    emit propertyAdded(destinationIndex);
}

void ProjectPropertiesModel::deleteProperty(int index)
{
    if (index < 0 && index >= m_properties.size()) {
        return;
    }

    beginRemoveRows(QModelIndex(), index, index);

    m_properties.removeAt(index);

    endRemoveRows();
}

void ProjectPropertiesModel::saveProperties()
{
    NOT_IMPLEMENTED;
}

void ProjectPropertiesModel::openFileLocation()
{
    Ret ret = interactive()->revealInFileBrowser(m_projectMetaInfo.filePath.toQString());

    if (!ret) {
        LOGE() << "Could not open folder: " << m_projectMetaInfo.filePath.toQString();
    }
}
