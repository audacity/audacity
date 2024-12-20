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

#pragma once

#include "iinteractive.h"
#include "ithumbnailcreator.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

namespace au::project {
class ProjectPropertiesModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<IThumbnailCreator> thumbnailCreator;

    Q_PROPERTY(QString filePath READ filePath CONSTANT)
    Q_PROPERTY(QString version READ version CONSTANT)
    Q_PROPERTY(QString revision READ revision CONSTANT)
    Q_PROPERTY(QString apiLevel READ apiLevel CONSTANT)

public:
    explicit ProjectPropertiesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void onThumbnailCreated(bool success);

    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString filePath() const;
    QString version() const;
    QString revision() const;
    QString apiLevel() const;

    Q_INVOKABLE void load();
    Q_INVOKABLE void newProperty();
    Q_INVOKABLE void deleteProperty(int index);
    Q_INVOKABLE void saveProperties();
    Q_INVOKABLE void openFileLocation();

signals:
    void propertyAdded(int index);
    void captureThumbnail(QString path);

private:
    enum Roles {
        PropertyName = Qt::UserRole + 1,
        PropertyValue,
        IsStandardProperty,
        IsMultiLineEdit
    };

    struct Property {
        QString key, name, value;
        bool isStandardProperty = false;
        bool isMultiLineEdit = false;
    };

    project::ProjectMeta m_projectMetaInfo;
    QList<Property> m_properties;
    IAudacityProjectPtr m_project;
};
}
