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
#ifndef AU_PROJECT_ABSTRACTPROJECTSMODEL_H
#define AU_PROJECT_ABSTRACTPROJECTSMODEL_H

#include <QAbstractListModel>

namespace au::project {
class AbstractProjectsModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(int rowCount READ rowCount NOTIFY rowCountChanged)
    Q_PROPERTY(QList<int> nonProjectItemIndices READ nonProjectItemIndices NOTIFY rowCountChanged)

public:
    explicit AbstractProjectsModel(QObject* parent = nullptr);

    Q_INVOKABLE virtual void load() = 0;

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    virtual QList<int> nonProjectItemIndices() const;

signals:
    void rowCountChanged();

protected:
    enum Roles {
        NameRole = Qt::UserRole + 1,
        IsNoResultsFoundRole,
        ProjectRole
    };

    static const QString NAME_KEY;
    static const QString PATH_KEY;
    static const QString SUFFIX_KEY;
    static const QString FILE_SIZE_KEY;
    static const QString THUMBNAIL_URL_KEY;
    static const QString TIME_SINCE_MODIFIED_KEY;
    static const QString IS_CREATE_NEW_KEY;
    static const QString IS_NO_RESULTS_FOUND_KEY;
    static const QString IS_CLOUD_KEY;
    static const QString CLOUD_PROJECT_ID_KEY;
    static const QString CLOUD_VISIBILITY_KEY;
    static const QString CLOUD_VIEW_COUNT_KEY;

    std::vector<QVariantMap> m_items;
};
}

#endif // AU_PROJECT_ABSTRACTPROJECTSMODEL_H
