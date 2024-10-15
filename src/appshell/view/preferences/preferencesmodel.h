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
#ifndef AU_APPSHELL_PREFERENCESMODEL_H
#define AU_APPSHELL_PREFERENCESMODEL_H

#include <QAbstractItemModel>

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "ui/iuiactionsregister.h"
#include "ui/view/iconcodes.h"

#include "iappshellconfiguration.h"
#include "preferencepageitem.h"

namespace au::appshell {
class PreferencesModel : public QAbstractItemModel
{
    Q_OBJECT

    INJECT(muse::actions::IActionsDispatcher, dispatcher)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(muse::ui::IUiActionsRegister, actionsRegister)

    Q_PROPERTY(QString currentPageId READ currentPageId WRITE setCurrentPageId NOTIFY currentPageIdChanged)

public:
    explicit PreferencesModel(QObject* parent = nullptr);
    ~PreferencesModel();

    QModelIndex index(int row, int column, const QModelIndex& parent) const override;
    QModelIndex parent(const QModelIndex& child) const override;
    int rowCount(const QModelIndex& parent) const override;
    int columnCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString currentPageId() const;

    Q_INVOKABLE void load(const QString& currentPageId);
    Q_INVOKABLE void resetFactorySettings();
    Q_INVOKABLE void apply();
    Q_INVOKABLE void cancel();
    Q_INVOKABLE void selectRow(const QModelIndex& rowIndex);

    Q_INVOKABLE QVariantList availablePages() const;

public slots:
    void setCurrentPageId(QString currentPageId);

signals:
    void currentPageIdChanged(QString currentPageId);

private:

    enum RoleNames {
        ItemRole = Qt::UserRole + 1
    };

    PreferencePageItem* makeItem(const QString& id, const QString& title, muse::ui::IconCode::Code icon = muse::ui::IconCode::Code::NONE,
                                 const QString& path = "", const QList<PreferencePageItem*>& children = {}) const;

    PreferencePageItem* modelIndexToItem(const QModelIndex& index) const;

    PreferencePageItem* m_rootItem = nullptr;
    QString m_currentPageId;
};
}

#endif // AU_APPSHELL_PREFERENCESMODEL_H
