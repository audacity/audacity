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

#ifndef MU_UI_UIARRANGEMENT_H
#define MU_UI_UIARRANGEMENT_H

#include <QString>
#include <QByteArray>
#include <QList>
#include <QJsonObject>
#include <QMap>

#include "modularity/ioc.h"
#include "workspace/iworkspacesdataprovider.h"
#include "async/asyncable.h"
#include "uitypes.h"

namespace mu::ui {
class UiArrangement : public async::Asyncable
{
    INJECT(workspace::IWorkspacesDataProvider, workspacesDataProvider)
public:
    UiArrangement() = default;

    void load();

    QString value(const QString& key) const;
    void setValue(const QString& key, const QString& val);
    async::Notification valueChanged(const QString& key) const;

    QByteArray state(const QString& key) const;
    void setState(const QString& key, const QByteArray& data);
    async::Notification stateChanged(const QString& key) const;

    ToolConfig toolConfig(const QString& toolName) const;
    void setToolConfig(const QString& toolName, const ToolConfig& config);
    async::Notification toolConfigChanged(const QString& toolName) const;

private:

    using Notifications = QMap<QString, async::Notification>;

    void updateData(workspace::DataKey key, QJsonObject& obj, Notifications& notifications) const;
    void saveData(workspace::DataKey key, const QJsonObject& obj);

    QJsonObject m_settings;
    mutable Notifications m_valuesNotifications;

    QJsonObject m_states;
    mutable Notifications m_statesNotifications;

    QJsonObject m_toolconfigs;
    mutable Notifications m_toolconfigsNotifications;
};
}

#endif // MU_UI_UIARRANGEMENT_H
