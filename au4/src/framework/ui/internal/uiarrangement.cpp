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

#include "uiarrangement.h"

#include <QJsonDocument>
#include <QJsonValue>
#include <QJsonArray>

#include "log.h"

using namespace mu::ui;
#ifdef MU_BUILD_WORKSPACE_MODULE
using namespace mu::workspace;
#endif
void UiArrangement::load()
{
#ifdef MU_BUILD_WORKSPACE_MODULE
    workspacesDataProvider()->workspaceChanged().onNotify(this, [this]() {
        updateData(DataKey::UiSettings, m_settings, m_valuesNotifications);
        updateData(DataKey::UiStates, m_states, m_statesNotifications);
        updateData(DataKey::UiToolConfigs, m_toolconfigs, m_toolconfigsNotifications);
    });

    updateData(DataKey::UiSettings, m_settings, m_valuesNotifications);
    updateData(DataKey::UiStates, m_states, m_statesNotifications);
    updateData(DataKey::UiToolConfigs, m_toolconfigs, m_toolconfigsNotifications);

#endif
}

#ifdef MU_BUILD_WORKSPACE_MODULE
void UiArrangement::updateData(DataKey key, QJsonObject& obj, Notifications& notifications) const
{
    RetVal<QByteArray> data = workspacesDataProvider()->rawData(key);
    if (!data.ret) {
        LOGD() << "no data: " << key_to_string(key);
        obj = QJsonObject();
        return;
    }

    QJsonObject oldObj = obj;
    obj = QJsonDocument::fromJson(data.val).object();

    Notifications::Iterator it = notifications.begin();
    for (; it != notifications.end(); ++it) {
        const QString& k = it.key();

        if (oldObj.value(k) != obj.value(k)) {
            it.value().notify();
        }
    }
}

void UiArrangement::saveData(workspace::DataKey key, const QJsonObject& obj)
{
    QByteArray data = QJsonDocument(obj).toJson();

    workspacesDataProvider()->setRawData(key, data);
}

#endif

QString UiArrangement::value(const QString& key) const
{
    QJsonValue val = m_settings.value(key);
    return val.toString();
}

void UiArrangement::setValue(const QString& key, const QString& val)
{
    m_settings[key] = val;
#ifdef MU_BUILD_WORKSPACE_MODULE
    saveData(DataKey::UiSettings, m_settings);
#endif
    if (m_valuesNotifications.contains(key)) {
        m_valuesNotifications[key].notify();
    }
}

mu::async::Notification UiArrangement::valueChanged(const QString& key) const
{
    return m_valuesNotifications[key];
}

QByteArray UiArrangement::state(const QString& key) const
{
    QJsonValue val = m_states.value(key);
    QString valStr = val.toString();
    return valStr.toLocal8Bit();
}

void UiArrangement::setState(const QString& key, const QByteArray& data)
{
    m_states[key] = QString::fromLocal8Bit(data);
#ifdef MU_BUILD_WORKSPACE_MODULE
    saveData(DataKey::UiStates, m_states);
#endif
    if (m_statesNotifications.contains(key)) {
        m_statesNotifications[key].notify();
    }
}

mu::async::Notification UiArrangement::stateChanged(const QString& key) const
{
    return m_statesNotifications[key];
}

ToolConfig UiArrangement::toolConfig(const QString& toolName) const
{
    TRACEFUNC;

    //! NOTE Maybe we need to cache?

    QJsonObject confObj = m_toolconfigs.value(toolName).toObject();
    QJsonArray itemsArr = confObj.value("items").toArray();

    ToolConfig config;

    config.items.reserve(itemsArr.size());
    for (const QJsonValue v : itemsArr) {
        QJsonObject itemObj = v.toObject();

        ToolConfig::Item item;
        item.action = itemObj.value("action").toString().toStdString();
        item.show = itemObj.value("show").toInt(1);

        config.items.push_back(std::move(item));
    }

    return config;
}

void UiArrangement::setToolConfig(const QString& toolName, const ToolConfig& config)
{
    TRACEFUNC;
    QJsonObject confObj;
    QJsonArray itemsArr;

    for (const ToolConfig::Item& item : config.items) {
        QJsonObject itemObj;
        itemObj["action"] = QString::fromStdString(item.action);
        itemObj["show"] = item.show ? 1 : 0;

        itemsArr.append(itemObj);
    }

    confObj["items"] = itemsArr;

    m_toolconfigs[toolName] = confObj;
#ifdef MU_BUILD_WORKSPACE_MODULE
    saveData(DataKey::UiToolConfigs, m_toolconfigs);
#endif
    if (m_toolconfigsNotifications.contains(toolName)) {
        m_toolconfigsNotifications[toolName].notify();
    }
}

mu::async::Notification UiArrangement::toolConfigChanged(const QString& toolName) const
{
    return m_toolconfigsNotifications[toolName];
}
