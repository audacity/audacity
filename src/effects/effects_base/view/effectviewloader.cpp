/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2024 MuseScore BVBA and others
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
#include "effectviewloader.h"

#include <QQmlEngine>

#include "global/types/number.h"

#include "log.h"

using namespace au::effects;

EffectViewLoader::EffectViewLoader(QObject* parent)
    : QObject(parent)
{}

void EffectViewLoader::load(const QString& type, const QString& instanceId, const QString& effectState, QObject* itemParent)
{
    LOGD() << "type: " << type << ", instanceId: " << instanceId;

    QString url = viewRegister()->viewUrl(type);
    if (url.isEmpty()) {
        LOGE() << "Not found view for type: " << type;
        return;
    }
    LOGD() << "found view for type: " << type << ", url: " << url;

    QQmlEngine* qmlEngine = engine()->qmlEngine();

    //! NOTE We create extension UI using a separate engine to control what we provide,
    //! making it easier to maintain backward compatibility and stability.
    QQmlComponent component = QQmlComponent(qmlEngine, url);
    if (!component.isReady()) {
        LOGE() << "Failed to load QML file: " << url;
        LOGE() << component.errorString();
        return;
    }

    QObject* obj = component.createWithInitialProperties(
    {
        { "parent", QVariant::fromValue(itemParent) },
        { "instanceId", QVariant::fromValue(instanceId) },
        { "effectState", QVariant::fromValue(effectState) }
    });

    m_contentItem = qobject_cast<QQuickItem*>(obj);
    if (!m_contentItem) {
        LOGE() << "Component not QuickItem, file: " << url;
    }

    if (m_contentItem) {
        if (muse::is_zero(m_contentItem->implicitHeight())) {
            m_contentItem->setImplicitHeight(m_contentItem->height());
            if (muse::is_zero(m_contentItem->implicitHeight())) {
                m_contentItem->setImplicitHeight(450);
            }
        }

        if (muse::is_zero(m_contentItem->implicitWidth())) {
            m_contentItem->setImplicitWidth(m_contentItem->width());
            if (muse::is_zero(m_contentItem->implicitWidth())) {
                m_contentItem->setImplicitWidth(200);
            }
        }
    }

    emit contentItemChanged();
}

QQuickItem* EffectViewLoader::contentItem() const
{
    return m_contentItem;
}
