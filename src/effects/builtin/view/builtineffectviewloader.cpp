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
#include "builtineffectviewloader.h"
#include "effects/effects_base/effectstypes.h"

#include <QQmlEngine>

#include "au3wrap/internal/wxtypes_convert.h"
#include "global/types/number.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "log.h"

using namespace au::effects;

namespace {
static int gInitializationInstanceId = -1;
}

BuiltinEffectViewLoader::BuiltinEffectViewLoader(QObject* parent)
    : QObject(parent)
{}

BuiltinEffectViewLoader::~BuiltinEffectViewLoader()
{
    if (m_contentItem) {
        m_contentItem->deleteLater();
        m_contentItem = nullptr;
    }
}

void BuiltinEffectViewLoader::load(const QString& instanceId, QObject* itemParent, QObject* dialogView)
{
    LOGD() << "instanceId: " << instanceId;

    // TODO: could instancesRegister have a `typeByInstanceId` method?
    const auto effectId = instancesRegister()->effectIdByInstanceId(instanceId.toInt()).toStdString();

    const auto effect = dynamic_cast<Effect*>(EffectManager::Get().GetEffect(effectId));
    IF_ASSERT_FAILED(effect) {
        LOGE() << "effect not available, instanceId: " << instanceId << ", effectId: " << effectId;
        return;
    }

    const muse::String type = au3::wxToString(effect->GetSymbol().Internal());

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

    gInitializationInstanceId = instanceId.toInt();
    QObject* obj = component.createWithInitialProperties(
    {
        { "parent", QVariant::fromValue(itemParent) },
        { "instanceId", QVariant::fromValue(instanceId) },
        { "dialogView", QVariant::fromValue(dialogView) }
    });
    gInitializationInstanceId = -1;

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

int BuiltinEffectViewLoader::initializationInstanceId()
{
    return gInitializationInstanceId;
}

QQuickItem* BuiltinEffectViewLoader::contentItem() const
{
    return m_contentItem;
}
