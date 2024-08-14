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
#include "effectbuilder.h"

#include <QQmlEngine>

#include "global/types/number.h"

#include "log.h"

using namespace au::effects;

EffectBuilder::EffectBuilder(QObject* parent)
    : QObject(parent)
{}

void EffectBuilder::load(const QString& id, QObject* itemParent)
{
    EffectMeta meta = provider()->meta(id);
    if (!meta.isValid()) {
        LOGE() << "Not found manifest, id: " << id;
        return;
    }

    setTitle(meta.title);

    QQmlEngine* engin = engine()->qmlEngine();

    //! NOTE We create extension UI using a separate engine to control what we provide,
    //! making it easier to maintain backward compatibility and stability.
    QQmlComponent component = QQmlComponent(engin, meta.url.toQString());
    if (!component.isReady()) {
        LOGE() << "Failed to load QML file: " << meta.url;
        LOGE() << component.errorString();
        return;
    }

    QObject* obj = component.createWithInitialProperties({ { "parent", QVariant::fromValue(itemParent) } });

    m_contentItem = qobject_cast<QQuickItem*>(obj);
    if (!m_contentItem) {
        LOGE() << "Component not QuickItem, file: " << meta.url;
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

QQuickItem* EffectBuilder::contentItem() const
{
    return m_contentItem;
}

void EffectBuilder::setTitle(QString title)
{
    if (m_title == title) {
        return;
    }

    m_title = title;
    emit titleChanged();
}

QString EffectBuilder::title() const
{
    return m_title;
}
