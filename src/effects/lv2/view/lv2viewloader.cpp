/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewloader.h"
#include "effects/effects_base/effectstypes.h"

#include <QQmlEngine>

#include "au3wrap/internal/wxtypes_convert.h"
#include "global/types/number.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "log.h"

namespace au::effects {
Lv2ViewLoader::Lv2ViewLoader(QObject* parent)
    : QObject(parent)
{}

Lv2ViewLoader::~Lv2ViewLoader()
{
    if (m_contentItem) {
        m_contentItem->deleteLater();
        m_contentItem = nullptr;
    }
}

void Lv2ViewLoader::load(const QString& /* instanceId */, QObject* /* itemParent */)
{
    LOGW() << "not implemented";
}

QQuickItem* Lv2ViewLoader::contentItem() const
{
    return m_contentItem;
}
}
