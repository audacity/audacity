/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewmodel.h"

#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "log.h"

namespace au::effects {
Lv2ViewModel::Lv2ViewModel(QObject* parent)
    : QObject(parent)
{}

Lv2ViewModel::~Lv2ViewModel()
{
    if (m_contentItem) {
        m_contentItem->deleteLater();
        m_contentItem = nullptr;
    }
}

void Lv2ViewModel::load(const QString& instanceId, QObject* /* itemParent */)
{
    LOGW() << "not implemented";
}

QQuickItem* Lv2ViewModel::contentItem() const
{
    return m_contentItem;
}
}
