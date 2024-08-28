#include "abstracteffectmodel.h"

#include "log.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

Effect* AbstractEffectModel::effect() const
{
    return effectsProvider()->effect(m_effectId);
}

QString AbstractEffectModel::effectId() const
{
    return m_effectId;
}

void AbstractEffectModel::setEffectId(const QString& newEffectId)
{
    if (m_effectId == newEffectId) {
        return;
    }
    m_effectId = newEffectId;
    emit effectIdChanged();
}
