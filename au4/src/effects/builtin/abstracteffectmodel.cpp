#include "abstracteffectmodel.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

QString AbstractEffectModel::instanceId() const
{
    return m_instanceId;
}

void AbstractEffectModel::setInstanceId(const QString &newInstanceId)
{
    if (m_instanceId == newInstanceId)
        return;
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
