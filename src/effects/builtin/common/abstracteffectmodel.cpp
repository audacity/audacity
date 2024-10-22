#include "abstracteffectmodel.h"

#include "log.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

Effect* AbstractEffectModel::effect() const
{
    EffectInstanceId id = m_instanceId.toULongLong();
    IF_ASSERT_FAILED(id != 0) {
        return nullptr;
    }

    return effectInstancesRegister()->instanceById(id);
}

EffectSettings* AbstractEffectModel::settings() const
{
    EffectInstanceId id = m_instanceId.toULongLong();
    IF_ASSERT_FAILED(id != 0) {
        return nullptr;
    }

    return effectInstancesRegister()->settingsById(id);
}

QString AbstractEffectModel::instanceId() const
{
    return m_instanceId;
}

void AbstractEffectModel::setInstanceId(const QString& newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
