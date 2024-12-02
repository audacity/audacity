#include "abstracteffectmodel.h"

#include "libraries/lib-effects/Effect.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

void AbstractEffectModel::init()
{
    doInit();
    m_inited = true;
}

bool AbstractEffectModel::inited() const
{
    return m_inited;
}

Effect* AbstractEffectModel::effect() const
{
    EffectInstanceId id = this->instanceId();
    IF_ASSERT_FAILED(id != 0) {
        return nullptr;
    }

    return effectInstancesRegister()->instanceById(id);
}

EffectSettings* AbstractEffectModel::settings() const
{
    EffectInstanceId id = this->instanceId();
    IF_ASSERT_FAILED(id != 0) {
        return nullptr;
    }

    return effectInstancesRegister()->settingsById(id);
}

EffectInstanceId AbstractEffectModel::instanceId() const
{
    return m_instanceId.toULongLong();
}

void AbstractEffectModel::preview()
{
    effectExecutionScenario()->previewEffect(this->instanceId(), *this->settings());
}

QString AbstractEffectModel::instanceId_prop() const
{
    return m_instanceId;
}

void AbstractEffectModel::setInstanceId_prop(const QString& newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
    emit effectIdChanged();
}

QString AbstractEffectModel::effectId_prop() const
{
    EffectInstanceId id = this->instanceId();
    if (id == 0) {
        return QString();
    }

    return effectInstancesRegister()->effectIdByInstanceId(id);
}
