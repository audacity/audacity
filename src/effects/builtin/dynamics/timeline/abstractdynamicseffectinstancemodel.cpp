#include "abstractdynamicseffectinstancemodel.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"

#include "global/log.h"

namespace au::effects {
AbstractDynamicsEffectInstanceModel::AbstractDynamicsEffectInstanceModel(QObject* parent)
    : QObject{parent}
{
}

void AbstractDynamicsEffectInstanceModel::init()
{
    const auto instance = std::dynamic_pointer_cast<::CompressorInstance>(instancesRegister()->instanceById(m_instanceId));
    IF_ASSERT_FAILED(instance) {
        LOGW() << "Could not find instance for id " << m_instanceId;
        return;
    }
    m_instance = instance;

    doInit();
}

void AbstractDynamicsEffectInstanceModel::setInstanceId(int id)
{
    if (m_instanceId == id) {
        return;
    }
    m_instanceId = id;
    emit instanceIdChanged();
}
} // namespace au::effects
