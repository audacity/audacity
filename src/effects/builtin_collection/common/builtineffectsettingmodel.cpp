/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsettingmodel.h"

#include "framework/global/log.h"

namespace au::effects {
BuiltinEffectSettingModel::BuiltinEffectSettingModel(QObject* parent, int instanceId)
    : QObject{parent}, muse::Contextable(muse::iocCtxForQmlObject(this)), BuiltinEffectInstanceAccess(instanceId)
{
    assert(instanceId != -1);
}

void BuiltinEffectSettingModel::init()
{
    instancesRegister()->settingsChanged(m_accessInstanceId).onNotify(this, [this]() {
        emit valueChanged();
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        emit valueChanged();
    });

    emit valueChanged();
}

void BuiltinEffectSettingModel::commitSettings()
{
    const EffectSettingsAccessPtr access = this->settingsAccess();
    IF_ASSERT_FAILED(access) {
        return;
    }
    access->Flush();
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

QString BuiltinEffectSettingModel::paramId() const
{
    return m_paramId;
}

void BuiltinEffectSettingModel::setParamId(const QString& newParamId)
{
    if (m_paramId == newParamId) {
        return;
    }
    m_paramId = newParamId;
    emit paramIdChanged();
}
} // namespace au::effects
