/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectmodel.h"

#include "framework/global/log.h"

using namespace au::effects;

BuiltinEffectModel::BuiltinEffectModel(QObject* parent, int instanceId)
    : AbstractEffectViewModel(parent, instanceId), BuiltinEffectInstanceAccess(instanceId)
{
    assert(m_instanceId != -1);
}

void BuiltinEffectModel::doInit()
{
    instancesRegister()->settingsChanged(m_instanceId).onNotify(this, [this]() {
        doReload();
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        doUpdateSettings();
    });

    doReload();
}

void BuiltinEffectModel::doStartPreview()
{
    if (const EffectSettingsAccessPtr access = this->settingsAccess()) {
        access->ModifySettings([this](EffectSettings& settings) {
            executionScenario()->previewEffect(m_instanceId, settings);
            return nullptr;
        });
    }
}

void BuiltinEffectModel::doStopPreview()
{
    executionScenario()->stopPreview();
}

void BuiltinEffectModel::commitSettings()
{
    const EffectSettingsAccessPtr access = this->settingsAccess();
    IF_ASSERT_FAILED(access) {
        return;
    }
    access->Flush();
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

QString BuiltinEffectModel::effectId() const
{
    return instancesRegister()->effectIdByInstanceId(m_instanceId);
}
