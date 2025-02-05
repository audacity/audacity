/*
* Audacity: A Digital Audio Editor
*/
#include "vstviewmodel.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-vst3/VST3Instance.h"
#include "libraries/lib-vst3/VST3Wrapper.h"

#include "effects/effects_base/effectstypes.h"

#include "log.h"

using namespace au::effects;

EffectSettingsAccess* VstViewModel::settingsAccess() const
{
    EffectInstanceId id = this->instanceId();
    if (id == 0) {
        return nullptr;
    }

    return instancesRegister()->settingsAccessById(id);
}

void VstViewModel::updateSettings()
{
    std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(this->instanceId());
    std::shared_ptr<VST3Instance> auVst3Instance = std::dynamic_pointer_cast<VST3Instance>(instance);
    IF_ASSERT_FAILED(auVst3Instance) {
        return;
    }

    VST3Wrapper& w = auVst3Instance->GetWrapper();
    if (EffectSettingsAccess* access = this->settingsAccess()) {
        access->ModifySettings([&w](EffectSettings& settings) {
            w.StoreSettings(settings);
            return nullptr;
        });
    }
}

void VstViewModel::onApply()
{
    updateSettings();
}

void VstViewModel::preview()
{
    updateSettings();

    if (EffectSettingsAccess* access = this->settingsAccess()) {
        access->ModifySettings([this](EffectSettings& settings) {
            executionScenario()->previewEffect(instanceId(), settings);
            return nullptr;
        });
    }
}

int VstViewModel::instanceId() const
{
    return m_instanceId;
}

void VstViewModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
