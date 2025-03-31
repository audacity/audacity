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

void VstViewModel::init()
{
    EffectInstanceId id = this->instanceId();
    IF_ASSERT_FAILED(id != 0) {
        return;
    }

    std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(this->instanceId());
    m_auVst3Instance = std::dynamic_pointer_cast<VST3Instance>(instance);
    IF_ASSERT_FAILED(m_auVst3Instance) {
        return;
    }

    m_settingsAccess = instancesRegister()->settingsAccessById(id);
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    instancesRegister()->settingsChanged(id).onNotify(this, [this]() {
        settingsToView();
    });

    instancesRegister()->updateSettingsRequested(id).onNotify(this, [this]() {
        settingsFromView();
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        settingsToView();
    });

    m_auVst3Instance->GetWrapper().ParamChangedHandler = [this](Steinberg::Vst::ParamID) {
        projectHistory()->modifyState();
        projectHistory()->markUnsaved();
    };

    settingsToView();

    // When playback is idle, no need for setting updates to be low-latency. Every 100ms is plenty.
    startTimer(std::chrono::milliseconds { 100 });
}

void VstViewModel::settingsToView()
{
    IF_ASSERT_FAILED(m_auVst3Instance && m_settingsAccess) {
        return;
    }

    VST3Wrapper& w = m_auVst3Instance->GetWrapper();
    m_settingsAccess->ModifySettings([&w](EffectSettings& settings) {
        w.FetchSettings(settings);
        return nullptr;
    });
}

void VstViewModel::settingsFromView()
{
    IF_ASSERT_FAILED(m_auVst3Instance && m_settingsAccess) {
        return;
    }

    VST3Wrapper& w = m_auVst3Instance->GetWrapper();
    m_settingsAccess->ModifySettings([&w](EffectSettings& settings) {
        w.StoreSettings(settings);
        return nullptr;
    });
}

void VstViewModel::preview()
{
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    settingsFromView();

    m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
        executionScenario()->previewEffect(instanceId(), settings);
        return nullptr;
    });
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

bool VstViewModel::event(QEvent* event)
{
    if (event->type() == QEvent::Timer && !m_auVst3Instance->GetWrapper().IsActive()) {
        bool hasChanges { false };
        m_settingsAccess->ModifySettings([this, &hasChanges](EffectSettings& settings)
        {
            auto& wrapper = m_auVst3Instance->GetWrapper();
            wrapper.FlushParameters(settings, &hasChanges);
            if (hasChanges) {
                wrapper.StoreSettings(settings);
            }
            return nullptr;
        });
        if (hasChanges) {
            m_settingsAccess->Flush();
        }
    }
    return QObject::event(event);
}
