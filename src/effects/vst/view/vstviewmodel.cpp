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

VstViewModel::~VstViewModel()
{
    m_settingUpdateTimer.stop();
    QObject::disconnect(&m_settingUpdateTimer, &QTimer::timeout, this, &VstViewModel::checkSettingChangesFromUiWhileIdle);
    checkSettingChangesFromUi(true);
}

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

    QObject::connect(&m_settingUpdateTimer, &QTimer::timeout, this, &VstViewModel::checkSettingChangesFromUiWhileIdle);

    // When playback is idle (see VstViewModel::event), no need for setting updates to be low-latency. Every 100ms is plenty.
    m_settingUpdateTimer.start(std::chrono::milliseconds { 100 });
}

void VstViewModel::checkSettingChangesFromUiWhileIdle()
{
    if (m_auVst3Instance->GetWrapper().IsActive()) {
        // While playback is active, setting updates are taken care of by AU3 backend.
        return;
    }
    checkSettingChangesFromUi(false);
}

void VstViewModel::checkSettingChangesFromUi(bool forceCommitting)
{
    bool hasChanges { false };
    m_settingsAccess->ModifySettings([&](EffectSettings& settings)
    {
        auto& wrapper = m_auVst3Instance->GetWrapper();
        wrapper.FlushParameters(settings, &hasChanges);
        if (hasChanges || forceCommitting) {
            wrapper.StoreSettings(settings);
        }
        return nullptr;
    });

    if (hasChanges || forceCommitting) {
        m_settingsAccess->Flush();
    }
}

void VstViewModel::settingsToView()
{
    IF_ASSERT_FAILED(m_auVst3Instance && m_settingsAccess) {
        return;
    }

    VST3Wrapper& w = m_auVst3Instance->GetWrapper();
    m_settingsAccess->ModifySettings([&w](EffectSettings& settings) {
        constexpr auto fallbackOnDefaults = true;
        w.FetchSettings(settings, fallbackOnDefaults);
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
