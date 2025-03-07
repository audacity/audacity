/*
* Audacity: A Digital Audio Editor
*/
#include "vstviewmodel.h"

#include "global/async/async.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-vst3/VST3Instance.h"
#include "libraries/lib-vst3/VST3Wrapper.h"

#include "effects/effects_base/effectstypes.h"

#include "log.h"

using namespace au::effects;

VstViewModel::VstViewModel(QObject* parent)
    : QObject(parent)
{
    //! NOTE We don't show the progress itself, it's only used for cancellation.
    m_currentPreviewProgress = std::make_shared<muse::Progress>();
    m_currentPreviewProgress->finished().onReceive(this, [this](auto) {
        setIsPreviewing(false);
    });
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

    settingsToView();
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

void VstViewModel::togglePreview()
{
    if (m_isPreviewing) {
        m_currentPreviewProgress->cancel();
    } else {
        //! NOTE We set the preview immediately
        //! so that can't run the preview several times in a row.
        setIsPreviewing(true);

        muse::async::Async::call(this, [this](){
            IF_ASSERT_FAILED(m_settingsAccess) {
                return;
            }

            settingsFromView();

            m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
                executionScenario()->previewEffect(instanceId(), settings, m_currentPreviewProgress);
                return nullptr;
            });
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

bool VstViewModel::isPreviewing() const
{
    return m_isPreviewing;
}

void VstViewModel::setIsPreviewing(bool newIsPreviewing)
{
    if (m_isPreviewing == newIsPreviewing) {
        return;
    }
    m_isPreviewing = newIsPreviewing;
    emit isPreviewingChanged();
}
