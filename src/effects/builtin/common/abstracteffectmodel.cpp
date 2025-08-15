/*
* Audacity: A Digital Audio Editor
*/
#include "abstracteffectmodel.h"

#include "../view/builtineffectviewloader.h"

#include "log.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent), m_instanceId(BuiltinEffectViewLoader::initializationInstanceId())
{
    assert(m_instanceId != -1);
}

void AbstractEffectModel::init()
{
    instancesRegister()->settingsChanged(m_instanceId).onNotify(this, [this]() {
        doReload();
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        doUpdateSettings();
    });

    doReload();
}

std::shared_ptr<au::effects::EffectInstance> AbstractEffectModel::instance() const
{
    return instancesRegister()->instanceById(m_instanceId);
}

const EffectSettings& AbstractEffectModel::settings() const
{
    const EffectSettings* s = instancesRegister()->settingsById(m_instanceId);
    IF_ASSERT_FAILED(s) {
        static EffectSettings null;
        return null;
    }

    return *s;
}

EffectSettingsAccessPtr AbstractEffectModel::settingsAccess() const
{
    return instancesRegister()->settingsAccessById(m_instanceId);
}

EffectInstanceId AbstractEffectModel::instanceId() const
{
    return m_instanceId;
}

void AbstractEffectModel::preview()
{
    if (const EffectSettingsAccessPtr access = this->settingsAccess()) {
        access->ModifySettings([this](EffectSettings& settings) {
            executionScenario()->previewEffect(m_instanceId, settings);
            return nullptr;
        });
    }
}

void AbstractEffectModel::modifySettings(const std::function<void(EffectSettings& settings)>& modifier)
{
    const EffectSettingsAccessPtr access = this->settingsAccess();
    IF_ASSERT_FAILED(access) {
        return;
    }
    access->ModifySettings([&](EffectSettings& settings) {
        modifier(settings);
        return nullptr;
    });
}

void AbstractEffectModel::commitSettings()
{
    const EffectSettingsAccessPtr access = this->settingsAccess();
    IF_ASSERT_FAILED(access) {
        return;
    }
    access->Flush();
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

QString AbstractEffectModel::effectId() const
{
    return instancesRegister()->effectIdByInstanceId(m_instanceId);
}
