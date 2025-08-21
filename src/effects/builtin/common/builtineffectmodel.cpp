/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectmodel.h"

#include "../view/builtineffectviewloader.h"

#include "log.h"

using namespace au::effects;

BuiltinEffectModel::BuiltinEffectModel(QObject* parent)
    : QObject(parent), m_instanceId(BuiltinEffectViewLoader::initializationInstanceId())
{
    assert(m_instanceId != -1);
}

void BuiltinEffectModel::init()
{
    instancesRegister()->settingsChanged(m_instanceId).onNotify(this, [this]() {
        doReload();
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        doUpdateSettings();
    });

    doReload();
}

std::shared_ptr<au::effects::EffectInstance> BuiltinEffectModel::instance() const
{
    return instancesRegister()->instanceById(m_instanceId);
}

const EffectSettings& BuiltinEffectModel::settings() const
{
    const EffectSettings* s = instancesRegister()->settingsById(m_instanceId);
    IF_ASSERT_FAILED(s) {
        static EffectSettings null;
        return null;
    }

    return *s;
}

EffectSettingsAccessPtr BuiltinEffectModel::settingsAccess() const
{
    return instancesRegister()->settingsAccessById(m_instanceId);
}

EffectInstanceId BuiltinEffectModel::instanceId() const
{
    return m_instanceId;
}

void BuiltinEffectModel::preview()
{
    if (const EffectSettingsAccessPtr access = this->settingsAccess()) {
        access->ModifySettings([this](EffectSettings& settings) {
            executionScenario()->previewEffect(m_instanceId, settings);
            return nullptr;
        });
    }
}

void BuiltinEffectModel::modifySettings(const std::function<void(EffectSettings& settings)>& modifier)
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
