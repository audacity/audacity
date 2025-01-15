/*
* Audacity: A Digital Audio Editor
*/
#include "abstracteffectmodel.h"

#include "log.h"

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

void AbstractEffectModel::init()
{
    if (m_inited) {
        return;
    }

    EffectInstanceId id = this->instanceId();
    IF_ASSERT_FAILED(id != 0) {
        return;
    }

    instancesRegister()->settingsChanged(id).onNotify(this, [this]() {
        doReload();
    });

    doReload();
    m_inited = true;
}

bool AbstractEffectModel::inited() const
{
    return m_inited;
}

std::shared_ptr<au::effects::EffectInstance> AbstractEffectModel::instance() const
{
    EffectInstanceId id = this->instanceId();
    if (id == 0) {
        return nullptr;
    }

    return instancesRegister()->instanceById(id);
}

const EffectSettings* AbstractEffectModel::settings() const
{
    EffectInstanceId id = this->instanceId();
    if (id == 0) {
        return nullptr;
    }

    return instancesRegister()->settingsById(id);
}

EffectSettingsAccess* AbstractEffectModel::settingsAccess() const
{
    EffectInstanceId id = this->instanceId();
    if (id == 0) {
        return nullptr;
    }

    return instancesRegister()->settingsAccessById(id);
}

EffectInstanceId AbstractEffectModel::instanceId() const
{
    return m_instanceId.toULongLong();
}

EffectId AbstractEffectModel::effectId() const
{
    return instancesRegister()->effectIdByInstanceId(this->instanceId());
}

void AbstractEffectModel::preview()
{
    if (EffectSettingsAccess* access = this->settingsAccess()) {
        access->ModifySettings([this](EffectSettings& settings) {
            executionScenario()->previewEffect(instanceId(), settings);
            return nullptr;
        });
    }
}

void AbstractEffectModel::modifySettings(const std::function<void(EffectSettings& settings)>& modifier)
{
    EffectSettingsAccess* const access = this->settingsAccess();
    IF_ASSERT_FAILED(access) {
        return;
    }
    access->ModifySettings([&](EffectSettings& settings) {
        modifier(settings);
        return nullptr;
    });
    access->Flush();
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

    return instancesRegister()->effectIdByInstanceId(id);
}
