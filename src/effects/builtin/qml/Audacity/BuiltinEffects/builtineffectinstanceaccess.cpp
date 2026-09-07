/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectinstanceaccess.h"

#include "framework/global/defer.h"

namespace au::effects {
BuiltinEffectInstanceAccess::BuiltinEffectInstanceAccess(EffectInstanceId instanceId)
    : m_accessInstanceId{instanceId}
{
}

std::shared_ptr<effects::EffectInstance> BuiltinEffectInstanceAccess::instance() const
{
    return instancesRegister()->instanceById(m_accessInstanceId);
}

const EffectSettings& BuiltinEffectInstanceAccess::settings() const
{
    const EffectSettings* s = instancesRegister()->settingsById(m_accessInstanceId);
    IF_ASSERT_FAILED(s) {
        static EffectSettings null;
        return null;
    }
    return *s;
}

EffectSettingsAccessPtr BuiltinEffectInstanceAccess::settingsAccess() const
{
    return instancesRegister()->settingsAccessById(m_accessInstanceId);
}
}
