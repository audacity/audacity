/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsapi.h"

using namespace au::effects;

EffectsApi::EffectsApi(muse::api::IApiEngine* e)
    : muse::api::ApiObject(e)
{
}

bool EffectsApi::isApplying() const
{
    return effectExecutionScenario() ? effectExecutionScenario()->isBusy() : false;
}

bool EffectsApi::isAvailable(const QString& title) const
{
    if (!effectsProvider()) {
        return false;
    }
    for (const EffectMeta& meta : effectsProvider()->effectMetaList()) {
        if (meta.isLoadable() && meta.title.toQString() == title) {
            return true;
        }
    }
    return false;
}
