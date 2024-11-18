/*
 * Audacity: A Digital Audio Editor
 */
#include "silenceviewmodel.h"
#include "silenceeffect.h"

#include "log.h"

using namespace au::effects;

SilenceEffect* SilenceViewModel::effect() const
{
    const auto instance = AbstractEffectModel::effect();
    IF_ASSERT_FAILED(instance) {
        return nullptr;
    }
    SilenceEffect* const e = dynamic_cast<SilenceEffect*>(instance);
    IF_ASSERT_FAILED(e) {
        return nullptr;
    }
    return e;
}
