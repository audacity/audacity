/*
* Audacity: A Digital Audio Editor
*/
#include "effectsconfiguration.h"

#include "global/settings.h"

using namespace au::effects;

static const std::string moduleName("effects");

static const muse::Settings::Key PREVIEW_DURATION(moduleName, "effects/previewDuration");

void EffectsConfiguration::init()
{
    muse::settings()->setDefaultValue(PREVIEW_DURATION, muse::Val(6.0)); // sec
}

muse::secs_t EffectsConfiguration::previewDuration() const
{
    return muse::settings()->value(PREVIEW_DURATION).toDouble();
}
