/*
 * Audacity: A Digital Audio Editor
 */
#include "repeatviewmodel.h"

#include "repeateffect.h"

#include "trackedit/itrackeditproject.h"

#include <algorithm>

using namespace au::effects;

RepeatViewModel::RepeatViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
    playbackConfiguration()->playbackTimeItemFormatChanged().onNotify(this, [this]() {
        emit timeFormatChanged();
    });
}

void RepeatViewModel::doReload()
{
    emit reloaded();
    emit repeatCountChanged();
}

int RepeatViewModel::repeatCount() const
{
    return effect<RepeatEffect>().repeatCount;
}

void RepeatViewModel::setRepeatCount(int newRepeatCount)
{
    auto& e = effect<RepeatEffect>();
    const int clamped = std::clamp(newRepeatCount, repeatCountMin(), repeatCountMax());
    if (e.repeatCount == clamped) {
        return;
    }
    e.repeatCount = clamped;
    emit repeatCountChanged();
}

int RepeatViewModel::repeatCountMin() const
{
    return RepeatEffect::Count.min;
}

int RepeatViewModel::repeatCountMax() const
{
    // Au3 limits it to INT_MAX / rate, which is way above this cap at any real sample rate
    return RepeatEffect::MaxRepeatCount;
}

double RepeatViewModel::selectionDuration() const
{
    const auto& e = effect<RepeatEffect>();
    return e.mT1 - e.mT0;
}

double RepeatViewModel::newSelectionDuration() const
{
    return selectionDuration() * (repeatCount() + 1);
}

double RepeatViewModel::sampleRate() const
{
    return effect<RepeatEffect>().mProjectRate;
}

double RepeatViewModel::tempo() const
{
    const auto project = globalContext()->currentTrackeditProject();
    return project ? project->timeSignature().tempo : 0.0;
}

int RepeatViewModel::upperTimeSignature() const
{
    const auto project = globalContext()->currentTrackeditProject();
    return project ? project->timeSignature().upper : 0;
}

int RepeatViewModel::lowerTimeSignature() const
{
    const auto project = globalContext()->currentTrackeditProject();
    return project ? project->timeSignature().lower : 0;
}

int RepeatViewModel::timeFormat() const
{
    return static_cast<int>(playbackConfiguration()->playbackTimeItemFormat());
}
