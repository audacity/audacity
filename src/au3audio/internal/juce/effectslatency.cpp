/*
* Audacity: A Digital Audio Editor
*/
#include "effectslatency.h"

#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

int au::au4juce::effectsLatency(const AudacityProject& project, const ChannelGroup* group, double sampleRate)
{
    const auto& list = group ? RealtimeEffectList::Get(*group) : RealtimeEffectList::Get(project);
    EffectInstance::SampleCount latency = 0;
    list.Visit([&](const RealtimeEffectState& state, bool listIsActive) {
        if (!listIsActive || !state.IsEnabled()) {
            return;
        }
        if (const auto instance = state.PeekInstance()) {
            latency += instance->GetLatency(state.GetSettings(), sampleRate);
        }
    });
    return static_cast<int>(latency);
}
