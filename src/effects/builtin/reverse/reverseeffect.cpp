#include "reverseeffect.h"
#include "EffectOutputTracks.h"
#include "LabelTrack.h"
#include "SyncLock.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include <algorithm>
#include <cmath>

namespace au::effects {
ReverseEffect::ReverseEffect()
{
}

ReverseEffect::~ReverseEffect()
{
}

const ComponentInterfaceSymbol ReverseEffect::Symbol { XO("Reverse") };

ComponentInterfaceSymbol ReverseEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString ReverseEffect::GetDescription() const
{
    return XO("Reverses the selected audio");
}

// EffectDefinitionInterface implementation

EffectType ReverseEffect::GetType() const
{
    return EffectTypeProcess;
}

bool ReverseEffect::IsInteractive() const
{
    return false;
}

// Effect implementation

bool ReverseEffect::Process(EffectInstance&, EffectSettings&)
{
    // all needed because ReverseEffect should move the labels too
    EffectOutputTracks outputs {
        *mTracks, GetType(), { { mT0, mT1 } }, true, true
    };
    bool bGoodResult = true;
    int count = 0;

    auto trackRange
        =outputs.Get().Any() + &SyncLock::IsSelectedOrSyncLockSelectedP;
    trackRange.VisitWhile(
        bGoodResult,
        [&](WaveTrack& track) {
        const auto progress = [&](double fraction) {
            return !TrackProgress(count, fraction);
        };
        if (mT1 > mT0) {
            auto start = track.TimeToLongSamples(mT0);
            auto end = track.TimeToLongSamples(mT1);
            auto len = end - start;

            if (!WaveTrackUtilities::Reverse(track, start, len, progress)) {
                bGoodResult = false;
            }
        }
        count += track.NChannels();
    },
        [&](LabelTrack& track) {
        track.ChangeLabelsOnReverse(mT0, mT1);
        count++;
    });

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}
}
