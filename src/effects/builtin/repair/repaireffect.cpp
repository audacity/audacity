/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.cpp

  Dominic Mazzoni

**********************************************************************/
#include "repaireffect.h"
#include "EffectOutputTracks.h"
#include "InterpolateAudio.h"
#include "TimeStretching.h"
#include "WaveTrack.h"
#include <cmath>

const ComponentInterfaceSymbol Repair::Symbol { XO("Repair") };

// ComponentInterface implementation

ComponentInterfaceSymbol Repair::GetSymbol() const
{
    return Symbol;
}

TranslatableString Repair::GetDescription() const
{
    return XO("Sets the peak amplitude of a one or more tracks");
}

// EffectDefinitionInterface implementation

EffectType Repair::GetType() const
{
    return EffectTypeProcess;
}

bool Repair::IsInteractive() const
{
    return false;
}

// Effect implementation

bool Repair::Process(EffectInstance&, EffectSettings&)
{
    // This may be too much copying for Repair. To support Cancel, may be
    // able to copy much less.
    // But for now, Cancel isn't supported without this.
    // Repair doesn't make sense for stretched clips, so don't pass a stretch
    // interval.
    EffectOutputTracks outputs { *mTracks, GetType(), {} };
    bool bGoodResult = true;

    int count = 0;
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        const double trackStart = track->GetStartTime();
        const double repair_t0 = std::max(mT0, trackStart);
        const double trackEnd = track->GetEndTime();
        const double repair_t1 = std::min(mT1, trackEnd);
        const double repair_deltat = repair_t1 - repair_t0;
        if (repair_deltat > 0) { // selection is within track audio
            const auto repair0 = track->TimeToLongSamples(repair_t0);
            const auto repair1 = track->TimeToLongSamples(repair_t1);
            const auto repairLen = repair1 - repair0;
            if (TimeStretching::HasPitchOrSpeed(*track, repair_t0, repair_t1)) {
                mLastError
                    =XO("The Repair effect cannot be applied within stretched or shrunk clips")
                      .Translation();
                bGoodResult = false;
                break;
            }
            if (repairLen > 128) {
                mLastError = XO(
                    "The Repair effect is intended to be used on very short sections of damaged audio (up to 128 samples).\n\nZoom in and select a tiny fraction of a second to repair.")
                             .Translation();
                bGoodResult = false;
                break;
            }

            const double rate = track->GetRate();
            const double spacing = std::max(repair_deltat * 2, 128. / rate);
            const double t0 = std::max(repair_t0 - spacing, trackStart);
            const double t1 = std::min(repair_t1 + spacing, trackEnd);

            const auto s0 = track->TimeToLongSamples(t0);
            const auto s1 = track->TimeToLongSamples(t1);
            // The difference is at most 2 * 128:
            const auto repairStart = (repair0 - s0).as_size_t();
            const auto len = s1 - s0;

            if (s0 == repair0 && s1 == repair1) {
                mLastError = XO(
                    "The Repair effect needs some data to go on.\n\nPlease select an area to repair with some audio on at least one side (the more the better).")
                             .Translation();
                bGoodResult = false;
                break;
            }

            for (const auto pChannel : track->Channels()) {
                if (!ProcessOne(
                        count++, *pChannel, s0,
                        // len is at most 5 * 128.
                        len.as_size_t(), repairStart,
                        // repairLen is at most 128.
                        repairLen.as_size_t())) {
                    bGoodResult = false;
                    goto done;
                }
            }
        }
    }
done:

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

bool Repair::ProcessOne(
    int count, WaveChannel& track, sampleCount start, size_t len,
    size_t repairStart, size_t repairLen)
{
    Floats buffer { len };
    track.GetFloats(buffer.get(), start, len);
    InterpolateAudio(buffer.get(), len, repairStart, repairLen);
    if (!track.SetFloats(
            &buffer[repairStart], start + repairStart, repairLen,
            // little repairs shouldn't force dither on rendering:
            narrowestSampleFormat)) {
        return false;
    }
    return !TrackProgress(count, 1.0); // TrackProgress returns true on Cancel.
}

bool Repair::NeedsDither() const
{
    return false;
}
