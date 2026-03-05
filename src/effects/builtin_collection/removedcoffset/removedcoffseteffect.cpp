/**********************************************************************

  Audacity: A Digital Audio Editor

  RemoveDCOffsetEffect.cpp

  Extracted from Normalize.cpp by drtootsie
  Original authors: Dominic Mazzoni, Vaughan Johnson

***********************************************************************/
#include "removedcoffseteffect.h"

#include "au3-effects/EffectOutputTracks.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-wave-track/WaveChannelUtilities.h"
#include "au3-wave-track/WaveTrack.h"
#include <cmath>

namespace au::effects {
const EffectParameterMethods& RemoveDCOffsetEffect::Parameters() const
{
    static CapturedParameters<RemoveDCOffsetEffect> parameters;
    return parameters;
}

const ComponentInterfaceSymbol RemoveDCOffsetEffect::Symbol { XO("RemoveDCOffset") };

RemoveDCOffsetEffect::RemoveDCOffsetEffect()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(false);
}

RemoveDCOffsetEffect::~RemoveDCOffsetEffect()
{
}

ComponentInterfaceSymbol RemoveDCOffsetEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString RemoveDCOffsetEffect::GetDescription() const
{
    return XO("Removes DC offset (centers audio on 0.0 vertically)");
}

ManualPageID RemoveDCOffsetEffect::ManualPage() const
{
    return L"Remove_DC_Offset";
}

::EffectType RemoveDCOffsetEffect::GetType() const
{
    return EffectTypeProcess;
}

bool RemoveDCOffsetEffect::Process(::EffectInstance&, ::EffectSettings&)
{
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };
    bool bGoodResult = true;
    double progress = 0;
    TranslatableString topMsg = XO("Removing DC offset...\n");

    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        double trackStart = track->GetStartTime();
        double trackEnd = track->GetEndTime();

        mCurT0 = std::max(trackStart, mT0);
        mCurT1 = std::min(trackEnd, mT1);

        if (mCurT1 > mCurT0) {
            wxString trackName = track->GetName();
            const auto channels = track->Channels();

            auto msg = topMsg + XO("Analyzing: %s").Format(trackName);

            const auto progressReport = [&](double fraction) {
                return !TotalProgress(
                    (progress + fraction / double(2 * GetNumWaveTracks())), msg);
            };

            for (auto channel : channels) {
                float offset = 0;
                bGoodResult = AnalyseTrackData(
                    *channel, progressReport, mCurT0, mCurT1, offset);
                if (!bGoodResult) {
                    goto break2;
                }
                progress += 1.0 / double(2 * GetNumWaveTracks());

                msg = topMsg + XO("Processing: %s").Format(trackName);
                if (false == (bGoodResult = ProcessOne(*channel, msg, progress, offset))) {
                    goto break2;
                }
            }
        }
    }

break2:

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

bool RemoveDCOffsetEffect::AnalyseTrackData(
    const WaveChannel& track, const ProgressReport& report, const double curT0,
    const double curT1, float& offset)
{
    bool rc = true;

    auto start = track.TimeToLongSamples(curT0);
    auto end = track.TimeToLongSamples(curT1);
    auto len = (end - start).as_double();

    Floats buffer { track.GetMaxBlockSize() };

    double sum = 0.0;
    sampleCount blockSamples;
    sampleCount totalSamples = 0;

    auto s = start;
    while (s < end)
    {
        const auto block
            =limitSampleBufferSize(track.GetBestBlockSize(s), end - s);

        track.GetFloats(
            buffer.get(), s, block, FillFormat::fillZero, true, &blockSamples);
        totalSamples += blockSamples;

        sum = AnalyseDataDC(buffer.get(), block, sum);

        s += block;

        if (!report((s - start).as_double() / len)) {
            rc = false;
            break;
        }
    }
    if (totalSamples > 0) {
        offset = -sum / totalSamples.as_double();
    } else {
        offset = 0.0;
    }

    return rc;
}

bool RemoveDCOffsetEffect::ProcessOne(
    WaveChannel& track, const TranslatableString& msg, double& progress,
    float offset)
{
    bool rc = true;

    auto start = track.TimeToLongSamples(mCurT0);
    auto end = track.TimeToLongSamples(mCurT1);
    auto len = (end - start).as_double();

    Floats buffer { track.GetMaxBlockSize() };

    auto s = start;
    while (s < end)
    {
        const auto block
            =limitSampleBufferSize(track.GetBestBlockSize(s), end - s);

        track.GetFloats(buffer.get(), s, block);

        ProcessData(buffer.get(), block, offset);

        if (!track.SetFloats(buffer.get(), s, block)) {
            rc = false;
            break;
        }

        s += block;

        if (TotalProgress(
                progress + ((s - start).as_double() / len)
                / double(2 * GetNumWaveTracks()),
                msg)) {
            rc = false;
            break;
        }
    }
    progress += 1.0 / double(2 * GetNumWaveTracks());

    return rc;
}

double RemoveDCOffsetEffect::AnalyseDataDC(float* buffer, size_t len, double sum)
{
    for (decltype(len) i = 0; i < len; i++) {
        sum += (double)buffer[i];
    }
    return sum;
}

void RemoveDCOffsetEffect::ProcessData(float* buffer, size_t len, float offset)
{
    for (decltype(len) i = 0; i < len; i++) {
        buffer[i] = buffer[i] + offset;
    }
}
}
