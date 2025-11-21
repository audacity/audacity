/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

**********************************************************************/
#include "StereoToMono.h"
#include "BasicUI.h"
#include "EffectOutputTracks.h"
#include "Mix.h"
#include "MixAndRender.h"
#include "Project.h"
#include "RealtimeEffectList.h"
#include "WaveTrack.h"

const ComponentInterfaceSymbol StereoToMono::Symbol { XO("Stereo To Mono") };

ComponentInterfaceSymbol StereoToMono::GetSymbol() const
{
    return Symbol;
}

TranslatableString StereoToMono::GetDescription() const
{
    return XO("Converts stereo tracks to mono");
}

// EffectDefinitionInterface implementation

EffectType StereoToMono::GetType() const
{
    // Really EffectTypeProcess, but this prevents it from showing in the Effect
    // Menu
    return EffectTypeHidden;
}

bool StereoToMono::IsInteractive() const
{
    return false;
}

unsigned StereoToMono::GetAudioInCount() const
{
    return 2;
}

unsigned StereoToMono::GetAudioOutCount() const
{
    return 1;
}

// Effect implementation

bool StereoToMono::Process(EffectInstance&, EffectSettings&)
{
    // Do not use mWaveTracks here.  We will possibly DELETE tracks,
    // so we must use the "real" tracklist.
    EffectOutputTracks outputs {
        *mTracks,
        GetType(),
        // This effect ignores mT0 and mT1 but always mixes the entire tracks.
        { { mTracks->GetStartTime(), mTracks->GetEndTime() } }
    };
    bool bGoodResult = true;

    // Determine the total time (in samples) used by all of the target tracks
    // only for progress dialog
    sampleCount totalTime = 0;

    auto trackRange = outputs.Get().Selected<WaveTrack>();
    for (const auto left : trackRange) {
        if (left->Channels().size() > 1) {
            auto start = left->TimeToLongSamples(left->GetStartTime());
            auto end = left->TimeToLongSamples(left->GetEndTime());
            totalTime += (end - start);
        }
    }

    // Process each stereo track
    sampleCount curTime = 0;

    mProgress->SetMessage(XO("Mixing down to mono"));

    for (const auto track : trackRange) {
        if (track->Channels().size() > 1) {
            if (!ProcessOne(outputs.Get(), curTime, totalTime, *track)) {
                break;
            }
        }
    }

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

bool StereoToMono::ProcessOne(
    TrackList& outputs, sampleCount& curTime, sampleCount totalTime,
    WaveTrack& track)
{
    auto idealBlockLen = track.GetMaxBlockSize() * 2;
    bool bResult = true;
    sampleCount processed = 0;

    const auto start = track.GetStartTime();
    const auto end = track.GetEndTime();

    Mixer::Inputs tracks;
    tracks.emplace_back(
        track.SharedPointer<const SampleTrack>(), GetEffectStages(track));

    Mixer mixer(
        move(tracks), std::nullopt,
        true, // Throw to abort mix-and-render if read fails:
        Mixer::WarpOptions { inputTracks()->GetOwner() }, start, end, 1,
        idealBlockLen,
        false, // Not interleaved
        track.GetRate(), floatSample);

    // Always make mono output; don't use EmptyCopy
    auto outTrack = track.EmptyCopy(1);
    auto tempList = TrackList::Temporary(nullptr, outTrack);
    outTrack->ConvertToSampleFormat(floatSample);

    double denominator = track.GetChannelVolume(0) + track.GetChannelVolume(1);
    while (auto blockLen = mixer.Process())
    {
        auto buffer = mixer.GetBuffer();
        for (auto i = 0; i < blockLen; i++) {
            ((float*)buffer)[i] /= denominator;
        }

        // If mixing channels that both had only 16 bit effective format
        // (for example), and no gains or envelopes, still there should be
        // dithering because of the averaging above, which may introduce samples
        // lying between the quantization levels.  So use widestSampleFormat.
        outTrack->Append(0, buffer, floatSample, blockLen, 1, widestSampleFormat);

        curTime += blockLen;
        if (TotalProgress(curTime.as_double() / totalTime.as_double())) {
            return false;
        }
    }
    outTrack->Flush();

    track.Clear(start, end);
    track.MakeMono();
    track.Paste(start, *outTrack);
    RealtimeEffectList::Get(track).Clear();

    return bResult;
}

bool StereoToMono::IsHiddenFromMenus() const
{
    return true;
}
