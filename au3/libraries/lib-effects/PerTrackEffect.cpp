/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

*******************************************************************//**

\class PerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/

#include "PerTrackEffect.h"
#include "EffectOutputTracks.h"

#include "AudioGraphBuffers.h"
#include "AudioGraphTask.h"
#include "EffectStage.h"
#include "SyncLock.h"
#include "TimeWarper.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveTrackSink.h"
#include "WideSampleSource.h"

PerTrackEffect::Instance::~Instance() = default;

bool PerTrackEffect::Instance::Process(EffectSettings& settings)
{
    return mProcessor.Process(*this, settings);
}

bool PerTrackEffect::Instance::ProcessInitialize(EffectSettings&,
                                                 double, ChannelNames)
{
    return true;
}

bool PerTrackEffect::Instance::ProcessFinalize() noexcept
{
    return true;
}

PerTrackEffect::~PerTrackEffect() = default;

bool PerTrackEffect::DoPass1() const
{
    return true;
}

bool PerTrackEffect::DoPass2() const
{
    return false;
}

bool PerTrackEffect::Process(
    EffectInstance& instance, EffectSettings& settings) const
{
    auto pThis = const_cast<PerTrackEffect*>(this);

    // Destroy any pre-formed output tracks when done
    auto pOutputs = mpOutputTracks.get();

    std::optional<EffectOutputTracks> outputs;
    if (!pOutputs) {
        pOutputs = &outputs.emplace(*mTracks, GetType(),
                                    EffectOutputTracks::TimeInterval { mT0, mT1 }, true);
    }

    bool bGoodResult = true;
    // mPass = 1;
    if (DoPass1()) {
        auto& myInstance = dynamic_cast<Instance&>(instance);
        bGoodResult = pThis->ProcessPass(pOutputs->Get(), myInstance, settings);
        // mPass = 2;
        if (bGoodResult && DoPass2()) {
            bGoodResult = pThis->ProcessPass(pOutputs->Get(), myInstance, settings);
        }
    }
    if (bGoodResult) {
        pOutputs->Commit();
    }
    DestroyOutputTracks();
    return bGoodResult;
}

bool PerTrackEffect::ProcessPass(TrackList& outputs,
                                 Instance& instance, EffectSettings& settings)
{
    const auto duration = settings.extra.GetDuration();
    bool bGoodResult = true;
    bool isGenerator = GetType() == EffectTypeGenerate;
    bool isProcessor = GetType() == EffectTypeProcess;

    Buffers inBuffers, outBuffers;
    ChannelName map[3];
    size_t prevBufferSize = 0;
    int count = 0;
    bool clear = false;

    // It's possible that the number of channels the effect expects changed based on
    // the parameters (the Audacity Reverb effect does when the stereo width is 0).
    const auto numAudioIn = instance.GetAudioInCount();
    const auto numAudioOut = instance.GetAudioOutCount();
    if (numAudioOut < 1) {
        return false;
    }

    // Instances that can be reused in each loop pass
    std::vector<std::shared_ptr<EffectInstance> > recycledInstances{
        // First one is the given one; any others pushed onto here are
        // discarded when we exit
        std::dynamic_pointer_cast<EffectInstanceEx>(instance.shared_from_this())
    };

    const bool multichannel = numAudioIn > 1;
    int iChannel = 0;
    TrackListHolder results;
    const auto waveTrackVisitor
        =[&](WaveTrack& wt, WaveChannel& chan, bool isFirst) {
        if (isFirst) {
            iChannel = 0;
        }

        sampleCount len = 0;
        sampleCount start = 0;
        WaveChannel* pRight{};

        const int channel = (multichannel ? -1 : iChannel++);
        const auto numChannels = MakeChannelMap(wt.NChannels(), channel, map);
        if (multichannel) {
            assert(numAudioIn > 1);
            if (numChannels == 2) {
                // TODO: more-than-two-channels
                pRight = (*wt.Channels().rbegin()).get();
                clear = false;
            }
        }

        if (!isGenerator) {
            GetBounds(wt, &start, &len);
            mSampleCnt = len;
            if (len > 0 && numAudioIn < 1) {
                bGoodResult = false;
                return;
            }
        } else {
            mSampleCnt = wt.TimeToLongSamples(duration);
        }

        const auto sampleRate = wt.GetRate();

        // Get the block size the client wants to use
        auto max = wt.GetMaxBlockSize() * 2;
        const auto blockSize = instance.SetBlockSize(max);
        if (blockSize == 0) {
            bGoodResult = false;
            return;
        }

        // Calculate the buffer size to be at least the max rounded up to the clients
        // selected block size.
        const auto bufferSize
            =((max + (blockSize - 1)) / blockSize) * blockSize;
        if (bufferSize == 0) {
            bGoodResult = false;
            return;
        }

        // Always create the number of input buffers the client expects even
        // if we don't have
        // the same number of channels.
        // (These resizes may do nothing after the first track)

        if (len > 0) {
            assert(numAudioIn > 0); // checked above
        }
        inBuffers.Reinit(
            // TODO fix this hack for making Generator progress work without
            // assertion violations.  Make a dummy Source class that doesn't
            // care about the buffers.
            std::max(1u, numAudioIn),
            blockSize,
            std::max<size_t>(1, bufferSize / blockSize));
        if (len > 0) {
            // post of Reinit later satisfies pre of Source::Acquire()
            assert(inBuffers.Channels() > 0);
        }

        if (prevBufferSize != bufferSize) {
            // Buffer size has changed
            // We won't be using more than the first 2 buffers,
            // so clear the rest (if any)
            for (size_t i = 2; i < numAudioIn; i++) {
                inBuffers.ClearBuffer(i, bufferSize);
            }
        }
        prevBufferSize = bufferSize;

        // Always create the number of output buffers the client expects
        // even if we don't have the same number of channels.
        // (These resizes may do nothing after the first track)
        // Output buffers get an extra blockSize worth to give extra room if
        // the plugin adds latency -- PRL:  actually not important to do
        assert(numAudioOut > 0);  // checked above
        outBuffers.Reinit(numAudioOut, blockSize,
                          (bufferSize / blockSize) + 1);
        // post of Reinit satisfies pre of ProcessTrack
        assert(outBuffers.Channels() > 0);

        // (Re)Set the input buffer positions
        inBuffers.Rewind();

        // Clear unused input buffers
        if (!pRight && !clear && numAudioIn > 1) {
            inBuffers.ClearBuffer(1, bufferSize);
            clear = true;
        }

        const auto genLength = [this, &settings, &wt, isGenerator](
                               ) -> std::optional<sampleCount> {
            double genDur = 0;
            if (isGenerator) {
                const auto duration = settings.extra.GetDuration();
                if (IsPreviewing()) {
                    gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &genDur, 6.0);
                    genDur = std::min(duration, CalcPreviewInputLength(settings, genDur));
                } else {
                    genDur = duration;
                }
                // round to nearest sample
                return sampleCount{ (wt.GetRate() * genDur) + 0.5 };
            } else {
                return {};
            }
        }();

        const auto pollUser = [this, numChannels, count, start,
                               length = (genLength ? *genLength : len).as_double()
                              ](sampleCount inPos){
            if (numChannels > 1) {
                if (TrackGroupProgress(
                        count, (inPos - start).as_double() / length)
                    ) {
                    return false;
                }
            } else {
                if (TrackProgress(count, (inPos - start).as_double() / length)) {
                    return false;
                }
            }
            return true;
        };

        // Assured above
        assert(len == 0 || inBuffers.Channels() > 0);
        // TODO fix this hack to make the time remaining of the generator
        // progress dialog correct
        if (len == 0 && genLength) {
            len = *genLength;
        }
        WideSampleSequence* pSeq = &chan;
        if (pRight) {
            pSeq = &wt;
        }
        WideSampleSource source{
            *pSeq, size_t(pRight ? 2 : 1), start, len, pollUser };
        // Assert source is safe to Acquire inBuffers
        assert(source.AcceptsBuffers(inBuffers));
        assert(source.AcceptsBlockSize(inBuffers.BlockSize()));

        // Make "wide" or "narrow" copy of the track if generating
        // Old generator code may still proceed "interval-major" and later
        // join mono into stereo
        auto wideTrack
            =(pRight && isGenerator) ? wt.EmptyCopy() : nullptr;
        auto narrowTrack
            =(!pRight && isGenerator) ? wt.EmptyCopy(1) : nullptr;
        const auto pGenerated = wideTrack
                                ? wideTrack
                                : narrowTrack;

        WaveTrackSink sink{ chan, pRight, pGenerated.get(), start, isProcessor,
                            instance.NeedsDither() ? widestSampleFormat : narrowestSampleFormat
        };
        assert(sink.AcceptsBuffers(outBuffers));

        // Go process the track(s)
        const auto factory
            =[this, &recycledInstances, counter = 0]() mutable {
            auto index = counter++;
            if (index < recycledInstances.size()) {
                return recycledInstances[index];
            } else {
                return recycledInstances.emplace_back(MakeInstance());
            }
        };
        bGoodResult = ProcessTrack(channel, factory, settings, source, sink,
                                   genLength, sampleRate, wt, inBuffers, outBuffers);
        if (bGoodResult) {
            sink.Flush(outBuffers);
            bGoodResult = sink.IsOk();
            if (bGoodResult && pGenerated) {
                if (!results) {
                    results = TrackList::Temporary(nullptr, pGenerated);
                } else {
                    results->Add(pGenerated);
                    if (!multichannel && !isFirst && narrowTrack) {
                        // Generated a stereo track, in channel-major fashion.
                        // Get the last track but one -- generated in the previous
                        // pass
                        const auto pLast
                            =static_cast<WaveTrack*>(*std::next(results->rbegin()));
                        pLast->ZipClips();
                    }
                }
            }
        }
        if (!bGoodResult) {
            return;
        }
        ++count;
    };
    const auto defaultTrackVisitor
        =[&](Track& t) {
        if (SyncLock::IsSyncLockSelected(t)) {
            t.SyncLockAdjust(mT1, mT0 + duration);
        }
    };

    outputs.Any().VisitWhile(bGoodResult,
                             [&](auto&& fallthrough){
        return [&](WaveTrack& wt) {
            if (!wt.GetSelected()) {
                return fallthrough();
            }
            const auto channels = wt.Channels();
            if (multichannel) {
                waveTrackVisitor(wt, **channels.begin(), true);
            } else {
                bool first = true;
                for (const auto pChannel : channels) {
                    waveTrackVisitor(wt, *pChannel, first);
                    first = false;
                }
            }
            if (results) {
                const auto t1 = ViewInfo::Get(*FindProject()).selectedRegion.t1();
                PasteTimeWarper warper { t1,
                                         mT0 + (*results->begin())->GetEndTime() };
                wt.ClearAndPaste(mT0, t1,
                                 static_cast<WaveTrack&>(*results->DetachFirst()),
                                 true, true, &warper);
                results.reset();
            }
        };
    },
                             defaultTrackVisitor
                             );

    if (bGoodResult && GetType() == EffectTypeGenerate) {
        mT1 = mT0 + duration;
    }

    return bGoodResult;
}

bool PerTrackEffect::ProcessTrack(int channel, const Factory& factory,
                                  EffectSettings& settings,
                                  AudioGraph::Source& upstream, AudioGraph::Sink& sink,
                                  std::optional<sampleCount> genLength,
                                  const double sampleRate, const SampleTrack& wt,
                                  Buffers& inBuffers, Buffers& outBuffers)
{
    assert(upstream.AcceptsBuffers(inBuffers));
    assert(sink.AcceptsBuffers(outBuffers));

    const auto blockSize = inBuffers.BlockSize();
    assert(upstream.AcceptsBlockSize(blockSize));
    assert(blockSize == outBuffers.BlockSize());

    auto pSource = EffectStage::Create(
        channel, static_cast<const WideSampleSequence&>(wt).NChannels(), upstream,
        inBuffers, factory, settings, sampleRate, genLength);
    if (!pSource) {
        return false;
    }
    assert(pSource->AcceptsBlockSize(blockSize)); // post of ctor
    assert(pSource->AcceptsBuffers(outBuffers));

    AudioGraph::Task task{ *pSource, outBuffers, sink };
    return task.RunLoop();
}

std::shared_ptr<EffectOutputTracks> PerTrackEffect::MakeOutputTracks()
{
    return mpOutputTracks
               =std::make_shared<EffectOutputTracks>(*mTracks, GetType(),
                                                     EffectOutputTracks::TimeInterval { mT0, mT1 }, true);
}

void PerTrackEffect::DestroyOutputTracks() const
{
    mpOutputTracks.reset();
}
