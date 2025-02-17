/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilenceBase.cpp

  Lynn Allan (from DM's Normalize)
  Philip Van Baren (more options and boundary fixes)

*******************************************************************//**

\class TruncSilenceBase
\brief Truncate Silence automatically reduces the length of passages
       where the volume is below a set threshold level.

*//*******************************************************************/
#include "TruncSilenceBase.h"
#include "BasicUI.h"
#include "EffectOutputTracks.h"
#include "Prefs.h"
#include "Project.h"
#include "SyncLock.h"
#include "WaveTrack.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <list>

class Enums
{
public:
    static const size_t NumDbChoices;
    static const EnumValueSymbol DbChoices[];
};

const EnumValueSymbol Enums::DbChoices[] = {
    // Table of text values, only for reading what was stored in legacy config
    // files.
    // It was inappropriate to make this a discrete choice control.
    { wxT("-20 dB") }, { wxT("-25 dB") }, { wxT("-30 dB") }, { wxT("-35 dB") },
    { wxT("-40 dB") }, { wxT("-45 dB") }, { wxT("-50 dB") }, { wxT("-55 dB") },
    { wxT("-60 dB") }, { wxT("-65 dB") }, { wxT("-70 dB") }, { wxT("-75 dB") },
    { wxT("-80 dB") }
};

// Map from position in table above to numerical value.
static inline double enumToDB(int val)
{
    return -(5.0 * val + 20.0);
}

const size_t Enums::NumDbChoices = WXSIZEOF(Enums::DbChoices);

using Region = WaveTrack::Region;

// Declaration of RegionList
class RegionList : public std::list<Region>
{
};

const EnumValueSymbol TruncSilenceBase::kActionStrings[nActions] = {
    { XO("Truncate Detected Silence") }, { XO("Compress Excess Silence") }
};

static CommandParameters::ObsoleteMap kObsoleteActions[] = {
    // Compatible with 2.1.0 and before
    { wxT("0"), 0 }, // Remap to Truncate Detected Silence
    { wxT("1"), 1 }, // Remap to Compress Excess Silence
};

static const size_t nObsoleteActions = WXSIZEOF(kObsoleteActions);

const EffectParameterMethods& TruncSilenceBase::Parameters() const
{
    static CapturedParameters<
        TruncSilenceBase, Threshold, ActIndex, Minimum, Truncate, Compress,
        Independent>
    parameters;
    return parameters;
}

static const size_t DEF_BlendFrameCount = 100;

// Lower bound on the amount of silence to find at a time -- this avoids
// detecting silence repeatedly in low-frequency sounds.
static const double DEF_MinTruncMs = 0.001;

// Typical fraction of total time taken by detection (better to guess low)
const double detectFrac = 0.4;

const ComponentInterfaceSymbol TruncSilenceBase::Symbol { XO(
                                                              "Truncate Silence") };

TruncSilenceBase::TruncSilenceBase()
{
    Parameters().Reset(*this);

    SetLinearEffectFlag(false);

    // This used to be changeable via the audacity.cfg/registry.  Doubtful that
    // was ever done.
    //
    // Original comment:
    //
    //   mBlendFrameCount only retrieved from prefs ... not using dialog
    //   Only way to change (for windows) is thru registry
    //   The values should be figured dynamically ... too many frames could be
    //   invalid
    mBlendFrameCount = DEF_BlendFrameCount;
}

TruncSilenceBase::~TruncSilenceBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol TruncSilenceBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString TruncSilenceBase::GetDescription() const
{
    return XO(
        "Automatically reduces the length of passages where the volume is below a specified level");
}

ManualPageID TruncSilenceBase::ManualPage() const
{
    return L"Truncate_Silence";
}

// EffectDefinitionInterface implementation

EffectType TruncSilenceBase::GetType() const
{
    return EffectTypeProcess;
}

bool TruncSilenceBase::LoadSettings(
    const CommandParameters& parms, EffectSettings& settings) const
{
    Effect::LoadSettings(parms, settings);

    // A bit of special treatment for two parameters

    // This control migrated from a choice to a text box in version 2.3.0
    double myThreshold {};
    bool newParams = [&] {
        double temp;
        if (!parms.ReadAndVerify(
                Threshold.key, &temp, Threshold.def, Threshold.min, Threshold.max)) {
            return false;
        }
        myThreshold = temp;
        return true;
    }();

    if (!newParams) {
        int temp;
        // Use legacy param:
        if (!parms.ReadAndVerify(
                L"Db", &temp, 0, Enums::DbChoices, Enums::NumDbChoices)) {
            return false;
        }
        myThreshold = enumToDB(temp);
    }

    {
        int temp;
        if (!parms.ReadAndVerify(
                ActIndex.key, &temp, ActIndex.def, kActionStrings, nActions,
                kObsoleteActions, nObsoleteActions)) {
            return false;
        }

        // TODO:  fix this when settings are really externalized
        const_cast<int&>(mActionIndex) = temp;
    }
    // TODO:  fix this when settings are really externalized
    const_cast<double&>(mThresholdDB) = myThreshold;
    return true;
}

// Effect implementation

double TruncSilenceBase::CalcPreviewInputLength(
    const EffectSettings&, double /* previewLength */) const
{
    double inputLength = mT1 - mT0;
    double minInputLength = inputLength;

    // Master list of silent regions
    RegionList silences;

    // Start with the whole selection silent
    silences.push_back(Region(mT0, mT1));

    int whichTrack = 0;

    for (auto wt : inputTracks()->Selected<const WaveTrack>()) {
        RegionList trackSilences;

        auto index = wt->TimeToLongSamples(mT0);
        sampleCount silentFrame = 0; // length of the current silence

        Analyze(
            silences, trackSilences, *wt, &silentFrame, &index, whichTrack,
            &inputLength, &minInputLength);

        whichTrack += wt->NChannels();
    }
    return inputLength;
}

bool TruncSilenceBase::Process(EffectInstance&, EffectSettings&)
{
    const bool success = mbIndependent ? ProcessIndependently() : ProcessAll();

    return success;
}

bool TruncSilenceBase::ProcessIndependently()
{
    unsigned nGroups = 0;

    const bool syncLock = SyncLockState::Get(*FindProject()).IsSyncLocked();

    // Check if it's permissible
    {
        for (auto track : inputTracks()->Selected<const WaveTrack>()) {
            if (syncLock) {
                auto otherTracks
                    =SyncLock::Group(*track).Filter<const WaveTrack>()
                      + &Track::IsSelected
                      -[&](const Track * pTrack) {
                    return pTrack == track;
                    };
                if (otherTracks) {
                    BasicUI::ShowMessageBox(XO(
                                                "When truncating independently, there may only be one selected audio track in each Sync-Locked Track Group."));
                    return false;
                }
            }

            ++nGroups;
        }
    }

    if (nGroups == 0) {
        // nothing to do
        return true;
    }

    // Now do the work

    // Copy tracks
    EffectOutputTracks outputs {
        *mTracks, GetType(), { { mT0, mT1 } }, true, true
    };
    double newT1 = 0.0;

    {
        unsigned iGroup = 0;
        for (auto track : outputs.Get().Selected<WaveTrack>()) {
            RegionList silences;
            if (!FindSilences(
                    silences, TrackList::SingletonRange(&as_const(*track)))) {
                return false;
            }
            // Treat tracks in the sync lock group only
            Track* groupFirst, * groupLast;
            auto range = syncLock ? SyncLock::Group(*track)
                         : TrackList::SingletonRange<Track>(track);
            double totalCutLen = 0.0;
            if (!DoRemoval(silences, range, iGroup, nGroups, totalCutLen)) {
                return false;
            }
            newT1 = std::max(newT1, mT1 - totalCutLen);

            ++iGroup;
        }
    }

    mT1 = newT1;

    outputs.Commit();

    return true;
}

bool TruncSilenceBase::ProcessAll()
{
    // Copy tracks
    EffectOutputTracks outputs {
        *mTracks, GetType(), { { mT0, mT1 } }, true, true
    };

    // Master list of silent regions.
    // This list should always be kept in order.
    RegionList silences;

    if (FindSilences(silences, outputs.Get().Selected<const WaveTrack>())) {
        auto trackRange = outputs.Get().Any();
        double totalCutLen = 0.0;
        if (DoRemoval(silences, trackRange, 0, 1, totalCutLen)) {
            mT1 -= totalCutLen;
            outputs.Commit();
            return true;
        }
    }

    return false;
}

bool TruncSilenceBase::FindSilences(
    RegionList& silences, const TrackIterRange<const WaveTrack>& range)
{
    // Start with the whole selection silent
    silences.push_back(Region(mT0, mT1));

    // Remove non-silent regions in each track
    int whichTrack = 0;
    for (auto wt : range) {
        // Smallest silent region to detect in frames
        auto minSilenceFrames = sampleCount(
            std::max(mInitialAllowedSilence, DEF_MinTruncMs) * wt->GetRate());

        //
        // Scan the track for silences
        //
        RegionList trackSilences;

        auto index = wt->TimeToLongSamples(mT0);
        sampleCount silentFrame = 0;

        // Detect silences
        bool cancelled = !(Analyze(
                               silences, trackSilences, *wt, &silentFrame, &index, whichTrack));

        // Buffer has been freed, so we're OK to return if cancelled
        if (cancelled) {
            return false;
        }

        if (silentFrame >= minSilenceFrames) {
            // Track ended in silence -- record region
            trackSilences.push_back(Region(
                                        wt->LongSamplesToTime(index - silentFrame),
                                        wt->LongSamplesToTime(index)));
        }

        // Intersect with the overall silent region list
        Intersect(silences, trackSilences);
        whichTrack++;
    }

    return true;
}

bool TruncSilenceBase::DoRemoval(
    const RegionList& silences, const TrackIterRange<Track>& range,
    unsigned iGroup, unsigned nGroups, double& totalCutLen)
{
    //
    // Now remove the silent regions from all selected / sync-lock selected
    // tracks.
    //

    // Loop over detected regions in reverse (so cuts don't change time values
    // down the line)
    int whichReg = 0;
    RegionList::const_reverse_iterator rit;
    for (rit = silences.rbegin(); rit != silences.rend(); ++rit) {
        const Region& region = *rit;
        const Region* const r = &region;

        // Progress dialog and cancellation. Do additional cleanup before return.
        const double frac
            =detectFrac + (1 - detectFrac)
              * (iGroup + whichReg / double(silences.size()))
              / nGroups;
        if (TotalProgress(frac)) {
            return false;
        }

        // Intersection may create regions smaller than allowed; ignore them.
        // Allow one nanosecond extra for consistent results with exact
        // milliseconds of allowed silence.
        if ((r->end - r->start) < (mInitialAllowedSilence - 0.000000001)) {
            continue;
        }

        // Find NEW silence length as requested
        double inLength = r->end - r->start;
        double outLength;

        switch (mActionIndex) {
        case kTruncate:
            outLength = std::min(mTruncLongestAllowedSilence, inLength);
            break;
        case kCompress:
            outLength
                =mInitialAllowedSilence + (inLength - mInitialAllowedSilence)
                  * mSilenceCompressPercent / 100.0;
            break;
        default: // Not currently used.
            outLength = std::min(
                mInitialAllowedSilence + (inLength - mInitialAllowedSilence)
                * mSilenceCompressPercent / 100.0,
                mTruncLongestAllowedSilence);
        }

        const double cutLen = std::max(0.0, inLength - outLength);
        // Don't waste time cutting nothing.
        if (cutLen == 0.0) {
            continue;
        }

        totalCutLen += cutLen;

        bool success = true;
        double cutStart = (r->start + r->end - cutLen) / 2;
        double cutEnd = cutStart + cutLen;
        (range + &SyncLock::IsSelectedOrSyncLockSelectedP
         -[&](const Track * pTrack) {
            return
            // Don't waste time past the end of a track
            pTrack->GetEndTime() < r->start;
        })
        .VisitWhile(
            success,
            [&](WaveTrack& wt) {
            // In WaveTracks, clear with a cross-fade
            auto blendFrames = mBlendFrameCount;
            // Round start/end times to frame boundaries
            cutStart = wt.SnapToSample(cutStart);
            cutEnd = wt.SnapToSample(cutEnd);

            // Make sure the cross-fade does not affect non-silent frames
            if (wt.LongSamplesToTime(blendFrames) > inLength) {
                // Result is not more than blendFrames:
                blendFrames = wt.TimeToLongSamples(inLength).as_size_t();
            }

            // Perform cross-fade in memory
            struct Buffers
            {
                Buffers(size_t size) : buf1 { size },
                buf2 { size }
                {
                }
                Floats buf1, buf2;
            };
            Buffers buffers[2] { blendFrames, blendFrames };
            auto t1 = wt.TimeToLongSamples(cutStart) - blendFrames / 2;
            auto t2 = wt.TimeToLongSamples(cutEnd) - blendFrames / 2;

            size_t iChannel = 0;
            for (const auto pChannel : wt.Channels()) {
                auto& buffer = buffers[iChannel];
                pChannel->GetFloats(buffer.buf1.get(), t1, blendFrames);
                pChannel->GetFloats(buffer.buf2.get(), t2, blendFrames);

                for (decltype(blendFrames) i = 0; i < blendFrames; ++i) {
                    buffer.buf1[i] = ((blendFrames - i) * buffer.buf1[i]
                                      + i * buffer.buf2[i])
                                     / (double)blendFrames;
                }
                ++iChannel;
            }

            wt.Clear(cutStart, cutEnd);

            iChannel = 0;
            for (const auto pChannel : wt.Channels()) {
                // Write cross-faded data
                auto& buffer = buffers[iChannel];
                success
                    =success
                      && pChannel->SetFloats(
                          buffer.buf1.get(), t1, blendFrames,
                          // This effect mostly shifts samples to remove silences,
                          // and does only a little bit of floating point
                          // calculations to cross-fade the splices, over a 100
                          // sample interval by default. Don't dither.
                          narrowestSampleFormat);
                ++iChannel;
            }
        },
            [&](Track& t) {
            // Non-wave tracks: just do a sync-lock adjust
            t.SyncLockAdjust(cutEnd, cutStart);
        });
        if (!success) {
            return false;
        }
        ++whichReg;
    }

    return true;
}

bool TruncSilenceBase::Analyze(
    RegionList& silenceList, RegionList& trackSilences, const WaveTrack& wt,
    sampleCount* silentFrame, sampleCount* index, int whichTrack,
    double* inputLength, double* minInputLength) const
{
    const auto rate = wt.GetRate();

    // Smallest silent region to detect in frames
    auto minSilenceFrames
        =sampleCount(std::max(mInitialAllowedSilence, DEF_MinTruncMs) * rate);

    double truncDbSilenceThreshold = DB_TO_LINEAR(mThresholdDB);
    auto blockLen = wt.GetMaxBlockSize();
    auto start = wt.TimeToLongSamples(mT0);
    auto end = wt.TimeToLongSamples(mT1);
    sampleCount outLength = 0;

    double previewLength;
    gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLength, 6.0);
    // Minimum required length in samples.
    const sampleCount previewLen(previewLength * rate);

    // Keep position in overall silences list for optimization
    RegionList::iterator rit(silenceList.begin());

    // Allocate buffers
    Floats buffers[] { Floats { blockLen }, Floats { blockLen } };

    // Loop through current track
    while (*index < end)
    {
        if (
            inputLength
            && ((outLength >= previewLen)
                || (*index - start > wt.TimeToLongSamples(*minInputLength)))) {
            *inputLength = std::min<double>(*inputLength, *minInputLength);
            if (outLength >= previewLen) {
                *minInputLength = *inputLength;
            }
            return true;
        }

        if (!inputLength) {
            // Show progress dialog, test for cancellation
            bool cancelled = TotalProgress(
                detectFrac
                * (whichTrack
                   + (*index - start).as_double() / (end - start).as_double())
                / (double)GetNumWaveTracks());
            if (cancelled) {
                return false;
            }
        }

        // Optimization: if not in a silent region skip ahead to the next one

        double curTime = wt.LongSamplesToTime(*index);
        for (; rit != silenceList.end(); ++rit) {
            // Find the first silent region ending after current time
            if (rit->end >= curTime) {
                break;
            }
        }

        if (rit == silenceList.end()) {
            // No more regions -- no need to process the rest of the track
            if (inputLength) {
                // Add available samples up to previewLength.
                auto remainingTrackSamples
                    =wt.TimeToLongSamples(wt.GetEndTime()) - *index;
                auto requiredTrackSamples = previewLen - outLength;
                outLength += (remainingTrackSamples > requiredTrackSamples)
                             ? requiredTrackSamples
                             : remainingTrackSamples;
            }

            break;
        } else if (rit->start > curTime) {
            // End current silent region, skip ahead
            if (*silentFrame >= minSilenceFrames) {
                trackSilences.push_back(Region(
                                            wt.LongSamplesToTime(*index - *silentFrame),
                                            wt.LongSamplesToTime(*index)));
            }
            *silentFrame = 0;
            auto newIndex = wt.TimeToLongSamples(rit->start);
            if (inputLength) {
                auto requiredTrackSamples = previewLen - outLength;
                // Add non-silent sample to outLength
                outLength += ((newIndex - *index) > requiredTrackSamples)
                             ? requiredTrackSamples
                             : newIndex - *index;
            }

            *index = newIndex;
        }
        // End of optimization

        // Limit size of current block if we've reached the end
        auto count = limitSampleBufferSize(blockLen, end - *index);

        // Fill buffers
        size_t iChannel = 0;
        for (const auto pChannel : wt.Channels()) {
            pChannel->GetFloats(buffers[iChannel++].get(), *index, count);
        }

        // Look for silenceList in current block
        for (decltype(count) i = 0; i < count; ++i) {
            if (
                inputLength
                && ((outLength >= previewLen)
                    || (outLength > wt.TimeToLongSamples(*minInputLength)))) {
                *inputLength
                    =wt.LongSamplesToTime(*index + i) - wt.LongSamplesToTime(start);
                break;
            }

            const bool silent
                =std::all_of(buffers, buffers + iChannel, [&](const Floats& buffer) {
                return fabs(buffer[i]) < truncDbSilenceThreshold;
            });
            if (silent) {
                (*silentFrame)++;
            } else {
                sampleCount allowed = 0;
                if (*silentFrame >= minSilenceFrames) {
                    if (inputLength) {
                        switch (mActionIndex) {
                        case kTruncate:
                            outLength
                                +=wt.TimeToLongSamples(mTruncLongestAllowedSilence);
                            break;
                        case kCompress:
                            allowed = wt.TimeToLongSamples(mInitialAllowedSilence);
                            outLength += sampleCount(
                                allowed.as_double()
                                + (*silentFrame - allowed).as_double()
                                * mSilenceCompressPercent / 100.0);
                            break;
                            // default: // Not currently used.
                        }
                    }

                    // Record the silent region
                    trackSilences.push_back(Region(
                                                wt.LongSamplesToTime(*index + i - *silentFrame),
                                                wt.LongSamplesToTime(*index + i)));
                } else if (inputLength) { // included as part of non-silence
                    outLength += *silentFrame;
                }
                *silentFrame = 0;
                if (inputLength) {
                    ++outLength; // Add non-silent sample to outLength
                }
            }
        }
        // Next block
        *index += count;
    }

    if (inputLength) {
        *inputLength = std::min<double>(*inputLength, *minInputLength);
        if (outLength >= previewLen) {
            *minInputLength = *inputLength;
        }
    }

    return true;
}

// EffectTruncSilence implementation

// Finds the intersection of the ordered region lists, stores in dest
void TruncSilenceBase::Intersect(RegionList& dest, const RegionList& src)
{
    RegionList::iterator destIter;
    destIter = dest.begin();
    // Any time we reach the end of the dest list we're finished
    if (destIter == dest.end()) {
        return;
    }
    RegionList::iterator curDest = destIter;

    // Operation: find non-silent regions in src, remove them from dest.
    double nsStart = curDest->start;
    double nsEnd;
    bool lastRun = false; // must run the loop one extra time

    RegionList::const_iterator srcIter = src.begin();

    // This logic, causing the loop to run once after end of src, must occur
    // each time srcIter is updated
    if (srcIter == src.end()) {
        lastRun = true;
    }

    while (srcIter != src.end() || lastRun)
    {
        // Don't use curSrc unless lastRun is false!
        RegionList::const_iterator curSrc;

        if (lastRun) {
            // The last non-silent region extends as far as possible
            nsEnd = std::numeric_limits<double>::max();
        } else {
            curSrc = srcIter;
            nsEnd = curSrc->start;
        }

        if (nsEnd > nsStart) {
            // Increment through dest until we have a region that could be affected
            while (curDest->end <= nsStart)
            {
                ++destIter;
                if (destIter == dest.end()) {
                    return;
                }
                curDest = destIter;
            }

            // Check for splitting dest region in two
            if (nsStart > curDest->start && nsEnd < curDest->end) {
                // The second region
                Region r(nsEnd, curDest->end);

                // The first region
                curDest->end = nsStart;

                // Insert second region after first
                RegionList::iterator nextIt(destIter);
                ++nextIt;

                // This should just read: destIter = dest.insert(nextIt, r); but we
                // work around two two wxList::insert() bugs. First, in some
                // versions it returns the wrong value. Second, in some versions,
                // it crashes when you insert at list end.
                if (nextIt == dest.end()) {
                    dest.push_back(r);
                } else {
                    dest.insert(nextIt, r);
                }
                ++destIter; // (now points at the newly-inserted region)

                curDest = destIter;
            }

            // Check for truncating the end of dest region
            if (
                nsStart > curDest->start && nsStart < curDest->end
                && nsEnd >= curDest->end) {
                curDest->end = nsStart;

                ++destIter;
                if (destIter == dest.end()) {
                    return;
                }
                curDest = destIter;
            }

            // Check for all dest regions that need to be removed completely
            while (nsStart <= curDest->start && nsEnd >= curDest->end)
            {
                destIter = dest.erase(destIter);
                if (destIter == dest.end()) {
                    return;
                }
                curDest = destIter;
            }

            // Check for truncating the beginning of dest region
            if (
                nsStart <= curDest->start && nsEnd > curDest->start
                && nsEnd < curDest->end) {
                curDest->start = nsEnd;
            }
        }

        if (lastRun) {
            // done
            lastRun = false;
        } else {
            // Next non-silent region starts at the end of this silent region
            nsStart = curSrc->end;
            ++srcIter;
            if (srcIter == src.end()) {
                lastRun = true;
            }
        }
    }
}

/*
void TruncSilenceBase::BlendFrames(float* buffer, int blendFrameCount, int
leftIndex, int rightIndex)
{
   float* bufOutput = &buffer[leftIndex];
   float* bufBefore = &buffer[leftIndex];
   float* bufAfter  = &buffer[rightIndex];
   double beforeFactor = 1.0;
   double afterFactor  = 0.0;
   double adjFactor = 1.0 / (double)blendFrameCount;
   for (int j = 0; j < blendFrameCount; ++j)
   {
      bufOutput[j] = (float)((bufBefore[j] * beforeFactor) + (bufAfter[j] *
afterFactor)); beforeFactor -= adjFactor; afterFactor  += adjFactor;
   }
}
*/

bool TruncSilenceBase::NeedsDither() const
{
    return false;
}
