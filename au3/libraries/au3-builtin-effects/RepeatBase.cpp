#include "RepeatBase.h"
#include "au3-effects/EffectOutputTracks.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include <algorithm>
#include <cmath>

const EffectParameterMethods& RepeatBase::Parameters() const
{
    static CapturedParameters<RepeatBase, Count> parameters;
    return parameters;
}

const ComponentInterfaceSymbol RepeatBase::Symbol { TranslatableString("builtin-effects", "Repeat") };

RepeatBase::RepeatBase()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(true);
}

RepeatBase::~RepeatBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol RepeatBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString RepeatBase::GetDescription() const
{
    return TranslatableString("builtin-effects", "Repeats the selection the specified number of times");
}

ManualPageID RepeatBase::ManualPage() const
{
    return L"Repeat";
}

// EffectDefinitionInterface implementation

EffectType RepeatBase::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

bool RepeatBase::Process(EffectInstance&, EffectSettings&)
{
    // Set up mOutputTracks.
    // This effect needs all for sync-lock grouping.
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

    int nTrack = 0;
    bool bGoodResult = true;
    double maxDestLen = 0.0; // used to change selection to generated bit

    const double t1 = mT1;

    // Au4: the output tracks are wave tracks only; label tracks are repeated below
    outputs.Get().Any().VisitWhile(
        bGoodResult,
        [&](WaveTrack& track) {
            if (!track.GetSelected()) {
                return;
            }
            auto start = track.TimeToLongSamples(mT0);
            auto end = track.TimeToLongSamples(mT1);
            auto len = end - start;
            const double tLen = track.LongSamplesToTime(len);
            const double tc = mT0 + tLen;

            if (len <= 0) {
                return;
            }

            // Au4: with clips selected, repeat only those: copy from a duplicate (which keeps the clip IDs)
            // without the unselected clips, and leave tracks without selected clips alone.
            // Not in preview, which mixes the selection into new clips.
            std::shared_ptr<WaveTrack> selectedClipsOnly;
            if (!mSelectedClipIds.empty() && !mIsPreview) {
                selectedClipsOnly = std::static_pointer_cast<WaveTrack>(track.Duplicate());
                bool hasSelectedClip = false;
                const auto intervals = std::vector(
                    selectedClipsOnly->Intervals().begin(), selectedClipsOnly->Intervals().end());
                for (const auto& clip : intervals) {
                    if (std::find(mSelectedClipIds.begin(), mSelectedClipIds.end(), clip->GetId())
                        != mSelectedClipIds.end()) {
                        hasSelectedClip = true;
                    } else {
                        selectedClipsOnly->RemoveInterval(clip);
                    }
                }
                if (!hasSelectedClip) {
                    return;
                }
            }

            auto firstTemp = std::static_pointer_cast<WaveTrack>(
                (selectedClipsOnly ? *selectedClipsOnly : track).Copy(mT0, mT1));

            auto t0 = tc;
            for (size_t j = 0; j < repeatCount; ++j) {
                if (TrackProgress(nTrack, static_cast<double>(j) / repeatCount)) {
                    // TrackProgress returns true on Cancel.
                    bGoodResult = false;
                    return;
                }
                // Copies keep the source clip's ID: give each pasted repeat its own,
                // or Au4 can't tell the clips apart (e.g. undo leaves them on screen)
                for (const auto& clip : firstTemp->Intervals()) {
                    clip->SetId(WaveClip::NewID());
                }
                const bool moveClips = false; // this need check
                track.Paste(t0, *firstTemp, moveClips);
                t0 += tLen;
            }
            if (t0 > maxDestLen) {
                maxDestLen = t0;
            }

            const auto compareIntervals = [](const auto& a, const auto& b) {
                return a->Start() < b->Start();
            };

            const auto eps = 0.5 / track.GetRate();
            auto sortedIntervals
                =std::vector(track.Intervals().begin(), track.Intervals().end());
            auto sourceIntervals = std::vector(
                firstTemp->Intervals().begin(), firstTemp->Intervals().end());
            std::sort(
                sortedIntervals.begin(), sortedIntervals.end(),
                compareIntervals);
            std::sort(
                sourceIntervals.begin(), sourceIntervals.end(),
                compareIntervals);
            for (auto it = sortedIntervals.begin(); it != sortedIntervals.end();
                 ++it) {
                const auto& interval = *it;
                // Find first pasted interval
                if (std::abs((*it)->GetPlayStartTime() - tc) > eps) {
                    continue;
                }

                // Fix pasted clips names
                for (int j = 0; j < repeatCount; ++j) {
                    for (const auto& src : sourceIntervals) {
                        if (it == sortedIntervals.end()) {
                            break;
                        }
                        (*it++)->SetName(src->GetName());
                    }
                }
                break;
            }
            nTrack++;
    });

    if (bGoodResult) {
        // Select the NEW bits + original bit
        mT1 = maxDestLen;
    }

    if (bGoodResult) {
        outputs.Commit();

        // Not in preview, whose tracks aren't the project's
        if (!mIsPreview) {
            for (auto track : mTracks->Selected<LabelTrack>()) {
                track->Repeat(mT0, t1, repeatCount);
            }
        }
    }
    return bGoodResult;
}

bool RepeatBase::NeedsDither() const
{
    return false;
}
