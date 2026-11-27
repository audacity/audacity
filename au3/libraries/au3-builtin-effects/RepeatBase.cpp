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

            // Au4: with clips selected, repeat only those: copy from a duplicate without the unselected clips,
            // and leave tracks without selected clips alone. The selected clips are found by ID in the project's track,
            // then by start time here: rendering pitch and speed changes gives clips new IDs.
            // Not in preview, whose input already has only the selected clips (mixed into new ones).
            std::shared_ptr<WaveTrack> selectedClipsOnly;
            if (!(mSelectedClipIds.empty() && mSelectedLabelIds.empty()) && !mIsPreview) {
                std::vector<double> selectedClipStarts;
                if (const auto original = dynamic_cast<const WaveTrack*>(mTracks->FindById(track.GetId()))) {
                    for (const auto& clip : original->Intervals()) {
                        if (std::find(mSelectedClipIds.begin(), mSelectedClipIds.end(), clip->GetId())
                            != mSelectedClipIds.end()) {
                            selectedClipStarts.push_back(clip->GetPlayStartTime());
                        }
                    }
                }
                if (selectedClipStarts.empty()) {
                    return;
                }
                const auto eps = 0.5 / track.GetRate();
                const auto isSelected = [&](const auto& clip) {
                    return std::any_of(selectedClipStarts.begin(), selectedClipStarts.end(), [&](double start) {
                        return std::abs(clip->GetPlayStartTime() - start) < eps;
                    });
                };
                // The rendered clips aren't selected: select them again, so that they and their repeats
                // end up selected, like other clips
                for (const auto& clip : track.Intervals()) {
                    if (isSelected(clip)) {
                        clip->SetSelected(true);
                    }
                }
                selectedClipsOnly = std::static_pointer_cast<WaveTrack>(track.Duplicate());
                const auto intervals = std::vector(
                    selectedClipsOnly->Intervals().begin(), selectedClipsOnly->Intervals().end());
                for (const auto& clip : intervals) {
                    if (!isSelected(clip)) {
                        selectedClipsOnly->RemoveInterval(clip);
                    }
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
                // Like Au3: insert the repeat, moving what follows to the right,
                // and merge it into the clip it is inserted in (Paste() always makes new clips)
                const bool merge = true;
                const bool moveClips = true;
                track.PasteWaveTrack(t0, *firstTemp, merge, moveClips);
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
            // A placeholder only keeps the copy's length (silence at the end of the selection): it isn't pasted
            sourceIntervals.erase(std::remove_if(sourceIntervals.begin(), sourceIntervals.end(),
                                                 [](const auto& clip) { return clip->GetIsPlaceholder(); }),
                                  sourceIntervals.end());
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

        // Repeat on a duplicate and replace the track with it, like the wave tracks above,
        // so that Au4 reloads the labels. Not in preview, whose tracks aren't the project's.
        if (!mIsPreview) {
            // Au4: with clips or labels selected, repeat only the selected labels
            const bool selectedItemsOnly = !(mSelectedClipIds.empty() && mSelectedLabelIds.empty());
            const auto isSelectedLabel = [&](const LabelStruct& label) {
                return std::find(mSelectedLabelIds.begin(), mSelectedLabelIds.end(), label.GetId()) != mSelectedLabelIds.end();
            };
            std::vector<LabelTrack*> labelTracks;
            for (auto track : mTracks->Any<LabelTrack>()) {
                const auto& labels = track->GetLabels();
                if (selectedItemsOnly ? std::any_of(labels.begin(), labels.end(), isSelectedLabel) : track->GetSelected()) {
                    labelTracks.push_back(track);
                }
            }
            const double tLen = t1 - mT0;
            for (auto track : labelTracks) {
                const auto repeated = std::static_pointer_cast<LabelTrack>(track->Duplicate());
                // Copying gives the labels new IDs: keep the original ones, to find the selected labels and keep them selected
                for (int i = 0; i < track->GetNumLabels(); ++i) {
                    repeated->GetLabelById(repeated->GetLabel(i)->GetId())->SetId(track->GetLabel(i)->GetId());
                }
                if (selectedItemsOnly) {
                    // Like LabelTrack::Repeat(), for the selected labels only.
                    // Like LabelTrack::ShiftLabelsOnInsert(), but leaving the selected labels alone:
                    // they are in the repeated range, and one ending at its end would be stretched over the repeats
                    for (const auto& label : std::vector(repeated->GetLabels().begin(), repeated->GetLabels().end())) {
                        if (isSelectedLabel(label)) {
                            continue;
                        }
                        LabelStruct* shifted = repeated->GetLabelById(label.GetId());
                        const auto relation = shifted->RegionRelation(t1, t1, repeated.get());
                        if (relation == LabelStruct::BEFORE_LABEL) {
                            shifted->selectedRegion.move(tLen * repeatCount);
                        } else if (relation == LabelStruct::WITHIN_LABEL) {
                            shifted->selectedRegion.moveT1(tLen * repeatCount);
                        }
                    }
                    for (const auto& label : track->GetLabels()) {
                        if (!isSelectedLabel(label)) {
                            continue;
                        }
                        for (int j = 1; j <= repeatCount; ++j) {
                            LabelStruct copy { label.selectedRegion, label.getT0() + j * tLen, label.getT1() + j * tLen, label.title };
                            copy.CopyAttributesFrom(label);
                            repeated->AddLabel(copy);
                        }
                    }
                } else {
                    repeated->Repeat(mT0, t1, repeatCount);
                }
                mTracks->ReplaceOne(*track, std::move(*TrackList::Temporary(nullptr, repeated)));
                // Select the repeats even when no audio was repeated
                mT1 = std::max(mT1, t1 + tLen * repeatCount);
            }
        }
    }
    return bGoodResult;
}

bool RepeatBase::NeedsDither() const
{
    return false;
}
