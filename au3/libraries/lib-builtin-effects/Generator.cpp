/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.cpp

  Effects that generate audio can derive from Generator.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#include "Generator.h"
#include "EffectOutputTracks.h"

#include "BasicUI.h"
#include "Project.h"
#include "Prefs.h"
#include "SyncLock.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

#include "TimeWarper.h"

bool Generator::Process(EffectInstance&, EffectSettings& settings)
{
    const auto duration = settings.extra.GetDuration();

    // Set up mOutputTracks.
    // This effect needs all for sync-lock grouping.
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };

    // Iterate over the tracks
    bool bGoodResult = true;
    int ntrack = 0;

    outputs.Get().Any().VisitWhile(bGoodResult,
                                   [&](auto&& fallthrough){
        return [&](WaveTrack& track) {
            if (!track.GetSelected()) {
                return fallthrough();
            }
            bool editClipCanMove = GetEditClipsCanMove();

            //if we can't move clips, and we're generating into an empty space,
            //make sure there's room.
            if (!editClipCanMove
                && track.IsEmpty(mT0, mT1 + 1.0 / track.GetRate())
                && !track.IsEmpty(mT0,
                                  mT0 + duration - (mT1 - mT0) - 1.0 / track.GetRate())) {
                using namespace BasicUI;
                ShowMessageBox(
                    XO("There is not enough room available to generate the audio"),
                    MessageBoxOptions {}.IconStyle(Icon::Error));
                bGoodResult = false;
                return;
            }

            if (duration > 0.0) {
                // Create a temporary track
                auto copy = track.EmptyCopy();
                // Fill with data
                if (!GenerateTrack(settings, *copy)) {
                    bGoodResult = false;
                }
                if (bGoodResult) {
                    copy->Flush();
                    PasteTimeWarper warper { mT1, mT0 + duration };
                    auto pProject = FindProject();
                    const auto& selectedRegion
                        =ViewInfo::Get(*pProject).selectedRegion;
                    // According to https://manual.audacityteam.org/man/silence.html,
                    // generating silence with an audio selection should behave like
                    // the "Silence Audio" command, which doesn't affect track clip
                    // boundaries.
                    constexpr auto preserve = true;
                    constexpr auto merge = true;
                    track.ClearAndPaste(
                        selectedRegion.t0(), selectedRegion.t1(), *copy, preserve,
                        merge, &warper);
                } else {
                    return;
                }
            } else {
                // If the duration is zero, there's no need to actually
                // generate anything
                track.Clear(mT0, mT1);
            }

            ntrack++;
        };
    },
                                   [&](Track& t) {
        if (SyncLock::IsSyncLockSelected(t)) {
            t.SyncLockAdjust(mT1, mT0 + duration);
        }
    }
                                   );

    if (bGoodResult) {
        outputs.Commit();
        mT1 = mT0 + duration; // Update selection.
    }

    return bGoodResult;
}
