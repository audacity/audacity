/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.cpp

  Effects that generate audio can derive from Generator.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#include "Generator.h"
#include "au3-effects/EffectOutputTracks.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-project/Project.h"
#include "au3-preferences/Prefs.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveTrackUtilities.h"

#include "au3-track/TimeWarper.h"

bool Generator::Process(EffectInstance&, EffectSettings& settings)
{
    const auto duration = settings.extra.GetDuration();

    // Set up mOutputTracks.
    // This effect needs all for sync-lock grouping.
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

    for (auto* track : outputs.Get().Selected<WaveTrack>()) {
        // Make sure there's room when generating into empty space.
        if (track->IsEmpty(mT0, mT1 + 1.0 / track->GetRate())
            && !track->IsEmpty(mT0, mT0 + duration - (mT1 - mT0) - 1.0 / track->GetRate())) {
            using namespace BasicUI;
            ShowMessageBox(
                TranslatableString("builtin-effects", "There is not enough room available to generate the audio"),
                MessageBoxOptions {}.IconStyle(Icon::Error));
            return false;
        }

        if (duration > 0.0) {
            auto copy = WaveTrackUtilities::EmptyCopy(*track, WaveTrackUtilities::RealtimeEffectsCopy::Ref);
            if (!GenerateTrack(settings, *copy)) {
                return false;
            }
            copy->Flush();
            PasteTimeWarper warper { mT1, mT0 + duration };
            constexpr auto merge = true;
            track->ClearAndPaste(mT0, mT1, *copy, mPreserveUnderlyingClipBoundaries, merge, &warper);
        } else {
            constexpr auto moveClips = false;
            track->Clear(mT0, mT1, moveClips);
        }
    }

    outputs.Commit();
    mT1 = mT0 + duration; // Update selection.
    return true;
}
