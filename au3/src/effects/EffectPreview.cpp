/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectPreview.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from EffectBase.cpp

*//*******************************************************************/
#include "EffectPreview.h"
#include "EffectBase.h"

#include "AudioIO.h"
#include "BasicUI.h"
#include "MixAndRender.h"
#include "ProjectAudioIO.h"
#include "TransportUtilities.h"
#include "WaveTrack.h"

void EffectPreview(EffectBase& effect,
                   EffectSettingsAccess& access, std::function<void()> updateUI, bool dryOnly)
{
    auto cleanup0 = effect.BeginPreview(access.Get());

    // These are temporary state in the Effect object that are meant be moved to
    // a new class EffectContext
    const auto numTracks = effect.mNumTracks;
    const auto rate = effect.mProjectRate;
    const auto& factory = effect.mFactory;
    auto& mT0 = effect.mT0;
    auto& mT1 = effect.mT1;
    auto& mTracks = effect.mTracks;
    auto& mProgress = effect.mProgress;
    auto& mIsPreview = effect.mIsPreview;

    // Get certain immutable properties of the effect
    const auto previewFullSelection = effect.PreviewsFullSelection();
    const auto isLinearEffect = effect.IsLinearEffect();

    if (numTracks == 0) { // nothing to preview
        return;
    }

    auto gAudioIO = AudioIO::Get();
    if (gAudioIO->IsBusy()) {
        return;
    }

    const auto FocusDialog = BasicUI::FindFocus();
    assert(FocusDialog); // postcondition

    double previewDuration;
    bool isNyquist = effect.GetFamily() == NYQUISTEFFECTS_FAMILY;
    bool isGenerator = effect.GetType() == EffectTypeGenerate;

    // Mix a few seconds of audio from all of the tracks
    double previewLen;
    gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen, 6.0);

    const auto& settings = access.Get();
    if (isNyquist && isGenerator) {
        previewDuration = effect.CalcPreviewInputLength(settings, previewLen);
    } else {
        previewDuration = std::min(settings.extra.GetDuration(),
                                   effect.CalcPreviewInputLength(settings, previewLen));
    }

    double t1 = mT0 + previewDuration;

    if ((t1 > mT1) && !isGenerator) {
        t1 = mT1;
    }

    if (t1 <= mT0) {
        return;
    }

    bool success = true;

    auto cleanup = finally([&] {
        // Effect is already inited; we will call Process and then Init
        // again, so the state is exactly the way it was before Preview
        // was called.
        if (!dryOnly) {
            // TODO remove this reinitialization of state within the Effect object
            // It is done indirectly via Effect::Instance
            if (auto pInstance
                    =std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance())
                    ) {
                pInstance->Init();
            }
        }
    });

    auto vr0 = valueRestorer(mT0);
    auto vr1 = valueRestorer(mT1);
    // Most effects should stop at t1.
    if (!previewFullSelection) {
        mT1 = t1;
    }

    // In case any dialog control depends on mT1 or mDuration:
    if (updateUI) {
        updateUI();
    }

    // Save the original track list
    auto saveTracks = mTracks;

    auto cleanup2 = finally([&] {
        mTracks = saveTracks;
        if (*FocusDialog) {
            BasicUI::SetFocus(*FocusDialog);
        }
    });

    // Build NEW tracklist from rendering tracks
    // Set the same owning project, so FindProject() can see it within Process()
    const auto pProject = saveTracks->GetOwner();
    mTracks = TrackList::Create(pProject);

    // Linear Effect preview optimised by pre-mixing to one track.
    // Generators need to generate per track.
    if (isLinearEffect && !isGenerator) {
        auto newTrack = MixAndRender(
            saveTracks->Selected<const WaveTrack>(),
            Mixer::WarpOptions { saveTracks->GetOwner() },
            wxString {}, // Don't care about the name of the temporary tracks
            factory, rate, floatSample, mT0, t1);
        if (!newTrack) {
            return;
        }
        mTracks->Add(newTrack);

        newTrack->MoveTo(0);
        newTrack->SetSelected(true);
    } else {
        for (auto src : saveTracks->Selected<const WaveTrack>()) {
            auto dest = src->Copy(mT0, t1);
            dest->SetSelected(true);
            mTracks->Add(dest);
        }
    }

    // NEW tracks start at time zero.
    // Adjust mT0 and mT1 to be the times to process, and to
    // play back in these tracks
    mT1 -= mT0;
    mT0 = 0.0;

    // Update track/group counts
    effect.CountWaveTracks();

    // Apply effect
    if (!dryOnly) {
        using namespace BasicUI;
        auto progress = MakeProgress(
            effect.GetName(),
            XO("Preparing preview"),
            ProgressShowStop
            ); // Have only "Stop" button.
        auto vr = valueRestorer(mProgress, progress.get());

        auto vr2 = valueRestorer(mIsPreview, true);

        access.ModifySettings([&](EffectSettings& settings){
            // Preview of non-realtime effect
            auto pInstance
                =std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance());
            success = pInstance && pInstance->Process(settings);
            return nullptr;
        });
    }

    if (success) {
        auto tracks = MakeTransportTracks(*mTracks, true);

        // Some effects (Paulstretch) may need to generate more
        // than previewLen, so take the min.
        t1 = std::min(mT0 + previewLen, mT1);

        // Start audio playing
        auto options = ProjectAudioIO::GetDefaultOptions(*pProject);
        int token = gAudioIO->StartStream(tracks, mT0, t1, t1, options);

        if (token) {
            using namespace BasicUI;
            auto previewing = ProgressResult::Success;
            // The progress dialog must be deleted before stopping the stream
            // to allow events to flow to the app during StopStream processing.
            // The progress dialog blocks these events.
            {
                auto progress = MakeProgress(effect.GetName(),
                                             XO("Previewing"), ProgressShowStop);

                while (gAudioIO->IsStreamActive(token) && previewing == ProgressResult::Success) {
                    using namespace std::chrono;
                    std::this_thread::sleep_for(100ms);
                    previewing = progress->Poll(
                        gAudioIO->GetStreamTime() - mT0, t1 - mT0);
                }
            }

            gAudioIO->StopStream();

            while (gAudioIO->IsBusy()) {
                using namespace std::chrono;
                std::this_thread::sleep_for(100ms);
            }
        } else {
            using namespace BasicUI;
            ShowErrorDialog(
                *FocusDialog, XO("Error"),
                XO("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
                wxT("Error_opening_sound_device"),
                ErrorDialogOptions { ErrorDialogType::ModalErrorReport });
        }
    }
}
