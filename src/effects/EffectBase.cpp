/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectBase.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

*******************************************************************//**

\class EffectBase
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/


#include "EffectBase.h"

#include <thread>
#include "AudioIO.h"
#include "ConfigInterface.h"
#include "widgets/wxWidgetsWindowPlacement.h"
#include "../MixAndRender.h"
#include "PluginManager.h"
#include "ProjectAudioIO.h"
#include "QualitySettings.h"
#include "TransactionScope.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/NumericTextCtrl.h"

// Effect application counter
int EffectBase::nEffectsDone = 0;

EffectBase::EffectBase()
{
   // PRL:  I think this initialization of mProjectRate doesn't matter
   // because it is always reassigned in DoEffect before it is used
   // STF: but can't call AudioIOBase::GetOptimalSupportedSampleRate() here.
   // (Which is called to compute the default-default value.)  (Bug 2280)
   mProjectRate = QualitySettings::DefaultSampleRate.ReadWithDefault(44100);
}

EffectBase::~EffectBase() = default;

double EffectBase::GetDefaultDuration()
{
   return 30.0;
}

// TODO:  Lift the possible user-prompting part out of this function, so that
// the recursive paths into this function via Effect::Delegate are simplified,
// and we don't have both EffectSettings and EffectSettingsAccessPtr
// If pAccess is not null, settings should have come from its Get()
bool EffectBase::DoEffect(EffectContext &context, EffectSettings &settings,
   const InstanceFinder &finder,
   double projectRate,
   TrackList *list,
   NotifyingSelectedRegion &selectedRegion,
   const EffectSettingsAccessPtr &pAccess)
{
   wxASSERT(selectedRegion.duration() >= 0.0);

   mOutputTracks.reset();

   mProjectRate = projectRate;

   SetTracks(list);
   // Don't hold a dangling pointer when done
   Finally Do([&]{ SetTracks(nullptr); });

   // This is for performance purposes only, no additional recovery implied
   auto &pProject = *const_cast<AudacityProject*>(FindProject()); // how to remove this const_cast?
   TransactionScope trans(pProject, "Effect");

   // Update track/group counts
   context.CountWaveTracks(*mTracks);

   bool isSelection = false;

   auto duration = 0.0;
   if (GetType() == EffectTypeGenerate)
      GetConfig(GetDefinition(), PluginSettings::Private,
         CurrentSettingsGroup(),
         EffectSettingsExtra::DurationKey(), duration, GetDefaultDuration());

   WaveTrack *newTrack{};
   bool success = false;
   auto oldDuration = duration;

   auto cleanup = finally( [&] {
      if (!success) {
         if (newTrack) {
            mTracks->Remove(newTrack);
         }
         // On failure, restore the old duration setting
         settings.extra.SetDuration(oldDuration);
      }
      else
         trans.Commit();

      ReplaceProcessedTracks( false );
      mPresetNames.clear();
   } );

   // We don't yet know the effect type for code in the Nyquist Prompt, so
   // assume it requires a track and handle errors when the effect runs.
   if ((GetType() == EffectTypeGenerate || GetPath() == NYQUIST_PROMPT_ID) &&
       (context.numTracks == 0)
   ) {
      auto track = context.factory->Create();
      track->SetName(mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
      newTrack = mTracks->Add(track);
      newTrack->SetSelected(true);
   }

   mT0 = selectedRegion.t0();
   mT1 = selectedRegion.t1();
   if (mT1 > mT0)
   {
      // there is a selection: let's fit in there...
      // MJS: note that this is just for the TTC and is independent of the track rate
      // but we do need to make sure we have the right number of samples at the project rate
      double quantMT0 = QUANTIZED_TIME(mT0, mProjectRate);
      double quantMT1 = QUANTIZED_TIME(mT1, mProjectRate);
      duration = quantMT1 - quantMT0;
      isSelection = true;
      mT1 = mT0 + duration;
   }

   // This is happening inside EffectSettingsAccess::ModifySettings
   auto newFormat = isSelection
      ? NumericConverter::TimeAndSampleFormat()
      : NumericConverter::DefaultSelectionFormat();
   auto updater = [&](EffectSettings &settings) {
      settings.extra.SetDuration(duration);
      settings.extra.SetDurationFormat( newFormat );
      return nullptr;
   };
   // Update our copy of settings; update the EffectSettingsAccess too,
   // if we are going to show a dialog
   updater(settings);
   if (pAccess)
      pAccess->ModifySettings(updater);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mF0 = selectedRegion.f0();
   mF1 = selectedRegion.f1();
   if( mF0 != SelectedRegion::UndefinedFrequency )
      mPresetNames.push_back(L"control-f0");
   if( mF1 != SelectedRegion::UndefinedFrequency )
      mPresetNames.push_back(L"control-f1");

#endif
   context.CountWaveTracks(*mTracks);

   // Allow the dialog factory to fill this in, but it might not
   std::shared_ptr<EffectInstance> pInstance;

   if (IsInteractive()) {
      if (auto result = finder(settings))
         pInstance = *result;
      else
         return false;
   }

   auto pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(pInstance);
   if (!pInstanceEx) {
      // Path that skipped the dialog factory -- effect may be non-interactive
      // or this is batch mode processing or repeat of last effect with stored
      // settings.
      pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(MakeInstance());
      // Note: Init may read parameters from preferences
      if (!pInstanceEx || !pInstanceEx->Init())
         return false;
   }


   // If the dialog was shown, then it has been closed without errors or
   // cancellation, and any change of duration has been saved in the config file

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipEffect(settings);
   if (skipFlag == false)
   {
      using namespace BasicUI;
      auto name = GetName();
      auto progress = MakeProgress(
         name,
         XO("Applying %s...").Format( name ),
         ProgressShowCancel
      );
      auto vr = valueRestorer(context.pProgress, progress.get());

      assert(pInstanceEx); // null check above
      returnVal = pInstanceEx->Process(context, settings);
   }

   if (returnVal && (mT1 >= mT0 ))
   {
      selectedRegion.setTimes(mT0, mT1);
   }

   success = returnVal;
   return returnVal;
}

void EffectBase::SetLinearEffectFlag(bool linearEffectFlag)
{
   mIsLinearEffect = linearEffectFlag;
}

void EffectBase::SetPreviewFullSelectionFlag(bool previewDurationFlag)
{
   mPreviewFullSelection = previewDurationFlag;
}


// If bGoodResult, replace mTracks tracks with successfully processed mOutputTracks copies.
// Else clear and DELETE mOutputTracks copies.
void EffectBase::ReplaceProcessedTracks(const bool bGoodResult)
{
   if (!bGoodResult) {
      // Free resources, unless already freed.

      // Processing failed or was cancelled so throw away the processed tracks.
      if ( mOutputTracks )
         mOutputTracks->Clear();

      // Reset map
      mIMap.clear();
      mOMap.clear();

      //TODO:undo the non-gui ODTask transfer
      return;
   }

   // Assume resources need to be freed.
   wxASSERT(mOutputTracks); // Make sure we at least did the CopyInputTracks().

   auto iterOut = mOutputTracks->ListOfTracks::begin(),
      iterEnd = mOutputTracks->ListOfTracks::end();

   size_t cnt = mOMap.size();
   size_t i = 0;

   for (; iterOut != iterEnd; ++i) {
      ListOfTracks::value_type o = *iterOut;
      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != o.get()) {
         const auto t = mIMap[i];
         if (t) {
            mTracks->Remove(t);
         }
         i++;
      }

      // This should never happen
      wxASSERT(i < cnt);

      // Remove the track from the output list...don't DELETE it
      iterOut = mOutputTracks->erase(iterOut);

      const auto  t = mIMap[i];
      if (t == NULL)
      {
         // This track is a NEW addition to output tracks; add it to mTracks
         mTracks->Add( o );
      }
      else
      {
         // Replace mTracks entry with the NEW track
         mTracks->Replace(t, o);
      }
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      const auto t = mIMap[i];
      if (t) {
         mTracks->Remove(t);
      }
      i++;
   }

   // Reset map
   mIMap.clear();
   mOMap.clear();

   // Make sure we processed everything
   wxASSERT(mOutputTracks->empty());

   // The output list is no longer needed
   mOutputTracks.reset();
   nEffectsDone++;
}

const AudacityProject *EffectBase::FindProject() const
{
   if (!inputTracks())
      return nullptr;
   return inputTracks()->GetOwner();
}

std::any EffectBase::BeginPreview(const EffectSettings &)
{
   return {};
}

void EffectBase::Preview(EffectContext &context,
   EffectSettingsAccess &access, std::function<void()> updateUI, bool dryOnly)
{
   auto cleanup0 = BeginPreview(access.Get());

   if (context.numTracks == 0) // nothing to preview
      return;

   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsBusy()) {
      return;
   }

   wxWindow *FocusDialog = wxWindow::FindFocus();

   double previewDuration;
   bool isNyquist = GetFamily() == NYQUISTEFFECTS_FAMILY;
   bool isGenerator = GetType() == EffectTypeGenerate;

   // Mix a few seconds of audio from all of the tracks
   auto previewLen = EffectPreviewLength.Read();

   const double rate = mProjectRate;

   const auto &settings = access.Get();
   if (isNyquist && isGenerator)
      previewDuration = CalcPreviewInputLength(context, settings, previewLen);
   else
      previewDuration = std::min(settings.extra.GetDuration(),
         CalcPreviewInputLength(context, settings, previewLen));

   double t1 = mT0 + previewDuration;

   if ((t1 > mT1) && !isGenerator) {
      t1 = mT1;
   }

   if (t1 <= mT0)
      return;

   bool success = true;

   auto cleanup = finally( [&] {

      // Effect is already inited; we will call Process and then Init
      // again, so the state is exactly the way it was before Preview
      // was called.
      if (!dryOnly)
         // TODO remove this reinitialization of state within the Effect object
         // It is done indirectly via Effect::Instance
         if (auto pInstance =
            std::dynamic_pointer_cast<EffectInstanceEx>(MakeInstance())
         )
            pInstance->Init();
   } );

   auto vr0 = valueRestorer( mT0 );
   auto vr1 = valueRestorer( mT1 );
   // Most effects should stop at t1.
   if (!mPreviewFullSelection)
      mT1 = t1;

   // In case any dialog control depends on mT1 or mDuration:
   if (updateUI)
      updateUI();

   // Save the original track list
   TrackList *saveTracks = mTracks;

   auto cleanup2 = finally( [&] {
      mTracks = saveTracks;
      if (FocusDialog) {
         FocusDialog->SetFocus();
      }

      // In case of failed effect, be sure to free memory.
      ReplaceProcessedTracks( false );
   } );

   // Build NEW tracklist from rendering tracks
   // Set the same owning project, so FindProject() can see it within Process()
   const auto pProject = saveTracks->GetOwner();
   auto uTracks = TrackList::Create( pProject );
   mTracks = uTracks.get();

   // Linear Effect preview optimised by pre-mixing to one track.
   // Generators need to generate per track.
   if (mIsLinearEffect && !isGenerator) {
      WaveTrack::Holder mixLeft, mixRight;
      MixAndRender(saveTracks->Selected<const WaveTrack>(),
         Mixer::WarpOptions{ *saveTracks },
         wxString{}, // Don't care about the name of the temporary tracks
         context.factory, rate, floatSample, mT0, t1, mixLeft, mixRight);
      if (!mixLeft)
         return;

      mixLeft->Offset(-mixLeft->GetStartTime());
      mixLeft->SetSelected(true);
      auto pLeft = mTracks->Add( mixLeft );
      Track *pRight{};
      if (mixRight) {
         mixRight->Offset(-mixRight->GetStartTime());
         mixRight->SetSelected(true);
         pRight = mTracks->Add( mixRight );
         mTracks->MakeMultiChannelTrack(*pLeft, 2, true);
      }
   }
   else {
      for (auto src : saveTracks->Any< const WaveTrack >()) {
         if (src->GetSelected()) {
            auto dest = src->Copy(mT0, t1);
            dest->SetSelected(src->GetSelected());
            mTracks->Add( dest );
         }
      }
   }

   // NEW tracks start at time zero.
   // Adjust mT0 and mT1 to be the times to process, and to
   // play back in these tracks
   mT1 -= mT0;
   mT0 = 0.0;

   // Update track/group counts
   context.CountWaveTracks(*mTracks);

   // Apply effect
   if (!dryOnly) {
      using namespace BasicUI;
      auto progress = MakeProgress(
         GetName(),
         XO("Preparing preview"),
         ProgressShowStop
      ); // Have only "Stop" button.
      auto vr = valueRestorer(context.pProgress, progress.get());

      auto vr2 = valueRestorer(context.isPreviewing, true);

      access.ModifySettings([&](EffectSettings &settings){
         // Preview of non-realtime effect
         auto pInstance =
            std::dynamic_pointer_cast<EffectInstanceEx>(MakeInstance());
         success = pInstance && pInstance->Process(context, settings);
         return nullptr;
      });
   }

   if (success)
   {
      auto tracks = TransportTracks{ *mTracks, true };

      // Some effects (Paulstretch) may need to generate more
      // than previewLen, so take the min.
      t1 = std::min(mT0 + previewLen, mT1);

      // Start audio playing
      auto options = ProjectAudioIO::GetDefaultOptions(*pProject);
      int token = gAudioIO->StartStream(tracks, mT0, t1, t1, options);

      if (token) {
         auto previewing = ProgressResult::Success;
         // The progress dialog must be deleted before stopping the stream
         // to allow events to flow to the app during StopStream processing.
         // The progress dialog blocks these events.
         {
            ProgressDialog progress
            (GetName(), XO("Previewing"), pdlgHideCancelButton);

            while (gAudioIO->IsStreamActive(token) && previewing == ProgressResult::Success) {
               using namespace std::chrono;
               std::this_thread::sleep_for(100ms);
               previewing = progress.Update(gAudioIO->GetStreamTime() - mT0, t1 - mT0);
            }
         }

         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            using namespace std::chrono;
            std::this_thread::sleep_for(100ms);
         }
      }
      else {
         using namespace BasicUI;
         ShowErrorDialog(
            wxWidgetsWindowPlacement{ FocusDialog }, XO("Error"),
            XO("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
            wxT("Error_opening_sound_device"),
            ErrorDialogOptions{ ErrorDialogType::ModalErrorReport } );
      }
   }
}

DoubleSetting EffectPreviewLength{ "/AudioIO/EffectsPreviewLen", 6.0 };
