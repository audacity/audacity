/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.cpp

  Dominic Mazzoni
 
  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#include "ExportUtils.h"
#include "Track.h"
#include "Mix.h"
#include "BasicUI.h"
#include "WaveTrack.h"
#include "MixAndRender.h"
#include "ProgressDialog.h"
#include "StretchingSequence.h"
#include "wxFileNameWrapper.h"

//Create a mixer by computing the time warp factor
std::unique_ptr<Mixer> ExportUtils::CreateMixer(const TrackList &tracks,
         bool selectionOnly,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         MixerOptions::Downmix *mixerSpec)
{
   Mixer::Inputs inputs;

   bool anySolo =
      !(tracks.Leaders<const WaveTrack>() + &WaveTrack::GetSolo).empty();

   const auto range = tracks.Leaders<const WaveTrack>()
      + (selectionOnly ? &Track::IsSelected : &Track::Any)
      - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);
   for (auto pTrack: range)
      inputs.emplace_back(
         StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()),
         GetEffectStages(*pTrack));
   // MB: the stop time should not be warped, this was a bug.
   return std::make_unique<Mixer>(move(inputs),
                  // Throw, to stop exporting, if read fails:
                  true,
                  Mixer::WarpOptions{ tracks.GetOwner() },
                  startTime, stopTime,
                  numOutChannels, outBufferSize, outInterleaved,
                  outRate, outFormat,
                  true, mixerSpec);
}

void ExportUtils::InitProgress(std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
   const TranslatableString &title, const TranslatableString &message)
{
   if (!pDialog)
      pDialog = std::make_unique<ProgressDialog>( title, message );
   else
   {
      if (auto pd = dynamic_cast<ProgressDialog*>(pDialog.get()))
      {
         pd->SetTitle(title);
         pd->Reinit();
      }

      pDialog->SetMessage(message);
   }
}

void ExportUtils::InitProgress(std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
   const wxFileNameWrapper &title, const TranslatableString &message)
{
   return InitProgress(
      pDialog, Verbatim( title.GetName() ), message );
}
