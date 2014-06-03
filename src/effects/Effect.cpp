/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//****************************************************************//**

\class EffectDialog
\brief New (Jun-2006) base class for effects dialogs.  Likely to get
greater use in future.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/timer.h>
#include <wx/hashmap.h>

#include "Effect.h"
#include "../AudioIO.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"
#include "../ondemand/ODManager.h"
#include "TimeWarper.h"

WX_DECLARE_VOIDPTR_HASH_MAP( bool, t2bHash );

//
// public static methods
//

double Effect::sDefaultGenerateLen = 30.0;


wxString Effect::StripAmpersand(const wxString& str)
{
   wxString strippedStr = str;
   strippedStr.Replace(wxT("&"), wxT(""));
   // ! is used for hiding effects, and should not affect sort order.
   strippedStr.Replace(wxT("!"), wxT(""));
   return strippedStr;
}


//
// public methods
//

Effect::Effect()
   : mWarper(NULL)
{
   mTracks = NULL;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
   mLength = 0;
   mNumTracks = 0;
   mNumGroups = 0;

   // Can change effect flags later (this is the new way)
   // OR using the old way, over-ride GetEffectFlags().
   mFlags = BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT;
}

Effect::~Effect() {
   if (mWarper != NULL)
      delete mWarper;
}

bool Effect::DoEffect(wxWindow *parent, int flags,
                      double projectRate,
                      TrackList *list,
                      TrackFactory *factory,
                      double *t0, double *t1, wxString params)
{
   wxASSERT(*t0 <= *t1);

   if (mOutputTracks) {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   mFactory = factory;
   mProjectRate = projectRate;
   mParent = parent;
   mTracks = list;
   mT0 = *t0;
   mT1 = *t1;
   CountWaveTracks();

   // Note: Init may read parameters from preferences
   if (!Init())
      return false;

   // If a parameter string was provided, it overrides any remembered settings
   // (but if the user is to be prompted, that takes priority)
   if (!params.IsEmpty())
   {
      ShuttleCli shuttle;
      shuttle.mParams = params;
      shuttle.mbStoreInClient=true;
      if( !TransferParameters( shuttle ))
      {
         wxMessageBox(
            wxString::Format(
               _("Could not set parameters of effect %s\n to %s."),
               GetEffectName().c_str(),
               params.c_str()
            )
         );
         return false;
      }
   }

   // Don't prompt user if we are dealing with a
   // effect that is already configured, e.g. repeating
   // the last effect on a different selection.
   if( (flags & CONFIGURED_EFFECT) == 0)
   {
      if (!PromptUser())
         return false;
   }

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipEffect();
   if (skipFlag == false) {
      mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                     GetEffectAction(),
                                     pdlgHideStopButton);
      returnVal = Process();
      delete mProgress;
   }

   End();

   if (mOutputTracks) {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   if (returnVal) {
      *t0 = mT0;
      *t1 = mT1;
   }

   return returnVal;
}

bool Effect::TotalProgress(double frac)
{
   int updateResult = mProgress->Update(frac);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackProgress(int whichTrack, double frac, wxString msg)
{
   int updateResult = mProgress->Update(whichTrack + frac, (double) mNumTracks, msg);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   int updateResult = mProgress->Update(whichGroup + frac, (double) mNumGroups);
   return (updateResult != eProgressSuccess);
}

void Effect::GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart ? trackStart : mT0;
   double t1 = mT1 > trackEnd ? trackEnd : mT1;

   if (mFlags & INSERT_EFFECT) {
      t1 = t0 + mLength;
      if (mT0 == mT1) {
         // Not really part of the calculation, but convenient to put here
         bool bResult = track->InsertSilence(t0, t1);
         wxASSERT(bResult); // TO DO: Actually handle this.
      }
   }

   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      sampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

void Effect::SetTimeWarper(TimeWarper *warper)
{
   if (mWarper != NULL)
   {
      delete mWarper;
      mWarper = NULL;
   }

   wxASSERT(warper != NULL);
   mWarper = warper;
}

TimeWarper *Effect::GetTimeWarper()
{
   wxASSERT(mWarper != NULL);
   return mWarper;
}

//
// private methods
//
// Use these two methods to copy the input tracks to mOutputTracks, if
// doing the processing on them, and replacing the originals only on success (and not cancel).
// Copy the group tracks that have tracks selected
void Effect::CopyInputTracks(int trackType)
{
   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   mOutputTracks = new TrackList();
   mOutputTracksType = trackType;

   //iterate over tracks of type trackType (All types if Track::All)
   TrackListOfKindIterator aIt(trackType, mTracks);
   t2bHash added;

   for (Track *aTrack = aIt.First(); aTrack; aTrack = aIt.Next()) {

      // Include selected tracks, plus sync-lock selected tracks for Track::All.
      if (aTrack->GetSelected() ||
            (trackType == Track::All && aTrack->IsSyncLockSelected()))
      {
         Track *o = aTrack->Duplicate();
         mOutputTracks->Add(o);
         mIMap.Add(aTrack);
         mOMap.Add(o);
      }
   }
}

void Effect::AddToOutputTracks(Track *t)
{
   mOutputTracks->Add(t);
   mIMap.Add(NULL);
   mOMap.Add(t);
}

// If bGoodResult, replace mTracks tracks with successfully processed mOutputTracks copies.
// Else clear and delete mOutputTracks copies.
void Effect::ReplaceProcessedTracks(const bool bGoodResult)
{
   wxASSERT(mOutputTracks != NULL); // Make sure we at least did the CopyInputTracks().

   if (!bGoodResult) {
      // Processing failed or was cancelled so throw away the processed tracks.
      mOutputTracks->Clear(true); // true => delete the tracks

      // Reset map
      mIMap.Clear();
      mOMap.Clear();

      //TODO:undo the non-gui ODTask transfer
      return;
   }

   TrackListIterator iterOut(mOutputTracks);

   Track *x;
   size_t cnt = mOMap.GetCount();
   size_t i = 0;

   for (Track *o = iterOut.First(); o; o = x, i++) {
      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != o) {
         Track *t = (Track *) mIMap[i];
         if (t) {
            mTracks->Remove(t, true);
         }
         i++;
      }

      // This should never happen
      wxASSERT(i < cnt);

      // Remove the track from the output list...don't delete it
      x = iterOut.RemoveCurrent(false);

      Track *t = (Track *) mIMap[i];
      if (t == NULL)
      {
         // This track is a new addition to output tracks; add it to mTracks
         mTracks->Add(o);
      }
      else
      {
         // Replace mTracks entry with the new track
         mTracks->Replace(t, o, false);

         // Swap the wavecache track the ondemand task uses, since now the new
         // one will be kept in the project
         if (ODManager::IsInstanceCreated()) {
            ODManager::Instance()->ReplaceWaveTrack((WaveTrack *)t,
                                                    (WaveTrack *)o);
         }

         // No longer need the original track
         delete t;
      }
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      Track *t = (Track *) mIMap[i];
      if (t) {
         mTracks->Remove((Track *)mIMap[i], true);
      }
      i++;
   }

   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   // Make sure we processed everything
   wxASSERT(iterOut.First() == NULL);

   // The output list is no longer needed
   delete mOutputTracks;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
}

void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   Track *t = iter.First();

   while(t) {
      if (!t->GetSelected()) {
         t = iter.Next();
         continue;
      }

      if (t->GetKind() == Track::Wave) {
         mNumTracks++;
         if (!t->GetLinked())
            mNumGroups++;
      }
      t = iter.Next();
   }
}

float TrapFloat(float x, float min, float max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double TrapDouble(double x, double min, double max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

long TrapLong(long x, long min, long max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double Effect::CalcPreviewInputLength(double previewLength)
{
   return previewLength;
}

wxString Effect::GetPreviewName()
{
   return _("Pre&view");
}

void Effect::Preview(bool dryOnly)
{
   if (mNumTracks==0) // nothing to preview
      return;

   wxWindow* FocusDialog = wxWindow::FindFocus();
   if (gAudioIO->IsBusy())
      return;

   // Mix a few seconds of audio from all of the tracks
   double previewLen = 6.0;
   gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);

   WaveTrack *mixLeft = NULL;
   WaveTrack *mixRight = NULL;
   double rate = mProjectRate;
   double t0 = mT0;
   double t1 = t0 + CalcPreviewInputLength(previewLen);

   if (t1 > mT1)
      t1 = mT1;

   if (t1 <= t0)
      return;

   bool success = ::MixAndRender(mTracks, mFactory, rate, floatSample, t0, t1,
                                 &mixLeft, &mixRight);

   if (!success) {
      return;
   }

   // Save the original track list
   TrackList *saveTracks = mTracks;

   // Build new tracklist from rendering tracks
   mTracks = new TrackList();
   mixLeft->SetSelected(true);
   mTracks->Add(mixLeft);
   if (mixRight) {
      mixRight->SetSelected(true);
      mTracks->Add(mixRight);
   }

   // Update track/group counts
   CountWaveTracks();

   // Reset times
   t0 = mixLeft->GetStartTime();
   t1 = mixLeft->GetEndTime();

   double t0save = mT0;
   double t1save = mT1;
   mT0 = t0;
   mT1 = t1;

   // Apply effect

   bool bSuccess(true);
   if (!dryOnly) {
      // Effect is already inited; we call Process, End, and then Init
      // again, so the state is exactly the way it was before Preview
      // was called.
      mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
            _("Preparing preview"),
            pdlgHideCancelButton); // Have only "Stop" button.
      bSuccess = Process();
      delete mProgress;
      End();
      Init();
   }
   if (bSuccess)
   {
      mT0 = t0save;
      mT1 = t1save;

      WaveTrackArray playbackTracks;
      WaveTrackArray recordingTracks;
      // Probably not the same tracks post-processing, so can't rely on previous values of mixLeft & mixRight.
      TrackListOfKindIterator iter(Track::Wave, mTracks);
      mixLeft = (WaveTrack*)(iter.First());
      mixRight = (WaveTrack*)(iter.Next());
      playbackTracks.Add(mixLeft);
      if (mixRight)
         playbackTracks.Add(mixRight);

      t1 = wxMin(mixLeft->GetEndTime(), t0 + previewLen);

#ifdef EXPERIMENTAL_MIDI_OUT
      NoteTrackArray empty;
#endif
      // Start audio playing
      int token =
         gAudioIO->StartStream(playbackTracks, recordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                               empty,
#endif
                               NULL, rate, t0, t1, NULL);

      if (token) {
         int previewing = eProgressSuccess;

         mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                        _("Previewing"), pdlgHideCancelButton);

         while (gAudioIO->IsStreamActive(token) && previewing == eProgressSuccess) {
            ::wxMilliSleep(100);
            previewing = mProgress->Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
         }
         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            ::wxMilliSleep(100);
         }

         delete mProgress;
      }
      else {
         wxMessageBox(_("Error while opening sound device. Please check the output device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, FocusDialog);
      }
   }

   if (FocusDialog) {
      FocusDialog->SetFocus();
   }

   delete mOutputTracks;
   mOutputTracks = NULL;

   mTracks->Clear(true); // true => delete the tracks
   delete mTracks;

   mTracks = saveTracks;
}

EffectDialog::EffectDialog(wxWindow * parent,
                           const wxString & title,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
   mAdditionalButtons = additionalButtons;
}

void EffectDialog::Init()
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);

      long buttons = eOkButton;

      if (mType == PROCESS_EFFECT || mType == INSERT_EFFECT)
      {
         buttons |= eCancelButton;
         if (mType == PROCESS_EFFECT)
         {
            buttons |= ePreviewButton;
         }
      }
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
{
   return;
}

bool EffectDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::Validate()
{
   return true;
}

void EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   return;
}
