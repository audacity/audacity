/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class EffectNyquist
\brief An Effect that calls up a Nyquist (XLISP) plug in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class NyquistInputDialog
\brief Dialog used with EffectNyquist

*//****************************************************************//**

\class NyquistOutputDialog
\brief Dialog used with EffectNyquist

*//****************************************************************//**

\class NyquistDialog
\brief Base class for NyquistInputDialog and NyquistOutputDialog.

*//****************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/



#include "../../Audacity.h"

#include <math.h>
#include <locale.h>

#include <wx/defs.h>
#include <wx/log.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/textfile.h>
#include <wx/choice.h>
#include <wx/checkbox.h>

#if defined EXPERIMENTAL_NYQUIST_TIME_PROPERTY
#include <wx/datetime.h>
#endif

#include "../../AudacityApp.h"
#include "../../FileNames.h"
#include "../../Internat.h"
#include "../../LabelTrack.h"
#include "../../Project.h"
#include "../../ShuttleGui.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../widgets/valnum.h"
#include "../../Prefs.h"

#include "Nyquist.h"

#ifndef nyx_returns_start_and_end_time
#error You need to update lib-src/libnyquist
#endif

#include <locale.h>
#include <iostream>
#include <ostream>
#include <sstream>
#include <float.h>

#include <wx/arrimpl.cpp>

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffect
//
///////////////////////////////////////////////////////////////////////////////

#define UNINITIALIZED_CONTROL ((double)99999999.99)

WX_DEFINE_OBJARRAY(NyqControlArray);

EffectNyquist::EffectNyquist(wxString fName)
{
   mAction = _("Applying Nyquist Effect...");
   mInputCmd = wxEmptyString;
   mCmd = wxEmptyString;
   SetEffectFlags(HIDDEN_EFFECT);
   mInteractive = false;
   mExternal = false;
   mCompiler = false;
   mDebug = false;
   mIsSal = false;
   mOK = false;
   mAuthor = wxT("N/A");
   mCopyright = wxT("N/A");
   // set clip/split handling when applying over clip boundary.
   mRestoreSplits = true;  // Default: Restore split lines. 
   mMergeClips = -1;       // Default (auto):  Merge if length remains unchanged.

   mVersion = 4;

   mStop = false;
   mBreak = false;
   mCont = false;

   if (!SetXlispPath()) {
      wxLogWarning(wxT("Critical Nyquist files could not be found. Nyquist effects will not work."));
      return;
   }

   if (fName == wxT("")) {
      // Interactive Nyquist
      gPrefs->Read(wxT("/Effects/NyquistPrompt/Version"), &mVersion, 4);
      mOK = true;
      mInteractive = true;
      mName = wxTRANSLATE("Nyquist Prompt...");
      SetEffectFlags(PROCESS_EFFECT | BUILTIN_EFFECT | ADVANCED_EFFECT);
      return;
   }

   // wxLogNull dontLog; // Vaughan, 2010-08-27: Why turn off logging? Logging is good!

   mName = wxFileName(fName).GetName();
   mFileName = wxFileName(fName);
   mFileModified = mFileName.GetModificationTime();
   ParseFile();
}

EffectNyquist::~EffectNyquist()
{
   nyx_set_xlisp_path(NULL);
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString EffectNyquist::GetPath()
{
   if (mFileName.GetFullPath().IsEmpty())
   {
      return wxT("nyquist prompt");
   }

   return mFileName.GetFullPath();
}

wxString EffectNyquist::GetSymbol()
{
   return mName;
}

wxString EffectNyquist::GetName()
{
   return GetSymbol();
}

wxString EffectNyquist::GetVendor()
{
   if (GetPath() == wxT("nyquist prompt"))
   {
      return _("Audacity");
   }

   return mAuthor;
}

wxString EffectNyquist::GetVersion()
{
   return wxT("N/A");
}

wxString EffectNyquist::GetDescription()
{
   return mCopyright;
}

// ============================================================================
// EffectIdentInterface implementation
// ============================================================================

EffectType EffectNyquist::GetType()
{
   // For now, relegate to Effect()
   return Effect::GetType();
}

wxString EffectNyquist::GetFamily()
{
   return NYQUISTEFFECTS_FAMILY;
}

bool EffectNyquist::IsInteractive()
{
   // For now, relegate to Effect()
   return Effect::IsInteractive();
}

bool EffectNyquist::IsDefault()
{
   if (GetPath() == wxT("nyquist prompt"))
   {
      return true;
   }

   return false;
}

bool EffectNyquist::IsLegacy()
{
   return true;
}

bool EffectNyquist::SupportsRealtime()
{
   return false;
}

bool EffectNyquist::SupportsAutomation()
{
   return true;
}

// ============================================================================
// Effect Implementation
// ============================================================================

bool EffectNyquist::SupportsChains()
{
   return (GetEffectFlags() & PROCESS_EFFECT) != 0;
}

bool EffectNyquist::TransferParameters( Shuttle & shuttle )
{

   for (size_t i = 0; i < mControls.GetCount(); i++) {
      NyqControl *ctrl = &mControls[i];
      double d = ctrl->val;
      bool good = false;

      if (d == UNINITIALIZED_CONTROL) {
         if (ctrl->type != NYQ_CTRL_STRING) {
            if (!shuttle.mbStoreInClient) {
               d = GetCtrlValue(ctrl->valStr);
            }
         }
      }

      if (ctrl->type == NYQ_CTRL_REAL) {
         good = shuttle.TransferDouble(ctrl->var, d, 0.0);
      }
      else if (ctrl->type == NYQ_CTRL_INT) {
         int val = (int) d;
         good = shuttle.TransferInt(ctrl->var, val, 0);
         d = (double) val;
      }
      else if (ctrl->type == NYQ_CTRL_CHOICE) {
         //str is coma separated labels for each choice
         wxString str = ctrl->label;
         wxArrayString choices;

         while (1) {
            int ci = str.Find( ',' ); //coma index

            if (ci == -1) {
               choices.Add( str );
               break;
            }
            else {
               choices.Add(str.Left(ci));
            }

            str = str.Right(str.length() - ci - 1);
         }

         int cnt = choices.GetCount();
         if (choices.GetCount() > 0) {
            wxString *array = NULL;
            array = new wxString[cnt];
            for (int j = 0; j < cnt; j++ ) {
               array[j] = choices[j];
            }

            int val = (int) d;
            good = shuttle.TransferEnum(ctrl->var, val, cnt, array);
            d = (double) val;

            delete [] array;
         }
      }
      else if (ctrl->type == NYQ_CTRL_STRING) {
         good = shuttle.TransferString(ctrl->var, ctrl->valStr, wxEmptyString);
      }

      if (ctrl->type != NYQ_CTRL_STRING) {
         if (shuttle.mbStoreInClient && good) {
            ctrl->val = d;
         }
      }
   }

   return true;
}

bool EffectNyquist::GeneratorPreview()
{
   // Enable Nyquist generator plug-ins to create preview without a selection
   return (mEnablePreview && (GetEffectFlags() & INSERT_EFFECT));
}

bool EffectNyquist::PromptUser()
{
   while (mInteractive) {
      NyquistInputDialog dlog(wxGetTopLevelParent(NULL), -1,
                              _("Nyquist Prompt"),
                              _("Enter Nyquist Command: "),
                              mInputCmd);
      dlog.mVersion = mVersion;
      dlog.CentreOnParent();
      int result = dlog.ShowModal();

      if (result == wxID_CANCEL) {
         return false;
      }

      mVersion = dlog.mVersion;

      /*if (result == eDebugID) {
         mDebug = true;
      }*/
      mDebug = (result == eDebugID);

      // remember exact input in mInputCmd which will appear in the next
      // NyquistInputDialog. Copy to mCmd for possible embedding in
      // "function main() begin ... end":
      mCmd = mInputCmd = dlog.GetCommand();

      // Is this LISP or SAL? Both allow comments. After comments, LISP
      // must begin with "(". Technically, a LISP expression could be a
      // symbol or number or string, etc., but these are not really
      // useful expressions. If the input begins with a symbol, number,
      // or string, etc., it is more likely an erroneous attempt to type
      // a SAL expression (which should probably begin with "return"),
      // so we will treat it as SAL.

      // this is a state machine to scan past LISP comments and white
      // space to find the first real character of LISP or SAL. Note
      // that #| ... |# style comments are not valid in SAL, so we do
      // not skip these. Instead, "#|" indicates LISP if found.
      //
      unsigned int i = 0;
      bool inComment = false; // handle "; ... \n" comments
      while (i < mCmd.Len()) {
         if (inComment) {
            inComment = (mCmd[i] != wxT('\n'));
         } else if (mCmd[i] == wxT(';')) {
            inComment = true;
         } else if (!wxIsspace(mCmd[i])) {
            break; // found the first non-comment, non-space character
         }
         i++;
      }

      // invariant: i == mCmd.Len() |
      //            mCmd[i] is first non-comment, non-space character

      mIsSal = false;
      if (mCmd.Len() > i && mCmd[i] != wxT('(') &&
          (mCmd[i] != wxT('#') || mCmd.Len() <= i + 1 ||
           mCmd[i + 1] != wxT('|'))) {
         mIsSal = true;
         wxString cmdUp = mCmd.Upper();
         int returnLoc = cmdUp.Find(wxT("RETURN"));
         if (returnLoc == wxNOT_FOUND) {
            wxMessageBox(_("Your code looks like SAL syntax, but there is no return statement. Either use a return statement such as\n\treturn s * 0.1\nfor SAL, or begin with an open parenthesis such as\n\t(mult s 0.1)\n for LISP."), _("Error in Nyquist code"), wxOK | wxCENTRE);
            return false;
         }
      }

      if (result != ePreviewID)
      {
         return true;
      }

      Preview();
   }

   if (!mExternal) {
      //TODO: If we want to auto-add parameters from spectral selection,
      //we will need to modify this test.
      //Note that removing it stops the caching of parameter values,
      //(during this session).
      if (mFileName.GetModificationTime().IsLaterThan(mFileModified)) 
      {
         ParseFile();
         mFileModified = mFileName.GetModificationTime();
      }
   }

   if (mControls.GetCount() == 0) {
      return true;
   }

   for (unsigned int i = 0; i < mControls.GetCount(); i++) {
      NyqControl *ctrl = &mControls[i];

      if (ctrl->type == NYQ_CTRL_STRING) {
         continue;
      }

      if (ctrl->val == UNINITIALIZED_CONTROL) {
         ctrl->val = GetCtrlValue(ctrl->valStr);
      }

      if (ctrl->type == NYQ_CTRL_CHOICE) {
         continue;
      }

      ctrl->low = GetCtrlValue(ctrl->lowStr);
      ctrl->high = GetCtrlValue(ctrl->highStr);

      if (ctrl->high < ctrl->low) {
         ctrl->high = ctrl->low + 1;
      }

      if (ctrl->val < ctrl->low) {
         ctrl->val = ctrl->low;
      }

      if (ctrl->val > ctrl->high) {
         ctrl->val = ctrl->high;
      }

      ctrl->ticks = 1000;
      if (ctrl->type == NYQ_CTRL_INT &&
          (ctrl->high - ctrl->low < ctrl->ticks)) {
         ctrl->ticks = (int)(ctrl->high - ctrl->low);
      }
   }

   NyquistDialog dlog(mParent, -1, mName, mInfo, mEnablePreview, this);
   dlog.CentreOnParent();
   int result = dlog.ShowModal();

   if (result == wxID_CANCEL) {
      return false;
   }

   /* if (result == eDebugID) {
      mDebug = true;
   } */
   mDebug = (result == eDebugID);

   return true;
}

bool EffectNyquist::Process()
{
   bool success = true;

   if (mExternal) {
      mProgress->Hide();
   }

   // We must copy all the tracks, because Paste needs label tracks to ensure
   // correct sync-lock group behavior when the timeline is affected; then we just want
   // to operate on the selected wave tracks
   this->CopyInputTracks(Track::All);
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   mCurTrack[0] = (WaveTrack *) iter.First();
   mOutputTime = 0;
   mCount = 0;
   mProgressIn = 0;
   mProgressOut = 0;
   mProgressTot = 0;
   mScale = (GetEffectFlags() & PROCESS_EFFECT ? 0.5 : 1.0) / GetNumWaveGroups();

   mStop = false;
   mBreak = false;
   mCont = false;

   mTrackIndex = 0;

   mDebugOutput = "";

   if (mVersion >= 4)
   {
      AudacityProject *project = GetActiveProject();

      mProps = wxEmptyString;

      mProps += wxString::Format(wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"), AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"), EscapeString(FileNames::BaseDir()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"), EscapeString(FileNames::DataDir()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"), EscapeString(FileNames::HtmlHelpDir().RemoveLast()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"), EscapeString(FileNames::TempDir()).c_str());

      wxArrayString paths = EffectNyquist::GetNyquistSearchPath();
      wxString list;
      for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++)
      {
         list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
      }
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list.RemoveLast().c_str());

#if defined EXPERIMENTAL_NYQUIST_TIME_PROPERTY
      // Date and time:
      wxDateTime now = wxDateTime::Now();
      int year = now.GetYear();
      int doy = now.GetDayOfYear();
      int dom = now.GetDay();
      // enumerated constants
      wxDateTime::Month month = now.GetMonth();
      wxDateTime::WeekDay day = now.GetWeekDay();
      
      // Date/time as a list: year, day of year, hour, minute, seconds
      mProps += wxString::Format(wxT("(setf *SYSTEM-TIME* (list %d %d %d %d %d))\n"),
                                 year, doy, now.GetHour(), now.GetMinute(), now.GetSecond());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DATE)\n"), now.FormatDate().c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'TIME)\n"), now.FormatTime().c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-DATE)\n"), now.FormatISODate().c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-TIME)\n"), now.FormatISOTime().c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'YEAR)\n"), year);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'DAY)\n"), dom);   // day of month
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'MONTH)\n"), month);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'MONTH-NAME)\n"), now.GetMonthName(month).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DAY-NAME)\n"), now.GetWeekDayName(day).c_str());
#endif


      TrackListIterator all(project->GetTracks());
      Track *t;
      int numTracks = 0;
      int numWave = 0;
      int numLabel = 0;
      int numMidi = 0;
      int numTime = 0;
      wxString waveTrackList = wxT("");   // track positions of selected audio tracks.

      for (t = all.First(); t; t = all.Next())
      {
         switch (t->GetKind())
         {
            case Track::Wave:
               numWave++;
               if (t->GetSelected()) {
                  waveTrackList += wxString::Format(wxT("%d "), 1 + numTracks);
               }
            break;
            case Track::Label: numLabel++; break;
#if defined(USE_MIDI)
            case Track::Note: numMidi++; break;
#endif
            case Track::Time: numTime++; break;
            default: break;
         }

         numTracks++;
         if (t->GetLinked())
         {
            all.Next();
         }
      }

      // We use Internat::ToString() rather than "%g" here because we
      // always have to use the dot as decimal separator when giving
      // numbers to Nyquist, whereas using "%g" will use the user's
      // decimal separator which may be a comma in some countries.
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'RATE)\n"),
                                 Internat::ToString(project->GetRate()).c_str());
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TRACKS)\n"), numTracks);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'WAVETRACKS)\n"), numWave);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'LABELTRACKS)\n"), numLabel);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'MIDITRACKS)\n"), numMidi);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TIMETRACKS)\n"), numTime);

      SelectedTrackListOfKindIterator sel(Track::Wave, mOutputTracks);
      int numChannels = 0;
      for (WaveTrack *t = (WaveTrack *) sel.First(); t; t = (WaveTrack *) sel.Next()) {
         numChannels++;
         if (mT1 >= mT0) {
            if (t->GetLinked()) {
               numChannels++;
               sel.Next();
            }
         }
      }

      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'START)\n"),
                                 Internat::ToString(mT0).c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'END)\n"),
                                 Internat::ToString(mT1).c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (list %s) 'TRACKS)\n"), waveTrackList.c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %d 'CHANNELS)\n"), numChannels);

      wxString lowHz = wxT("nil");
      wxString highHz = wxT("nil");
      wxString centerHz = wxT("nil");
      wxString bandwidth = wxT("nil");

#if defined(EXPERIMENTAL_SPECTRAL_EDITING)
      if (mF0 >= 0.0) {
         lowHz.Printf(wxT("(float %s)"), Internat::ToString(mF0).c_str());
      }

      if (mF1 >= 0.0) {
         highHz.Printf(wxT("(float %s)"), Internat::ToString(mF1).c_str());
      }

      if ((mF0 >= 0.0) && (mF1 >= 0.0)) {
         centerHz.Printf(wxT("(float %s)"), Internat::ToString(sqrt(mF0 * mF1)).c_str());
      }

      if ((mF0 > 0.0) && (mF1 >= mF0)) {
         bandwidth.Printf(wxT("(float %s)"), Internat::ToString(log(mF1 / mF0)/log(2.0)).c_str());
      }

#endif
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'LOW-HZ)\n"), lowHz.c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'CENTER-HZ)\n"), centerHz.c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'HIGH-HZ)\n"), highHz.c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'BANDWIDTH)\n"), bandwidth.c_str());

   }

   // Keep track of whether the current track is first selected in its sync-lock group
   // (we have no idea what the length of the returned audio will be, so we have
   // to handle sync-lock group behavior the "old" way).
   mFirstInGroup = true;
   Track *gtLast = NULL;

   while (mCurTrack[0]) {
      mCurNumChannels = 1;
      if (mT1 >= mT0) {
         if (mCurTrack[0]->GetLinked()) {
            mCurNumChannels = 2;

            mCurTrack[1] = (WaveTrack *)iter.Next();
            if (mCurTrack[1]->GetRate() != mCurTrack[0]->GetRate()) {
               wxMessageBox(_("Sorry, cannot apply effect on stereo tracks where the tracks don't match."),
                            wxT("Nyquist"),
                            wxOK | wxCENTRE, mParent);
               success = false;
               goto finish;
            }
            mCurStart[1] = mCurTrack[1]->TimeToLongSamples(mT0);
         }

         // Check whether we're in the same group as the last selected track
         SyncLockedTracksIterator gIter(mOutputTracks);
         Track *gt = gIter.First(mCurTrack[0]);
         mFirstInGroup = !gtLast || (gtLast != gt);
         gtLast = gt;

         mCurStart[0] = mCurTrack[0]->TimeToLongSamples(mT0);
         sampleCount end = mCurTrack[0]->TimeToLongSamples(mT1);
         mCurLen = (sampleCount)(end - mCurStart[0]);

         mProgressIn = 0.0;
         mProgressOut = 0.0;

         // libnyquist breaks except in LC_NUMERIC=="C".
         //
         // Note that we must set the locale to "C" even before calling
         // nyx_init() because otherwise some effects will not work!
         //
         // MB: setlocale is not thread-safe.  Should use uselocale()
         //     if available, or fix libnyquist to be locale-independent.
         // See also http://bugzilla.audacityteam.org/show_bug.cgi?id=642#c9
         // for further info about this thread safety question.
         wxString prevlocale = wxSetlocale(LC_NUMERIC, NULL);
         wxSetlocale(LC_NUMERIC, wxString(wxT("C")));

         nyx_init();
         nyx_set_os_callback(StaticOSCallback, (void *)this);
         nyx_capture_output(StaticOutputCallback, (void *)this);

         success = ProcessOne();

         nyx_capture_output(NULL, (void *)NULL);
         nyx_set_os_callback(NULL, (void *)NULL);
         nyx_cleanup();

         // Reset previous locale
         wxSetlocale(LC_NUMERIC, prevlocale);

         if (!success) {
            goto finish;
         }
         mProgressTot += mProgressIn + mProgressOut;
      }

      mCurTrack[0] = (WaveTrack *) iter.Next();
      mCount += mCurNumChannels;
   }

   if (mOutputTime > 0.0) {
      mT1 = mT0 + mOutputTime;
   }

 finish:

   if (mDebug && !mExternal) {
      NyquistOutputDialog dlog(mParent, -1,
                               _("Nyquist"),
                               _("Nyquist Output: "),
                               NyquistToWxString(mDebugOutput.c_str()));
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   this->ReplaceProcessedTracks(success);

   mDebug = false;

   return success;
}

bool EffectNyquist::ProcessOne()
{
   nyx_rval rval;

   wxString cmd;

   if (mVersion >= 4) {
      nyx_set_audio_name("*TRACK*");
      cmd += wxT("(setf S 0.25)\n");
   }
   else {
      nyx_set_audio_name("S");
      cmd += wxT("(setf *TRACK* '*unbound*)\n");
   }

   if (mVersion >= 4) {
      cmd += mProps;

      // Set the track TYPE and VIEW properties
      wxString type;
      wxString view;
      wxString bitFormat;
      switch (mCurTrack[0]->GetKind())
      {
         case Track::Wave:
            type = wxT("wave");
            switch (((WaveTrack *) mCurTrack[0])->GetDisplay())
            {
               case WaveTrack::WaveformDisplay: view = wxT("\"Waveform\""); break;
               case WaveTrack::WaveformDBDisplay: view = wxT("\"Waveform (dB)\""); break;
               case WaveTrack::SpectrumDisplay: view = wxT("\"Spectrogram\""); break;
               case WaveTrack::SpectrumLogDisplay: view = wxT("\"Spectrogram log(f)\""); break;
               case WaveTrack::PitchDisplay: view = wxT("\"Pitch (EAC)\""); break;
               default: view = wxT("NIL"); break;
            }
         break;
#if defined(USE_MIDI)
         case Track::Note:
            type = wxT("midi");
            view = wxT("\"Midi\"");
         break;
#endif
         case Track::Label:
            type = wxT("label");
            view = wxT("\"Label\"");
         break;
         case Track::Time:
            type = wxT("time");
            view = wxT("\"Time\"");
         break;
      }

      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'INDEX)\n"), ++mTrackIndex);
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'NAME)\n"), mCurTrack[0]->GetName().c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'TYPE)\n"), type.c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view.c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'START-TIME)\n"),
                              Internat::ToString(mCurTrack[0]->GetStartTime()).c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'END-TIME)\n"),
                              Internat::ToString(mCurTrack[0]->GetEndTime()).c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'GAIN)\n"),
                              Internat::ToString(mCurTrack[0]->GetGain()).c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'PAN)\n"),
                              Internat::ToString(mCurTrack[0]->GetPan()).c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'RATE)\n"),
                              Internat::ToString(mCurTrack[0]->GetRate()).c_str());

      switch (mCurTrack[0]->GetSampleFormat())
      {
         case int16Sample:
            bitFormat = wxT("16");
            break;
         case int24Sample:
            bitFormat = wxT("24");
            break;
         case floatSample:
            bitFormat = wxT("32.0");
            break;
      }
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'FORMAT)\n"), bitFormat.c_str());

      float maxPeak = 0.0;
      wxString clips;
      for (int i = 0; i < mCurNumChannels; i++) {
         WaveClipArray ca;
         mCurTrack[i]->FillSortedClipArray(ca);
         // A list of clips for mono, or an array of lists for multi-channel.
         if (mCurNumChannels > 1) clips += wxT("(list ");
         // Each clip is a list (start-time, end-time)
         for (size_t j = 0; j < ca.GetCount(); j++) {
            clips += wxString::Format(wxT("(list (float %s) (float %s))"),
                                      Internat::ToString(ca[j]->GetStartTime()).c_str(),
                                      Internat::ToString(ca[j]->GetEndTime()).c_str());
         }
         if (mCurNumChannels > 1) clips += wxT(" )");

         float min, max;
         mCurTrack[i]->GetMinMax(&min, &max, mT0, mT1);
         maxPeak = wxMax(wxMax(fabs(min), fabs(max)), maxPeak);
      }
      // A list of clips for mono, or an array of lists for multi-channel.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s%s ) 'CLIPS)\n"),
                              (mCurNumChannels == 1) ? wxT("(list ") : wxT("(vector "),
                              clips.c_str());
      cmd += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'PEAK-LEVEL)\n"),
                              Internat::ToString(maxPeak).c_str());
   }

   if (GetEffectFlags() & INSERT_EFFECT) {
      nyx_set_audio_params(mCurTrack[0]->GetRate(), 0);
   }
   else {
      nyx_set_audio_params(mCurTrack[0]->GetRate(), mCurLen);

      nyx_set_input_audio(StaticGetCallback, (void *)this,
                          mCurNumChannels,
                          mCurLen, mCurTrack[0]->GetRate());
   }

   // Restore the Nyquist sixteenth note symbol for Generate plugins.
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=490.
   if (GetEffectFlags() & INSERT_EFFECT) {
      cmd += wxT("(setf s 0.25)\n");
   }

   if (mDebug) {
      cmd += wxT("(setf *tracenable* T)\n");
      if (mExternal) {
         cmd += wxT("(setf *breakenable* T)\n");
      }
   }
   else {
      // Explicitly disable backtrace and prevent values
      // from being carried through to the output.
      // This should be the final command before evaluating the Nyquist script.
      cmd += wxT("(setf *tracenable* NIL)\n");
   }

   for (unsigned int j = 0; j < mControls.GetCount(); j++) {
      if (mControls[j].type == NYQ_CTRL_REAL) {
         // We use Internat::ToString() rather than "%f" here because we
         // always have to use the dot as decimal separator when giving
         // numbers to Nyquist, whereas using "%f" will use the user's
         // decimal separator which may be a comma in some countries.
         cmd += wxString::Format(wxT("(setf %s %s)\n"),
                                 mControls[j].var.c_str(),
                                 Internat::ToString(mControls[j].val, 14).c_str());
      }
      else if (mControls[j].type == NYQ_CTRL_INT ||
            mControls[j].type == NYQ_CTRL_CHOICE) {
         cmd += wxString::Format(wxT("(setf %s %d)\n"),
                                 mControls[j].var.c_str(),
                                 (int)(mControls[j].val));
      }
      else if (mControls[j].type == NYQ_CTRL_STRING) {
         cmd += wxT("(setf ");
         // restrict variable names to 7-bit ASCII:
         cmd += mControls[j].var.c_str();
         cmd += wxT(" \"");
         cmd += EscapeString(mControls[j].valStr); // unrestricted value will become quoted UTF-8
         cmd += wxT("\")\n");
      }
   }

   if (mIsSal) {
      wxString str = mCmd;
      EscapeString(str);
      // this is tricky: we need SAL to call main so that we can get a
      // SAL traceback in the event of an error (sal-compile catches the
      // error and calls sal-error-output), but SAL does not return values.
      // We will catch the value in a special global aud:result and if no
      // error occurs, we will grab the value with a LISP expression
      str += wxT("\nset aud:result = main()\n");

      if (mDebug) {
         // since we're about to evaluate SAL, remove LISP trace enable and
         // break enable (which stops SAL processing) and turn on SAL stack
         // trace
         cmd += wxT("(setf *tracenable* nil)\n");
         cmd += wxT("(setf *breakenable* nil)\n");
         cmd += wxT("(setf *sal-traceback* t)\n");
      }

      if (mCompiler) {
         cmd += wxT("(setf *sal-compiler-debug* t)\n");
      }

      cmd += wxT("(setf *sal-call-stack* nil)\n");
      // if we do not set this here and an error occurs in main, another
      // error will be raised when we try to return the value of aud:result
      // which is unbound
      cmd += wxT("(setf aud:result nil)\n");
      cmd += wxT("(sal-compile-audacity \"") + str + wxT("\" t t nil)\n");
      // Capture the value returned by main (saved in aud:result), but
      // set aud:result to nil so sound results can be evaluated without
      // retaining audio in memory
      cmd += wxT("(prog1 aud:result (setf aud:result nil))\n");
   }
   else {
      cmd += mCmd;
   }

   int i;
   for (i = 0; i < mCurNumChannels; i++) {
      mCurBuffer[i] = NULL;
   }

   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   if (!rval) {
      wxLogWarning(wxT("Nyquist returned NIL"));
      return true;
   }

   if (rval == nyx_string) {
      wxMessageBox(NyquistToWxString(nyx_get_string()),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);

      // True if not process type.
      // If not returning audio from process effect,
      // return first reult then stop (disables preview)
      // but allow all output from Nyquist Prompt.
      return (!(GetEffectFlags() & PROCESS_EFFECT)|| mInteractive);
   }

   if (rval == nyx_double) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %f")),
                 nyx_get_double());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return (!(GetEffectFlags() & PROCESS_EFFECT)|| mInteractive);
   }

   if (rval == nyx_int) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %d")),
                 nyx_get_int());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return (!(GetEffectFlags() & PROCESS_EFFECT)|| mInteractive);
   }

   if (rval == nyx_labels) {
      unsigned int numLabels = nyx_get_num_labels();
      unsigned int l;
      LabelTrack *ltrack = NULL;

      TrackListIterator iter(mOutputTracks);
      for (Track *t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label) {
            ltrack = (LabelTrack *)t;
            break;
         }
      }

      if (!ltrack) {
         ltrack = mFactory->NewLabelTrack();
         this->AddToOutputTracks((Track *)ltrack);
      }

      for (l = 0; l < numLabels; l++) {
         double t0, t1;
         const char *str;

         // PRL:  to do:
         // let Nyquist analyzers define more complicated selections
         nyx_get_label(l, &t0, &t1, &str);

         ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str));
      }
      return (!(GetEffectFlags() & PROCESS_EFFECT)|| mInteractive);
   }

   if (rval != nyx_audio) {
      // This should not happen, but leaving in for now just in case (Dec 2014)
      wxMessageBox(_("Undefined return value.\n"), wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return false;
   }

   int outChannels;

   outChannels = nyx_get_audio_num_channels();
   if (outChannels > mCurNumChannels) {
      wxMessageBox(_("Nyquist returned too many audio channels.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return false;
   }

   if (outChannels == -1) {
      wxMessageBox(_("Nyquist returned one audio channel as an array.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return false;
   }

   if (outChannels == 0) {
      wxMessageBox(_("Nyquist returned an empty array.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      return false;
   }

   double rate = mCurTrack[0]->GetRate();
   for (i = 0; i < outChannels; i++) {
      sampleFormat format = mCurTrack[i]->GetSampleFormat();

      if (outChannels == mCurNumChannels) {
         rate = mCurTrack[i]->GetRate();
      }

      mOutputTrack[i] = mFactory->NewWaveTrack(format, rate);
      mCurBuffer[i] = NULL;
   }

   int success = nyx_get_audio(StaticPutCallback, (void *)this);

   if (!success) {
      for(i = 0; i < outChannels; i++) {
         delete mOutputTrack[i];
         mOutputTrack[i] = NULL;
      }
      return false;
   }

   for (i = 0; i < outChannels; i++) {
      mOutputTrack[i]->Flush();
      if (mCurBuffer[i]) {
         DeleteSamples(mCurBuffer[i]);
      }
      mOutputTime = mOutputTrack[i]->GetEndTime();

      if (mOutputTime <= 0) {
         wxMessageBox(_("Nyquist did not return audio.\n"),
                      wxT("Nyquist"),
                      wxOK | wxCENTRE, mParent);
         for (i = 0; i < outChannels; i++) {
            delete mOutputTrack[i];
            mOutputTrack[i] = NULL;
         }
         return true;
      }
   }

   for (i = 0; i < mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == mCurNumChannels) {
         out = mOutputTrack[i];
      }
      else {
         out = mOutputTrack[0];
      }

      if (mMergeClips < 0) {
         // Use sample counts to determine default behaviour - times will rarely be equal.
         bool bMergeClips = (out->TimeToLongSamples(mT0) + out->TimeToLongSamples(mOutputTime) == 
                                                                     out->TimeToLongSamples(mT1));
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, bMergeClips);
      }
      else {
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, mMergeClips);
      }
         
      // If we were first in the group adjust non-selected group tracks
      if (mFirstInGroup) {
         SyncLockedTracksIterator git(mOutputTracks);
         Track *t;
         for (t = git.First(mCurTrack[i]); t; t = git.Next())
         {
            if (!t->GetSelected() && t->IsSyncLockSelected()) {
               t->SyncLockAdjust(mT1, mT0 + out->GetEndTime());
            }
         }
      }

      // Only the first channel can be first in its group
      mFirstInGroup = false;
   }

   for (i = 0; i < outChannels; i++) {
      delete mOutputTrack[i];
      mOutputTrack[i] = NULL;
   }

   return true;
}

// ============================================================================
// EffectNyquist Implementation
// ============================================================================

wxString EffectNyquist::NyquistToWxString(const char *nyqString)
{
    wxString str(nyqString, wxConvUTF8);
    if (nyqString != NULL && nyqString[0] && str.IsEmpty()) {
        // invalid UTF-8 string, convert as Latin-1
        str = _("[Warning: Nyquist returned invalid UTF-8 string, converted here as Latin-1]");
        str += LAT1CTOWX(nyqString);
    }
    return str;
}

wxString EffectNyquist::EscapeString(const wxString & inStr)
{
   wxString str = inStr;

   str.Replace(wxT("\\"), wxT("\\\\"));
   str.Replace(wxT("\""), wxT("\\\""));

   return str;
}

void EffectNyquist::Break()
{
   mBreak = true;
}

void EffectNyquist::Continue()
{
   mCont = true;
}

void EffectNyquist::Stop()
{
   mStop = true;
}

wxString EffectNyquist::UnQuote(wxString s)
{
   wxString out;
   int len = s.Length();

   if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
      return s.Mid(1, len - 2);
   }

   return s;
}

double EffectNyquist::GetCtrlValue(wxString s)
{
   if (s == wxT("rate")) {
      TrackListOfKindIterator iter(Track::Wave, mTracks);
      return ((WaveTrack *)iter.First())->GetRate();
   }

   return Internat::CompatibleToDouble(s);
}

void EffectNyquist::Parse(wxString line)
{
   wxArrayString tokens;

   int i;
   int len = line.Length();
   bool sl = false;
   bool q = false;
   wxString tok = wxT("");

   for (i = 1; i < len; i++) {
      wxChar c = line[i];

      if (c == wxT('\\')) {
         sl = true;
      }
      else if (c == wxT('"')) {
         q = !q;
      }
      else {
         if ((!q && !sl && c == wxT(' ')) || c == wxT('\t')) {
            tokens.Add(tok);
            tok = wxT("");
         }
         else if (sl && c == wxT('n')) {
            tok += wxT('\n');
         }
         else {
            tok += c;
         }

         sl = false;
      }
   }

   if (tok != wxT("")) {
      tokens.Add(tok);
   }

   len = tokens.GetCount();
   if (len < 1) {
      return;
   }

   // As of version 4 plugins ";nyquist plug-in" is depricated in favour of ";nyquist plugin".
   // The hyphenated version must be maintained while we support plugin versions < 4.
   if (len == 2 && tokens[0] == wxT("nyquist") &&
      (tokens[1] == wxT("plugin") || tokens[1] == wxT("plug-in"))) {
      mOK = true;
      return;
   }

   if (len >= 2 && tokens[0] == wxT("type")) {
      if (tokens[1] == wxT("process")) {
         SetEffectFlags(PROCESS_EFFECT | PLUGIN_EFFECT);
      }
      else if (tokens[1] == wxT("generate")) {
         SetEffectFlags(INSERT_EFFECT | PLUGIN_EFFECT);
      }
      else if (tokens[1] == wxT("analyze")) {
         SetEffectFlags(ANALYZE_EFFECT | PLUGIN_EFFECT);
      }
      return;
   }

   if (len == 2 && tokens[0] == wxT("codetype")) {
      if (tokens[1] == wxT("lisp")) {
         mIsSal = false;
      }
      else if (tokens[1] == wxT("sal")) {
         mIsSal = true;
      }
      return;
   }

   if (len >= 2 && tokens[0] == wxT("debugflags")) {
      for (int i = 1; i < len; i++) {
         // Note: "trace" and "notrace" are overridden by "Debug" and "OK"
         // buttons if the plug-in generates a dialog box by using controls
         if (tokens[i] == wxT("trace")) {
            mDebug = true;
         }
         else if (tokens[i] == wxT("notrace")) {
            mDebug = false;
         }
         else if (tokens[i] == wxT("compiler")) {
            mCompiler = true;
         }
         else if (tokens[i] == wxT("nocompiler")) {
            mCompiler = false;
         }
      }
      return;
   }

   // We support versions 1, 2 and 3
   // (Version 2 added support for string parameters.)
   // (Version 3 added support for choice parameters.)
   // (Version 4 added support for project/track/selection information.)
   if (len >= 2 && tokens[0] == wxT("version")) {
      long v;
      tokens[1].ToLong(&v);
      if (v < 1 && v > 4) {
         // This is an unsupported plug-in version
         mOK = false;
         return;
      }
      mVersion = (int) v;
   }

   if (len >= 2 && tokens[0] == wxT("name")) {
      mName = UnQuote(tokens[1]);
      return;
   }

   if (len >= 2 && tokens[0] == wxT("action")) {
      mAction = UnQuote(tokens[1]);
      return;
   }

   if (len >= 2 && tokens[0] == wxT("info")) {
      mInfo = UnQuote(tokens[1]);
      return;
   }

   if (len >= 2 && tokens[0] == wxT("preview")) {
      if (tokens[1] == wxT("enabled") || tokens[1] == wxT("true")) {
         mEnablePreview = true;
      }
      return;
   }

#if defined(EXPERIMENTAL_NYQUIST_SPLIT_CONTROL)
   if (len >= 2 && tokens[0] == wxT("mergeclips")) {
      long v;
      // -1 = auto (default), 0 = don't merge clips, 1 = do merge clips
      tokens[1].ToLong(&v);
      mMergeClips = v;
      return;
   }

   if (len >= 2 && tokens[0] == wxT("restoresplits")) {
      long v;
      // Splits are restored by default. Set to 0 to prevent.
      tokens[1].ToLong(&v);
      mRestoreSplits = v;
      return;
   }
#endif

   if (len >= 2 && tokens[0] == wxT("author")) {
      mAuthor = UnQuote(tokens[1]);
      return;
   }

   if (len >= 2 && tokens[0] == wxT("copyright")) {
      mCopyright = UnQuote(tokens[1]);
      return;
   }

   if (len >= 6 && tokens[0] == wxT("control")) {
      NyqControl ctrl;

      ctrl.var = tokens[1];
      ctrl.name = tokens[2];
      ctrl.label = tokens[4];
      ctrl.valStr = tokens[5];

      if (tokens[3] == wxT("string")) {
         ctrl.type = NYQ_CTRL_STRING;
      }
      else if (tokens[ 3 ] == wxT("choice")) {
         ctrl.type = NYQ_CTRL_CHOICE;
      }
      else {
         if (len < 8) {
            return;
         }

         if ((tokens[3] == wxT("real")) ||
               (tokens[3] == wxT("float"))) // undocumented, but useful, alternative
            ctrl.type = NYQ_CTRL_REAL;
         else if (tokens[3] == wxT("int"))
            ctrl.type = NYQ_CTRL_INT;
         else
         {
            wxString str;
            str.Printf(_("Bad Nyquist 'control' type specification: '%s' in plugin file '%s'.\nControl not created."),
                       tokens[3].c_str(), mFileName.GetFullPath().c_str());

            // Too disturbing to show alert before Audacity frame is up.
            //    wxMessageBox(str, wxT("Nyquist Warning"), wxOK | wxICON_EXCLAMATION);

            // Note that the AudacityApp's mLogger has not yet been created,
            // so this brings up an alert box, but after the Audacity frame is up.
            wxLogWarning(str);
            return;
         }

         ctrl.lowStr = tokens[6];
         ctrl.highStr = tokens[7];
      }

      ctrl.val = UNINITIALIZED_CONTROL;

      if( mPresetNames.Index( ctrl.var ) == wxNOT_FOUND )
      {
         mControls.Add(ctrl);
      }
   }

   if (len >= 2 && tokens[0] == wxT("categories")) {
      for (size_t i = 1; i < tokens.GetCount(); ++i) {
         mCategories.Add(tokens[i]);
      }
   }
}

void EffectNyquist::ParseFile()
{
   wxTextFile f(mFileName.GetFullPath());
   if (!f.Open())
      return;

   mCmd = wxT("");
   SetEffectFlags(PROCESS_EFFECT | PLUGIN_EFFECT);
   mOK = false;
   mEnablePreview = false;
   mIsSal = false;
   mControls.Clear();
   mDebug = false;

   int i;
   int len = f.GetLineCount();
   wxString line;
   for (i = 0; i < len; i++) {
      line = f[i];
      if (line.Length() > 1 && line[0] == wxT(';')) {
         Parse(line);
      }
      // preserve comments so that SAL effects compile with proper line numbers
      mCmd += line + wxT("\n");
   }
}

void EffectNyquist::SetCommand(wxString cmd)
{
   mExternal = true;
   mInteractive = false;
   mCmd = wxT("");
   SetEffectFlags(INSERT_EFFECT | HIDDEN_EFFECT);
   mOK = false;
   mIsSal = false;
   mControls.Clear();

   wxStringTokenizer lines(cmd, wxT("\n"));
   while (lines.HasMoreTokens()) {
      wxString line = lines.GetNextToken();

      if (line.Length() > 1 && line[0] == wxT(';')) {
         Parse(line);
      }
      else {
         mCmd += line + wxT("\n");
      }
   }
}

bool EffectNyquist::SetXlispPath()
{
   wxString fname;

   fname = mXlispPath + wxFILE_SEP_PATH + wxT("nyinit.lsp");
   if (!(::wxFileExists(fname))) {
      mXlispPath = wxT("");
   }

   if (mXlispPath == wxT("")) {
      wxArrayString audacityPathList = wxGetApp().audacityPathList;
      wxArrayString pathList;
      wxArrayString files;
      unsigned int i;

      for (i = 0; i < audacityPathList.GetCount(); i++) {
         wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
         wxGetApp().AddUniquePathToPathList(prefix + wxT("nyquist"),
                                            pathList);
      }

      wxGetApp().FindFilesInPathList(wxT("nyquist.lsp"), pathList, files);

      if (files.GetCount() > 0) {
         mXlispPath = ::wxPathOnly(files[0]);
      }
   }

   /* set_xlisp_path doesn't handle fn_Str() in Unicode build. May or may not actually work. */
   nyx_set_xlisp_path(mXlispPath.mb_str());

   fname = mXlispPath + wxFILE_SEP_PATH + wxT("nyinit.lsp");
   return ::wxFileExists(fname);
}

int EffectNyquist::StaticGetCallback(float *buffer, int channel,
                                     long start, long len, long totlen,
                                     void *userdata)
{
   EffectNyquist *This = (EffectNyquist *)userdata;
   return This->GetCallback(buffer, channel, start, len, totlen);
}

int EffectNyquist::GetCallback(float *buffer, int ch,
                               long start, long len, long WXUNUSED(totlen))
{
   if (mCurBuffer[ch]) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         delete[] mCurBuffer[ch];
         mCurBuffer[ch] = NULL;
      }
   }

   if (!mCurBuffer[ch]) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

      if (mCurBufferLen[ch] < len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      if (mCurBufferStart[ch] + mCurBufferLen[ch] > mCurStart[ch] + mCurLen) {
         mCurBufferLen[ch] = mCurStart[ch] + mCurLen - mCurBufferStart[ch];
      }

      mCurBuffer[ch] = NewSamples(mCurBufferLen[ch], floatSample);
      if (!mCurTrack[ch]->Get(mCurBuffer[ch], floatSample,
                              mCurBufferStart[ch], mCurBufferLen[ch])) {

         wxPrintf(wxT("GET error\n"));

         return -1;
      }
   }

   long offset = (mCurStart[ch] + start) - mCurBufferStart[ch];
   CopySamples(mCurBuffer[ch] + offset*SAMPLE_SIZE(floatSample), floatSample,
               (samplePtr)buffer, floatSample,
               len);

   if (ch == 0) {
      double progress = mScale*(((float)start+len)/mCurLen);

      if (progress > mProgressIn) {
         mProgressIn = progress;
      }

      if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
         return -1;
      }
   }

   return 0;
}

int EffectNyquist::StaticPutCallback(float *buffer, int channel,
                                     long start, long len, long totlen,
                                     void *userdata)
{
   EffectNyquist *This = (EffectNyquist *)userdata;
   return This->PutCallback(buffer, channel, start, len, totlen);
}

int EffectNyquist::PutCallback(float *buffer, int channel,
                               long start, long len, long totlen)
{
   if (channel == 0) {
      double progress = mScale*((float)(start+len)/totlen);

      if (progress > mProgressOut) {
         mProgressOut = progress;
      }

      if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
         return -1;
      }
   }

   if (mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len)) {
      return 0;  // success
   }

   return -1; // failure
}

void EffectNyquist::StaticOutputCallback(int c, void *This)
{
   ((EffectNyquist *)This)->OutputCallback(c);
}

void EffectNyquist::OutputCallback(int c)
{
   if (mDebug && !mExternal) {
      mDebugOutput += (char)c;
      return;
   }
   std::cout << (char)c;
}

void EffectNyquist::StaticOSCallback(void *This)
{
   ((EffectNyquist *)This)->OSCallback();
}

void EffectNyquist::OSCallback()
{
   if (mStop) {
      mStop = false;
      nyx_stop();
   }
   else if (mBreak) {
      mBreak = false;
      nyx_break();
   }
   else if (mCont) {
      mCont = false;
      nyx_continue();
   }

   // LLL:  STF figured out that yielding while the effect is being applied
   //       produces an EXTREME slowdown.  It appears that yielding is not
   //       really necessary on Linux and Windows.
   //
   //       However, on the Mac, the spinning cursor appears during longer
   //       Nyquist processing and that may cause the user to think Audacity
   //       has crashed or hung.  In addition, yielding or not on the Mac
   //       doesn't seem to make much of a difference in execution time.
   //
   //       So, yielding on the Mac only...
#if defined(__WXMAC__)
   wxYieldIfNeeded();
#endif
}

wxArrayString EffectNyquist::GetNyquistSearchPath()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;

   for (size_t i = 0; i < audacityPathList.GetCount(); i++)
   {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
   }

   return pathList;
}

///////////////////////////////////////////////////////////////////////////////
//
// NyquistDialog
//
///////////////////////////////////////////////////////////////////////////////

#define ID_NYQ_SLIDER 2000
#define ID_NYQ_TEXT   3000
#define ID_NYQ_CHOICE 4000

BEGIN_EVENT_TABLE(NyquistDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, NyquistDialog::OnCancel)
   EVT_BUTTON(eDebugID, NyquistDialog::OnDebug)
   EVT_BUTTON(ePreviewID, NyquistDialog::OnPreview)
   EVT_COMMAND_RANGE(ID_NYQ_SLIDER, ID_NYQ_SLIDER+99,
                     wxEVT_COMMAND_SLIDER_UPDATED, NyquistDialog::OnSlider)
   EVT_COMMAND_RANGE(ID_NYQ_TEXT, ID_NYQ_TEXT+99,
                      wxEVT_COMMAND_TEXT_UPDATED, NyquistDialog::OnText)
   EVT_COMMAND_RANGE( ID_NYQ_CHOICE, ID_NYQ_CHOICE + 99,
                     wxEVT_COMMAND_CHOICE_SELECTED, NyquistDialog::OnChoice )
END_EVENT_TABLE()

NyquistDialog::NyquistDialog(wxWindow * parent, wxWindowID id,
                             const wxString & title,
                             wxString info,
                             bool preview,
                             EffectNyquist *effect)
:   wxDialog(parent, id, title)
{
   mEffect = effect;
   mControls = &mEffect->mControls;
   mInHandler = true; // prevents race condition on MSW

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   if (info.Length() > 0) {
      wxControl  *item;
      item = new wxStaticText(this, -1, info);
      item->SetName(info);    // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);
   }

   wxFlexGridSizer *grid = new wxFlexGridSizer(4, 0, 0);

   for (size_t i = 0; i < mControls->GetCount(); i++) {
      wxControl  *item;
      NyqControl *ctrl = &((*mControls)[i]);

      item = new wxStaticText(this, -1, ctrl->name + wxT(":"));
      item->SetName(item->GetLabel());    // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      grid->Add(item, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

      if (ctrl->type == NYQ_CTRL_STRING) {
         grid->Add(10, 10);

         item = new wxTextCtrl(this, ID_NYQ_TEXT+i, ctrl->valStr,
                               wxDefaultPosition, wxSize(150, -1));
         item->SetName(ctrl->name);
         grid->Add(item, 0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
      else if (ctrl->type == NYQ_CTRL_CHOICE) {
         //str is coma separated labels for each choice
         wxString str = ctrl->label;
         wxArrayString choices;

         while (1) {
            int ci = str.Find( ',' ); //coma index

            if (ci == -1) {
               choices.Add( str );
               break;
            }
            else {
               choices.Add(str.Left(ci));
            }

            str = str.Right(str.length() - ci - 1);
         }

         wxChoice *choice = new wxChoice(this, ID_NYQ_CHOICE + i,
               wxDefaultPosition, wxSize(150, -1), choices);
         choice->SetName(ctrl->name);

         int val = (int)ctrl->val;
         if (val >= 0 && val < (int)choice->GetCount()) {
            choice->SetSelection(val);
         }

         grid->Add(10, 10);
         grid->Add(choice, 0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
      else {
         // Integer or Real
         int val = (int)(0.5 + ctrl->ticks * (ctrl->val - ctrl->low) /
               (ctrl->high - ctrl->low));

         item = new wxTextCtrl(this, ID_NYQ_TEXT+i, wxT(""),
                               wxDefaultPosition, wxSize(60, -1));
         item->SetName(ctrl->name);
         if (ctrl->type == NYQ_CTRL_REAL) {
            // > 12 decimal places can cause rounding errors in display.
            FloatingPointValidator<double> vld(12, &ctrl->val);
            vld.SetRange(-FLT_MAX, FLT_MAX);
            // Set number of decimal places
            if (ctrl->high - ctrl->low < 10) {
               vld.SetStyle(NUM_VAL_THREE_TRAILING_ZEROES);
            } else if (ctrl->high - ctrl->low < 100) {
               vld.SetStyle(NUM_VAL_TWO_TRAILING_ZEROES);
            } else {
               vld.SetStyle(NUM_VAL_ONE_TRAILING_ZERO);
            }
            item->SetValidator(vld);
         }
         else {
            IntegerValidator<double> vld(&ctrl->val);
            vld.SetRange(INT_MIN, INT_MAX);
            item->SetValidator(vld);
         }

         grid->Add(item, 0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 5);

         item = new wxSlider(this, ID_NYQ_SLIDER+i, val, 0, ctrl->ticks,
                             wxDefaultPosition, wxSize(150, -1));
         item->SetName(ctrl->name);

         grid->Add(item, 0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }

      if (ctrl->type == NYQ_CTRL_CHOICE) {
         grid->Add( 10, 10 );
      }
      else {
         item = new wxStaticText(this, -1, ctrl->label);
         item->SetName(ctrl->label);   // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
         grid->Add(item, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
   }
   mainSizer->Add(grid, 0, wxALIGN_CENTRE | wxALL, 5);

   if (preview) {
      mainSizer->Add(CreateStdButtonSizer(this, ePreviewButton | eDebugButton | eCancelButton | eOkButton),
                     0,
                     wxEXPAND);
   } else {
      mainSizer->Add(CreateStdButtonSizer(this, eDebugButton | eCancelButton | eOkButton),
                     0,
                     wxEXPAND);
   }

   mInHandler = false;

   wxCommandEvent dummy;
   OnSlider(dummy);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

// ============================================================================
// NyquistDialog implementation
// ============================================================================

void NyquistDialog::OnSlider(wxCommandEvent & /* event */)
{
   if (mInHandler) {
      return; // prevent recursing forever
   }

   mInHandler = true;

   for (size_t i = 0; i < mControls->GetCount(); i++) {
      NyqControl *ctrl = &((*mControls)[i]);

      if (ctrl->type == NYQ_CTRL_STRING || ctrl->type == NYQ_CTRL_CHOICE) {
         continue;
      }

      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + i);
      wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + i);
      wxASSERT(slider && text);

      int val = slider->GetValue();

      double newVal = (val / (double)ctrl->ticks)*
         (ctrl->high - ctrl->low) + ctrl->low;

      // Determine precision for displayed number
      int precision = ctrl->high - ctrl->low < 1 ? 3 :
                      ctrl->high - ctrl->low < 10 ? 2 :
                      ctrl->high - ctrl->low < 100 ? 1 :
                      0;

      // If the value is at least one tick different from the current value
      // change it (this prevents changes from manually entered values unless
      // the slider actually moved)
      if (fabs(newVal - ctrl->val) >= (1 / (double)ctrl->ticks) *
                                      (ctrl->high - ctrl->low) &&
          fabs(newVal - ctrl->val) >= pow(0.1, precision) / 2 )
      {
         // First round to the appropriate precision
         newVal *= pow(10.0, precision);
         newVal = floor(newVal + 0.5);
         newVal /= pow(10.0, precision);

         ctrl->val = newVal;

         text->GetValidator()->TransferToWindow();
      }
   }

   mInHandler = false;
}

void NyquistDialog::OnChoice( wxCommandEvent &event )
{
   if (mInHandler) {
      return; // prevent recursing forever
   }
   mInHandler = true;

   unsigned int ctrlId = event.GetId() - ID_NYQ_CHOICE;
   wxASSERT(ctrlId >= 0 && ctrlId < mControls->GetCount());

   NyqControl *ctrl = &(mControls->Item(ctrlId));
   wxChoice *choice = (wxChoice *)FindWindow(ID_NYQ_CHOICE + ctrlId);
   wxASSERT(choice);

   ctrl->val = choice->GetSelection();

   mInHandler = false;
}

void NyquistDialog::OnText(wxCommandEvent &event)
{
   if (mInHandler) {
      return; // prevent recursing forever
   }

   mInHandler = true;

   unsigned int ctrlId = event.GetId() - ID_NYQ_TEXT;
   wxASSERT(ctrlId >= 0 && ctrlId < mControls->GetCount());

   NyqControl *ctrl = &((*mControls)[ctrlId]);
   wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + ctrlId);
   wxASSERT(text);

   if (ctrl->type == NYQ_CTRL_STRING) {
      ctrl->valStr = text->GetValue();
   } else {
      text->GetValidator()->TransferFromWindow();
      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + ctrlId);
      wxASSERT(slider);

      int pos = (int)floor((ctrl->val - ctrl->low) /
                           (ctrl->high - ctrl->low) * ctrl->ticks + 0.5);
      if (pos < 0) {
         pos = 0;
      }
      else if (pos > ctrl->ticks) {
         pos = ctrl->ticks;
      }

      slider->SetValue(pos);
   }

   mInHandler = false;
}

void NyquistDialog::OnOk(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(wxID_OK);
}

void NyquistDialog::OnCancel(wxCommandEvent & /* event */)
{
   EndModal(wxID_CANCEL);
}

void NyquistDialog::OnDebug(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(eDebugID);
}

void NyquistDialog::OnPreview(wxCommandEvent & /* event */)
{
   mEffect->Preview();
}

///////////////////////////////////////////////////////////////////////////////
//
// NyquistInputDialog
//
///////////////////////////////////////////////////////////////////////////////

#define ID_VERSION 1001

BEGIN_EVENT_TABLE(NyquistInputDialog, wxDialog)
   EVT_CHECKBOX(ID_VERSION, NyquistInputDialog::OnVersionCheck)
   EVT_BUTTON(wxID_OK, NyquistInputDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, NyquistInputDialog::OnCancel)
   EVT_BUTTON(eDebugID, NyquistInputDialog::OnDebug)
   EVT_BUTTON(ePreviewID, NyquistInputDialog::OnPreview)
END_EVENT_TABLE()

NyquistInputDialog::NyquistInputDialog(wxWindow * parent, wxWindowID id,
                                       const wxString & title,
                                       const wxString & prompt,
                                       wxString initialCommand)
:  wxDialog(parent, id, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   // Recover last used setting.
   gPrefs->Read(wxT("/Effects/NyquistPrompt/Version"), &mVersion, 4);

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
          S.AddVariableText(prompt);
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxEXPAND, 1);
      {
          mCommandText = S.AddTextWindow(initialCommand);
          mCommandText->SetMinSize(wxSize(500, 200));
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxEXPAND, 0);
      {
         mVersionCheckBox = S.Id(ID_VERSION).AddCheckBox(_("&Use legacy (version 3) syntax."),
                                    (mVersion == 3) ? wxT("true") : wxT("false"));
        
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   // Debug, OK, & Cancel buttons
   S.AddStandardButtons(ePreviewButton|eDebugButton|eCancelButton|eOkButton);

   GetSizer()->SetSizeHints(this);

   mCommandText->SetFocus();
}

// ============================================================================
// NyquistInputDialog implementation
// ============================================================================
 
wxString NyquistInputDialog::GetCommand()
{
   return mCommandText->GetValue();
}

void NyquistInputDialog::OnVersionCheck(wxCommandEvent& WXUNUSED(evt))
{
   mVersion = (mVersionCheckBox->GetValue()) ? 3 : 4;
   gPrefs->Write(wxT("/Effects/NyquistPrompt/Version"), mVersion);
   gPrefs->Flush();
}

void NyquistInputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
}

void NyquistInputDialog::OnCancel(wxCommandEvent & /* event */)
{
   EndModal(wxID_CANCEL);
}

void NyquistInputDialog::OnDebug(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(eDebugID);
}

void NyquistInputDialog::OnPreview(wxCommandEvent & /* event */)
{
   EndModal(ePreviewID);
}


///////////////////////////////////////////////////////////////////////////////
//
// NyquistOutputDialog
//
///////////////////////////////////////////////////////////////////////////////


BEGIN_EVENT_TABLE(NyquistOutputDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistOutputDialog::OnOk)
END_EVENT_TABLE()

NyquistOutputDialog::NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                                       const wxString & title,
                                       const wxString & prompt,
                                       wxString message)
:  wxDialog(parent, id, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hSizer;
   wxButton   *button;
   wxControl  *item;

   item = new wxStaticText(this, -1, prompt);
   item->SetName(prompt);  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   mainSizer->Add(item, 0, wxALIGN_LEFT | wxLEFT | wxTOP | wxRIGHT, 10);

   // TODO use ShowInfoDialog() instead.
   // Beware this dialog MUST work with screen readers.
   item = new wxTextCtrl(this, -1, message,
                         wxDefaultPosition, wxSize(400, 200),
                         wxTE_MULTILINE | wxTE_READONLY);
   mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 10);

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   /* i18n-hint: In most languages OK is to be translated as OK.  It appears on a button.*/
   button = new wxButton(this, wxID_OK, _("OK"));
   button->SetDefault();
   hSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxLEFT | wxBOTTOM | wxRIGHT, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

// ============================================================================
// NyquistOutputDialog implementation
// ============================================================================

void NyquistOutputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
}

