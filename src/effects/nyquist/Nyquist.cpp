/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class NyquistEffect
\brief An Effect that calls up a Nyquist (XLISP) plug in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class NyquistOutputDialog
\brief Dialog used with NyquistEffect

*//****************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/

#include "../../Audacity.h"

#include <algorithm>
#include <cmath>

#include <locale.h>

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/datetime.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/sstream.h>
#include <wx/textdlg.h>
#include <wx/txtstrm.h>
#include <wx/valgen.h>
#include <wx/wfstream.h>
#include <wx/numformatter.h>

#include "../../AudacityApp.h"
#include "../../FileNames.h"
#include "../../Internat.h"
#include "../../LabelTrack.h"
#include "../../prefs/SpectrogramSettings.h"
#include "../../Project.h"
#include "../../ShuttleGui.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../widgets/valnum.h"
#include "../../Prefs.h"
#include "../../prefs/WaveformSettings.h"

#include "FileDialog.h"

#include "Nyquist.h"

#ifndef nyx_returns_start_and_end_time
#error You need to update lib-src/libnyquist
#endif

#include <locale.h>
#include <iostream>
#include <ostream>
#include <sstream>
#include <float.h>

#include "../../Experimental.h"

enum
{
   ID_Editor = 10000,
   ID_Version,
   ID_Load,
   ID_Save,

   ID_Slider = 11000,
   ID_Text = 12000,
   ID_Choice = 13000
};

// Protect Nyquist from selections greater than 2^31 samples (bug 439)
#define NYQ_MAX_LEN (std::numeric_limits<long>::max())

#define UNINITIALIZED_CONTROL ((double)99999999.99)

static const wxChar *KEY_Version = XO("Version");
static const wxChar *KEY_Command = XO("Command");

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffect
//
///////////////////////////////////////////////////////////////////////////////

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(NyqControlArray);

BEGIN_EVENT_TABLE(NyquistEffect, wxEvtHandler)
   EVT_BUTTON(ID_Load, NyquistEffect::OnLoad)
   EVT_BUTTON(ID_Save, NyquistEffect::OnSave)

   EVT_COMMAND_RANGE(ID_Slider, ID_Slider+99,
                     wxEVT_COMMAND_SLIDER_UPDATED, NyquistEffect::OnSlider)
   EVT_COMMAND_RANGE(ID_Text, ID_Text+99,
                     wxEVT_COMMAND_TEXT_UPDATED, NyquistEffect::OnText)
   EVT_COMMAND_RANGE(ID_Choice, ID_Choice + 99,
                     wxEVT_COMMAND_CHOICE_SELECTED, NyquistEffect::OnChoice)
END_EVENT_TABLE()

NyquistEffect::NyquistEffect(const wxString &fName)
{
   mAction = _("Applying Nyquist Effect...");
   mInputCmd = wxEmptyString;
   mCmd = wxEmptyString;
   mIsPrompt = false;
   mExternal = false;
   mCompiler = false;
   mRedirectOutput = false;
   mDebug = false;
   mIsSal = false;
   mOK = false;
   mAuthor = XO("n/a");
   mCopyright = XO("n/a");

   // set clip/split handling when applying over clip boundary.
   mRestoreSplits = true;  // Default: Restore split lines. 
   mMergeClips = -1;       // Default (auto):  Merge if length remains unchanged.

   mVersion = 4;

   mStop = false;
   mBreak = false;
   mCont = false;

   mMaxLen = NYQ_MAX_LEN;

   // Interactive Nyquist
   if (fName == NYQUIST_PROMPT_ID)
   {
      mName = XO("Nyquist Prompt");
      mType = EffectTypeProcess;
      mOK = true;
      mIsPrompt = true;

      return;
   }

   mFileName = fName;
   mName = mFileName.GetName();
   mFileModified = mFileName.GetModificationTime();
   ParseFile();
}

NyquistEffect::~NyquistEffect()
{
}

// IdentInterface implementation

wxString NyquistEffect::GetPath()
{
   if (mIsPrompt)
   {
      return NYQUIST_PROMPT_ID;
   }

   return mFileName.GetFullPath();
}

wxString NyquistEffect::GetSymbol()
{
   if (mIsPrompt)
   {
      return XO("Nyquist Prompt");
   }

   return mName;
}

wxString NyquistEffect::GetName()
{
   return GetSymbol();
}

wxString NyquistEffect::GetVendor()
{
   if (mIsPrompt)
   {
      return _("Audacity");
   }

   return mAuthor;
}

wxString NyquistEffect::GetVersion()
{
   return XO("n/a");
}

wxString NyquistEffect::GetDescription()
{
   return mCopyright;
}

// EffectIdentInterface implementation

EffectType NyquistEffect::GetType()
{
   return mType;
}

wxString NyquistEffect::GetFamily()
{
   return NYQUISTEFFECTS_FAMILY;
}

bool NyquistEffect::IsInteractive()
{
   if (mIsPrompt)
   {
      return true;
   }

   return mControls.GetCount() != 0;
}

bool NyquistEffect::IsDefault()
{
   return mIsPrompt;
}

// EffectClientInterface implementation

bool NyquistEffect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   if (mExternal)
   {
      return true;
   }

   if (mIsPrompt)
   {
      parms.Write(KEY_Command, mInputCmd);
      parms.Write(KEY_Version, mVersion);

      return true;
   }

   for (size_t c = 0, cnt = mControls.GetCount(); c < cnt; c++)
   {
      NyqControl & ctrl = mControls[c];
      double d = ctrl.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
      {
         d = GetCtrlValue(ctrl.valStr);
      }

      if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
      {
         parms.Write(ctrl.var, d);
      }
      else if (ctrl.type == NYQ_CTRL_INT)
      {
         parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         wxArrayString choices = ParseChoice(ctrl);
         parms.WriteEnum(ctrl.var, (int) d, choices);
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         parms.Write(ctrl.var, ctrl.valStr);
      }
   }

   return true;
}

bool NyquistEffect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   if (mExternal)
   {
      return true;
   }

   if (mIsPrompt)
   {
      parms.Read(KEY_Command, &mInputCmd, wxEmptyString);
      parms.Read(KEY_Version, &mVersion, mVersion);

      return true;
   }

   // First pass verifies values
   for (size_t c = 0, cnt = mControls.GetCount(); c < cnt; c++)
   {
      NyqControl & ctrl = mControls[c];
      bool good = false;

      if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
      {
         double val;
         good = parms.Read(ctrl.var, &val) &&
                val >= ctrl.low &&
                val <= ctrl.high;
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         int val;
         good = parms.Read(ctrl.var, &val) &&
                val >= ctrl.low &&
                val <= ctrl.high;
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         int val;
         wxArrayString choices = ParseChoice(ctrl);
         good = parms.ReadEnum(ctrl.var, &val, choices) &&
                val != wxNOT_FOUND;
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         wxString val;
         good = parms.Read(ctrl.var, &val);
      }

      if (!good)
      {
         return false;
      }
   }

   // Second pass sets the variables
   for (size_t c = 0, cnt = mControls.GetCount(); c < cnt; c++)
   {
      NyqControl & ctrl = mControls[c];

      double d = ctrl.val;
      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
      {
         d = GetCtrlValue(ctrl.valStr);
      }

      if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
      {
         parms.Read(ctrl.var, &ctrl.val);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         int val;
         parms.Read(ctrl.var, &val);
         ctrl.val = (double) val;
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         int val;
         wxArrayString choices = ParseChoice(ctrl);
         parms.ReadEnum(ctrl.var, &val, choices);
         ctrl.val = (double) val;
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         parms.Read(ctrl.var, &ctrl.valStr);
      }
   }

   return true;
}

// Effect Implementation

bool NyquistEffect::Init()
{
   // EffectType may not be defined in script, so
   // reset each time we call the Nyquist Prompt.
   if (mIsPrompt)
      mType = EffectTypeProcess;

   // As of Audacity 2.1.2 rc1, 'spectral' effects are allowed only if
   // the selected track(s) are in a spectrogram view, and there is at
   // least one frequency bound and Spectral Selection is enabled for the
   // selected track(s) - (but don't apply to Nyquist Prompt).

   if (!mIsPrompt && mIsSpectral) {
      AudacityProject *project = GetActiveProject();
      bool bAllowSpectralEditing = true;

      SelectedTrackListOfKindIterator sel(Track::Wave, project->GetTracks());
      for (WaveTrack *t = (WaveTrack *) sel.First(); t; t = (WaveTrack *) sel.Next()) {
         if (t->GetDisplay() != WaveTrack::Spectrum ||
             !(t->GetSpectrogramSettings().SpectralSelectionEnabled())) {
            bAllowSpectralEditing = false;
            break;
         }
      }

      if (!bAllowSpectralEditing || ((mF0 < 0.0) && (mF1 < 0.0))) {
         wxMessageBox(_("To use 'Spectral effects', enable 'Spectral Selection'\n"
                        "in the track Spectrogram settings and select the\n"
                        "frequency range for the effect to act on."), 
            _("Error"), wxOK | wxICON_EXCLAMATION | wxCENTRE);

         return false;
      }
   }

   if (!mIsPrompt && !mExternal)
   {
      //TODO: If we want to auto-add parameters from spectral selection,
      //we will need to modify this test.
      //Note that removing it stops the caching of parameter values,
      //(during this session).
      if (mFileName.GetModificationTime().IsLaterThan(mFileModified)) 
      {
         SaveUserPreset(GetCurrentSettingsGroup());

         mMaxLen = NYQ_MAX_LEN;
         ParseFile();
         mFileModified = mFileName.GetModificationTime();

         LoadUserPreset(GetCurrentSettingsGroup());
      }
   }

   return true;
}

bool NyquistEffect::CheckWhetherSkipEffect()
{
   // If we're a prompt and we have controls, then we've already processed
   // the audio, so skip further processing.
   return (mIsPrompt && mControls.GetCount() > 0);
}

bool NyquistEffect::Process()
{
   bool success = true;
   mProjectChanged = false;
   EffectManager & em = EffectManager::Get();
   em.SetSkipStateFlag(false);

   if (mExternal) {
      mProgress->Hide();
   }

   // We must copy all the tracks, because Paste needs label tracks to ensure
   // correct sync-lock group behavior when the timeline is affected; then we just want
   // to operate on the selected wave tracks
   CopyInputTracks(Track::All);
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   mCurTrack[0] = (WaveTrack *) iter.First();
   mOutputTime = 0;
   mCount = 0;
   mProgressIn = 0;
   mProgressOut = 0;
   mProgressTot = 0;
   mScale = (GetType() == EffectTypeProcess ? 0.5 : 1.0) / GetNumWaveGroups();

   mStop = false;
   mBreak = false;
   mCont = false;

   mTrackIndex = 0;

   mNumSelectedChannels = 0;
   SelectedTrackListOfKindIterator sel(Track::Wave, mOutputTracks.get());
   for (WaveTrack *t = (WaveTrack *) sel.First(); t; t = (WaveTrack *) sel.Next()) {
      mNumSelectedChannels++;
      if (mT1 >= mT0) {
         if (t->GetLinked()) {
            mNumSelectedChannels++;
            sel.Next();
         }
      }
   }

   mDebugOutput.Clear();

   if (mVersion >= 4)
   {
      AudacityProject *project = GetActiveProject();

      mProps = wxEmptyString;

      mProps += wxString::Format(wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"), AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);

      mProps += wxString::Format(wxT("(setf *DECIMAL-SEPARATOR* #\\%c)\n"), wxNumberFormatter::GetDecimalSeparator());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"), EscapeString(FileNames::BaseDir()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"), EscapeString(FileNames::DataDir()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"), EscapeString(FileNames::HtmlHelpDir().RemoveLast()).c_str());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"), EscapeString(FileNames::TempDir()).c_str());

      wxArrayString paths = NyquistEffect::GetNyquistSearchPath();
      wxString list;
      for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++)
      {
         list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
      }
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list.RemoveLast().c_str());


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

      double previewLen = 6.0;
      gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'PREVIEW-DURATION)\n"),
                                 Internat::ToString(previewLen).c_str());

      // *PREVIEWP* is true when previewing (better than relying on track view).
      wxString isPreviewing = (this->IsPreviewing())? wxT("T") : wxT("NIL");
      mProps += wxString::Format(wxT("(setf *PREVIEWP* %s)\n"), isPreviewing.c_str());

      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'START)\n"),
                                 Internat::ToString(mT0).c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'END)\n"),
                                 Internat::ToString(mT1).c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (list %s) 'TRACKS)\n"), waveTrackList.c_str());
      mProps += wxString::Format(wxT("(putprop '*SELECTION* %d 'CHANNELS)\n"), mNumSelectedChannels);
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
                            wxOK | wxCENTRE, mUIParent);
               success = false;
               goto finish;
            }
            mCurStart[1] = mCurTrack[1]->TimeToLongSamples(mT0);
         }

         // Check whether we're in the same group as the last selected track
         SyncLockedTracksIterator gIter(mOutputTracks.get());
         Track *gt = gIter.StartWith(mCurTrack[0]);
         mFirstInGroup = !gtLast || (gtLast != gt);
         gtLast = gt;

         mCurStart[0] = mCurTrack[0]->TimeToLongSamples(mT0);
         auto end = mCurTrack[0]->TimeToLongSamples(mT1);
         mCurLen = end - mCurStart[0];

         if (mCurLen > NYQ_MAX_LEN) {
            float hours = (float)NYQ_MAX_LEN / (44100 * 60 * 60);
            const auto message = wxString::Format(
_("Selection too long for Nyquist code.\nMaximum allowed selection is %ld samples\n(about %.1f hours at 44100 Hz sample rate)."),
               (long)NYQ_MAX_LEN, hours
            );
            wxMessageBox(message, _("Nyquist Error"), wxOK | wxCENTRE);
            if (!mProjectChanged)
               em.SetSkipStateFlag(true);
            return false;
         }

         mCurLen = std::min(mCurLen, mMaxLen);

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

         if (mVersion >= 4)
         {
            mPerTrackProps = wxEmptyString;
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
               // with very small values, bandwidth calculation may be inf.
               // (Observed on Linux)
               double bw = log(mF1 / mF0) / log(2.0);
               if (!std::isinf(bw)) {
                  bandwidth.Printf(wxT("(float %s)"), Internat::ToString(bw).c_str());
               }
            }

#endif
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'LOW-HZ)\n"), lowHz.c_str());
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'CENTER-HZ)\n"), centerHz.c_str());
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'HIGH-HZ)\n"), highHz.c_str());
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'BANDWIDTH)\n"), bandwidth.c_str());
         }

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

   if (mDebug && !mRedirectOutput) {
      NyquistOutputDialog dlog(mUIParent, -1,
                               _("Nyquist"),
                               _("Nyquist Output: "),
                               mDebugOutput.c_str());
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   ReplaceProcessedTracks(success);

   mDebug = false;

   if (!mProjectChanged)
      em.SetSkipStateFlag(true);

   return success;
}

bool NyquistEffect::ShowInterface(wxWindow *parent, bool forceModal)
{
   // Show the normal (prompt or effect) interface
   bool res = Effect::ShowInterface(parent, forceModal);

   // Remember if the user clicked debug
   mDebug = (mUIResultID == eDebugID);

   // We're done if the user clicked "Close", we are not the Nyquist Prompt,
   // or the program currently loaded into the prompt doesn't have a UI.
   if (!res || !mIsPrompt || mControls.GetCount() == 0)
   {
      return res;
   }

   NyquistEffect effect(NYQUIST_WORKER_ID);

   effect.SetCommand(mInputCmd);
   effect.mDebug = (mUIResultID == eDebugID);

   SelectedRegion region(mT0, mT1);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   region.setF0(mF0);
   region.setF1(mF1);
#endif
   return effect.DoEffect(parent,
                          mProjectRate,
                          mTracks,
                          mFactory,
                          &region,
                          true);
}

void NyquistEffect::PopulateOrExchange(ShuttleGui & S)
{
   if (mIsPrompt)
   {
      BuildPromptWindow(S);
   }
   else
   {
      BuildEffectWindow(S);
   }

   EnableDebug();
}

bool NyquistEffect::TransferDataToWindow()
{
   mUIParent->TransferDataToWindow();

   bool success;
   if (mIsPrompt)
   {
      success = TransferDataToPromptWindow();
   }
   else
   {
      success = TransferDataToEffectWindow();
   }

   if (success)
   {
      EnablePreview(mEnablePreview);
   }

   return success;
}

bool NyquistEffect::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   if (mIsPrompt)
   {
      return TransferDataFromPromptWindow();
   }

   return TransferDataFromEffectWindow();
}

// NyquistEffect implementation

bool NyquistEffect::ProcessOne()
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
      cmd += mPerTrackProps;

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
               case WaveTrack::Waveform:
                  view = (mCurTrack[0]->GetWaveformSettings().scaleType == 0) ? wxT("\"Waveform\"") : wxT("\"Waveform (dB)\"");
                  break;
               case WaveTrack::Spectrum:
                  view = wxT("\"Spectrogram\"");
                  break;
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
      // Note: "View" property may change when Audacity's choice of track views has stabilized.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view.c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);

      double startTime = 0.0;
      double endTime = 0.0;

      if (mCurTrack[0]->GetLinked()) {
         startTime = std::min<double>(mCurTrack[0]->GetStartTime(), mCurTrack[0]->GetLink()->GetStartTime());
      }
      else {
         startTime = mCurTrack[0]->GetStartTime();
      }

      if (mCurTrack[0]->GetLinked()) {
         endTime = std::max<double>(mCurTrack[0]->GetEndTime(), mCurTrack[0]->GetLink()->GetEndTime());
      }
      else {
         endTime = mCurTrack[0]->GetEndTime();
      }

      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'START-TIME)\n"),
                              Internat::ToString(startTime).c_str());
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'END-TIME)\n"),
                              Internat::ToString(endTime).c_str());
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
         auto ca = mCurTrack[i]->SortedClipArray();
         // A list of clips for mono, or an array of lists for multi-channel.
         if (mCurNumChannels > 1) clips += wxT("(list ");
         // Each clip is a list (start-time, end-time)
         for (const auto clip: ca) {
            clips += wxString::Format(wxT("(list (float %s) (float %s))"),
                                      Internat::ToString(clip->GetStartTime()).c_str(),
                                      Internat::ToString(clip->GetEndTime()).c_str());
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

   if (GetType() == EffectTypeGenerate) {
      nyx_set_audio_params(mCurTrack[0]->GetRate(), 0);
   }
   else {
      // UNSAFE_SAMPLE_COUNT_TRUNCATION
      // Danger!  Truncation of long long to long!
      // Don't say we didn't warn you!

      // Note mCurLen was elsewhere limited to mMaxLen, which is normally
      // the greatest long value, and yet even mMaxLen may be experimentally
      // increased with a nyquist comment directive.
      // See the parsing of "maxlen"

      auto curLen = long(mCurLen.as_long_long());
      nyx_set_audio_params(mCurTrack[0]->GetRate(), curLen);

      nyx_set_input_audio(StaticGetCallback, (void *)this,
                          mCurNumChannels,
                          curLen, mCurTrack[0]->GetRate());
   }

   // Restore the Nyquist sixteenth note symbol for Generate plugins.
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=490.
   if (GetType() == EffectTypeGenerate) {
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
      if (mControls[j].type == NYQ_CTRL_REAL || mControls[j].type == NYQ_CTRL_FLOAT_TEXT) {
         // We use Internat::ToString() rather than "%f" here because we
         // always have to use the dot as decimal separator when giving
         // numbers to Nyquist, whereas using "%f" will use the user's
         // decimal separator which may be a comma in some countries.
         cmd += wxString::Format(wxT("(setf %s %s)\n"),
                                 mControls[j].var.c_str(),
                                 Internat::ToString(mControls[j].val, 14).c_str());
      }
      else if (mControls[j].type == NYQ_CTRL_INT ||
            mControls[j].type == NYQ_CTRL_INT_TEXT ||
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
      wxString str = EscapeString(mCmd);
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
      mCurBuffer[i].Free();
   }

   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   // Audacity has no idea how long Nyquist processing will take, but
   // can monitor audio being returned.
   // Anything other than audio should be returmed almost instantly
   // so notify the user that process has completed (bug 558)
   if ((rval != nyx_audio) && ((mCount + mCurNumChannels) == mNumSelectedChannels)) {
      if (mCurNumChannels == 1) {
         TrackProgress(mCount, 1.0, _("Processing complete."));
      }
      else {
         TrackGroupProgress(mCount, 1.0, _("Processing complete."));
      }
   }

   if (!rval) {
      wxLogWarning(wxT("Nyquist returned NIL"));
      return true;
   }

   if (rval == nyx_string) {
      wxMessageBox(NyquistToWxString(nyx_get_string()),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);

      // True if not process type.
      // If not returning audio from process effect,
      // return first reult then stop (disables preview)
      // but allow all output from Nyquist Prompt.
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   if (rval == nyx_double) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %f")),
                 nyx_get_double());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   if (rval == nyx_int) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %d")),
                 nyx_get_int());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   if (rval == nyx_labels) {
      mProjectChanged = true;
      unsigned int numLabels = nyx_get_num_labels();
      unsigned int l;
      LabelTrack *ltrack = NULL;

      TrackListIterator iter(mOutputTracks.get());
      for (Track *t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label) {
            ltrack = (LabelTrack *)t;
            break;
         }
      }

      if (!ltrack) {
         ltrack = static_cast<LabelTrack*>(AddToOutputTracks(mFactory->NewLabelTrack()));
      }

      for (l = 0; l < numLabels; l++) {
         double t0, t1;
         const char *str;

         // PRL:  to do:
         // let Nyquist analyzers define more complicated selections
         nyx_get_label(l, &t0, &t1, &str);

         ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str));
      }
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   if (rval != nyx_audio) {
      // This should not happen, but leaving in for now just in case (Dec 2014)
      wxMessageBox(_("Undefined return value.\n"), wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return false;
   }

   auto outChannels = nyx_get_audio_num_channels();
   if (outChannels > mCurNumChannels) {
      wxMessageBox(_("Nyquist returned too many audio channels.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return false;
   }

   if (outChannels == -1) {
      wxMessageBox(_("Nyquist returned one audio channel as an array.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return false;
   }

   if (outChannels == 0) {
      wxMessageBox(_("Nyquist returned an empty array.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mUIParent);
      return false;
   }

   double rate = mCurTrack[0]->GetRate();
   for (i = 0; i < outChannels; i++) {
      sampleFormat format = mCurTrack[i]->GetSampleFormat();

      if (outChannels == mCurNumChannels) {
         rate = mCurTrack[i]->GetRate();
      }

      mOutputTrack[i] = mFactory->NewWaveTrack(format, rate);
      mCurBuffer[i].Free();
   }

   int success = nyx_get_audio(StaticPutCallback, (void *)this);

   if (!success) {
      for(i = 0; i < outChannels; i++) {
         mOutputTrack[i].reset();
      }
      return false;
   }

   for (i = 0; i < outChannels; i++) {
      mOutputTrack[i]->Flush();
      mCurBuffer[i].Free();
      mOutputTime = mOutputTrack[i]->GetEndTime();

      if (mOutputTime <= 0) {
         wxMessageBox(_("Nyquist did not return audio.\n"),
                      wxT("Nyquist"),
                      wxOK | wxCENTRE, mUIParent);
         for (int j = 0; j < outChannels; j++) {
            mOutputTrack[j].reset();
         }
         return true;
      }
   }

   for (i = 0; i < mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == mCurNumChannels) {
         out = mOutputTrack[i].get();
      }
      else {
         out = mOutputTrack[0].get();
      }

      if (mMergeClips < 0) {
         // Use sample counts to determine default behaviour - times will rarely be equal.
         bool bMergeClips = (out->TimeToLongSamples(mT0) + out->TimeToLongSamples(mOutputTime) == 
                                                                     out->TimeToLongSamples(mT1));
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, bMergeClips);
      }
      else {
         mCurTrack[i]->ClearAndPaste(mT0, mT1, out, mRestoreSplits, mMergeClips != 0);
      }
         
      // If we were first in the group adjust non-selected group tracks
      if (mFirstInGroup) {
         SyncLockedTracksIterator git(mOutputTracks.get());
         Track *t;
         for (t = git.StartWith(mCurTrack[i]); t; t = git.Next())
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
      mOutputTrack[i].reset();
   }
   mProjectChanged = true;
   return true;
}

// ============================================================================
// NyquistEffect Implementation
// ============================================================================

wxString NyquistEffect::NyquistToWxString(const char *nyqString)
{
    wxString str(nyqString, wxConvUTF8);
    if (nyqString != NULL && nyqString[0] && str.IsEmpty()) {
        // invalid UTF-8 string, convert as Latin-1
        str = _("[Warning: Nyquist returned invalid UTF-8 string, converted here as Latin-1]");
        str += LAT1CTOWX(nyqString);
    }
    return str;
}

wxString NyquistEffect::EscapeString(const wxString & inStr)
{
   wxString str = inStr;

   str.Replace(wxT("\\"), wxT("\\\\"));
   str.Replace(wxT("\""), wxT("\\\""));

   return str;
}

wxArrayString NyquistEffect::ParseChoice(const NyqControl & ctrl)
{
   wxArrayString choices = wxStringTokenize(ctrl.label, wxT(","));

   for (size_t i = 0, cnt = choices.GetCount();i < cnt; i++)
   {
      choices[i] = choices[i].Trim(true).Trim(false);
   }

   return choices;
}

void NyquistEffect::RedirectOutput()
{
   mRedirectOutput = true;
}

void NyquistEffect::SetCommand(const wxString &cmd)
{
   mExternal = true;

   ParseCommand(cmd);
}

void NyquistEffect::Break()
{
   mBreak = true;
}

void NyquistEffect::Continue()
{
   mCont = true;
}

void NyquistEffect::Stop()
{
   mStop = true;
}

wxString NyquistEffect::UnQuote(const wxString &s)
{
   wxString out;
   int len = s.Length();

   if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
      return s.Mid(1, len - 2);
   }

   return s;
}

double NyquistEffect::GetCtrlValue(const wxString &s)
{
   /* For this to work correctly requires that the plug-in header is
    * parsed on each run so that the correct value for "half-srate" may
    * be determined.
    * 
   AudacityProject *project = GetActiveProject();
   double rate = INT_MAX;
   if (project && s.IsSameAs(wxT("half-srate"), false)) {
      SelectedTrackListOfKindIterator sel(Track::Wave, project->GetTracks());
      for (WaveTrack *t = (WaveTrack *) sel.First(); t; t = (WaveTrack *) sel.Next()) {
         rate = std::min(t->GetRate(), rate);
      }
      return (rate / 2.0);
   }
   */

   return Internat::CompatibleToDouble(s);
}

void NyquistEffect::Parse(const wxString &line)
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

   // Consistency decission is for "plug-in" as the correct spelling
   // "plugin" is allowed as an undocumented convenience.
   if (len == 2 && tokens[0] == wxT("nyquist") &&
      (tokens[1] == wxT("plug-in") || tokens[1] == wxT("plugin"))) {
      mOK = true;
      return;
   }

   if (len >= 2 && tokens[0] == wxT("type")) {
      if (tokens[1] == wxT("process")) {
         mType = EffectTypeProcess;
      }
      else if (tokens[1] == wxT("generate")) {
         mType = EffectTypeGenerate;
      }
      else if (tokens[1] == wxT("analyze")) {
         mType = EffectTypeAnalyze;
      }
      if (len >= 3 && tokens[2] == wxT("spectral")) {;
         mIsSpectral = true;
      }
      return;
   }

   if (len == 2 && tokens[0] == wxT("codetype")) {
      // This will stop ParseProgram() from doing a best guess as program type.
      if (tokens[1] == wxT("lisp")) {
         mIsSal = false;
         mFoundType = true;
      }
      else if (tokens[1] == wxT("sal")) {
         mIsSal = true;
         mFoundType = true;
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
      if (mName.EndsWith(wxT("...")))
      {
         mName = mName.RemoveLast(3);
      }
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
         SetLinearEffectFlag(false);
      }
      else if (tokens[1] == wxT("linear")) {
         mEnablePreview = true;
         SetLinearEffectFlag(true);
      }
      else if (tokens[1] == wxT("selection")) {
         mEnablePreview = true;
         SetPreviewFullSelectionFlag(true);
      }
      else if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mEnablePreview = false;
      }
      return;
   }

   // Maximum number of samples to be processed. This can help the
   // progress bar if effect does not process all of selection.
   if (len >= 2 && tokens[0] == wxT("maxlen")) {
      long long v; // Note that Nyquist may overflow at > 2^31 samples (bug 439)
      tokens[1].ToLongLong(&v);
      mMaxLen = (sampleCount) v;
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
      mRestoreSplits = !!v;
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
      ctrl.val = GetCtrlValue(ctrl.valStr);

      if (tokens[3] == wxT("string")) {
         ctrl.type = NYQ_CTRL_STRING;
      }
      else if (tokens[3] == wxT("choice")) {
         ctrl.type = NYQ_CTRL_CHOICE;
      }
      else {
         if (len < 8) {
            return;
         }

         if ((tokens[3] == wxT("float")) ||
               (tokens[3] == wxT("real"))) // Deprecated
            ctrl.type = NYQ_CTRL_REAL;
         else if (tokens[3] == wxT("int"))
            ctrl.type = NYQ_CTRL_INT;
         else if (tokens[3] == wxT("float-text"))
            ctrl.type = NYQ_CTRL_FLOAT_TEXT;
         else if (tokens[3] == wxT("int-text"))
            ctrl.type = NYQ_CTRL_INT_TEXT;
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
         if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
            ctrl.low = INT_MIN;
         }
         else if (ctrl.type == NYQ_CTRL_FLOAT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
            ctrl.low = -(FLT_MAX);
         }
         else {
            ctrl.low = GetCtrlValue(ctrl.lowStr);
         }

         ctrl.highStr = tokens[7];
         if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
            ctrl.high = INT_MAX;
         }
         else if (ctrl.type == NYQ_CTRL_FLOAT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
            ctrl.high = FLT_MAX;
         }
         else {
            ctrl.high = GetCtrlValue(ctrl.highStr);
         }

         if (ctrl.high < ctrl.low) {
            ctrl.high = ctrl.low + 1;
         }

         if (ctrl.val < ctrl.low) {
            ctrl.val = ctrl.low;
         }

         if (ctrl.val > ctrl.high) {
            ctrl.val = ctrl.high;
         }

         ctrl.ticks = 1000;
         if (ctrl.type == NYQ_CTRL_INT &&
             (ctrl.high - ctrl.low < ctrl.ticks)) {
            ctrl.ticks = (int)(ctrl.high - ctrl.low);
         }
      }

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

bool NyquistEffect::ParseProgram(wxInputStream & stream)
{
   if (!stream.IsOk())
   {
      return false;
   }

   wxTextInputStream pgm(stream);

   mCmd = wxT("");
   mIsSal = false;
   mControls.Clear();
   mCategories.Clear();
   mIsSpectral = false;

   mFoundType = false;
   while (!stream.Eof() && stream.IsOk())
   {
      wxString line = pgm.ReadLine().Trim(false);
      if (line.Length() > 1 && line[0] == wxT(';'))
      {
         Parse(line);
      }
      else if (!mFoundType && line.Length() > 0)
      {
         mFoundType = true;

         if (line[0] == wxT('(') ||
            (line[0] == wxT('#') && line.Length() > 1 && line[1] == wxT('|')))
         {
            mIsSal = false;
         }
         else if (line.Upper().Find(wxT("RETURN")) != wxNOT_FOUND)
         {
            mIsSal = true;
         }
         else if (mIsPrompt)
         {
            wxMessageBox(_("Your code looks like SAL syntax, but there is no return statement. Either use a return statement such as\n\treturn s * 0.1\nfor SAL, or begin with an open parenthesis such as\n\t(mult s 0.1)\n for LISP."), _("Error in Nyquist code"), wxOK | wxCENTRE);
            return false;
         }
         // Just throw it at Nyquist to see what happens
      }

      // preserve comments so that SAL effects compile with proper line numbers
      mCmd += line + wxT("\n");
   }

   return true;
}

void NyquistEffect::ParseFile()
{
   mEnablePreview = true;

   wxFileInputStream stream(mFileName.GetFullPath());

   ParseProgram(stream);
}

bool NyquistEffect::ParseCommand(const wxString & cmd)
{
   mEnablePreview = true;

   wxStringInputStream stream(cmd + wxT(" "));

   return ParseProgram(stream);
}

int NyquistEffect::StaticGetCallback(float *buffer, int channel,
                                     long start, long len, long totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->GetCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::GetCallback(float *buffer, int ch,
                               long start, long len, long WXUNUSED(totlen))
{
   if (mCurBuffer[ch].ptr()) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         mCurBuffer[ch].Free();
      }
   }

   if (!mCurBuffer[ch].ptr()) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

      if (mCurBufferLen[ch] < len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      mCurBufferLen[ch] =
         limitSampleBufferSize( mCurBufferLen[ch],
                                mCurStart[ch] + mCurLen - mCurBufferStart[ch] );

      mCurBuffer[ch].Allocate(mCurBufferLen[ch], floatSample);
      if (!mCurTrack[ch]->Get(mCurBuffer[ch].ptr(), floatSample,
                              mCurBufferStart[ch], mCurBufferLen[ch])) {

         wxPrintf(wxT("GET error\n"));

         return -1;
      }
   }

   // We have guaranteed above that this is nonnegative and bounded by
   // mCurBufferLen[ch]:
   auto offset = ( mCurStart[ch] + start - mCurBufferStart[ch] ).as_size_t();
   CopySamples(mCurBuffer[ch].ptr() + offset*SAMPLE_SIZE(floatSample), floatSample,
               (samplePtr)buffer, floatSample,
               len);

   if (ch == 0) {
      double progress = mScale *
         ( (start+len)/ mCurLen.as_double() );

      if (progress > mProgressIn) {
         mProgressIn = progress;
      }

      if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
         return -1;
      }
   }

   return 0;
}

int NyquistEffect::StaticPutCallback(float *buffer, int channel,
                                     long start, long len, long totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->PutCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::PutCallback(float *buffer, int channel,
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

void NyquistEffect::StaticOutputCallback(int c, void *This)
{
   ((NyquistEffect *)This)->OutputCallback(c);
}

void NyquistEffect::OutputCallback(int c)
{
   if (mDebug && !mRedirectOutput) {
      mDebugOutput += (char)c;
      return;
   }

   std::cout << (char)c;
}

void NyquistEffect::StaticOSCallback(void *This)
{
   ((NyquistEffect *)This)->OSCallback();
}

void NyquistEffect::OSCallback()
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

wxArrayString NyquistEffect::GetNyquistSearchPath()
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

bool NyquistEffect::TransferDataToPromptWindow()
{
   mCommandText->ChangeValue(mInputCmd);
   mVersionCheckBox->SetValue(mVersion <= 3);

   return true;
}

bool NyquistEffect::TransferDataToEffectWindow()
{
   for (size_t i = 0, cnt = mControls.GetCount(); i < cnt; i++)
   {
      NyqControl & ctrl = mControls[i];

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         wxArrayString choices = ParseChoice(ctrl);

         int val = (int)ctrl.val;
         if (val < 0 || val >= (int)choices.GetCount())
         {
            val = 0;
         }

         wxChoice *c = (wxChoice *) mUIParent->FindWindow(ID_Choice + i);
         c->SetSelection(val);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_REAL)
      {
         // wxTextCtrls are handled by the validators
         double range = ctrl.high - ctrl.low;
         int val = (int)(0.5 + ctrl.ticks * (ctrl.val - ctrl.low) / range);
         wxSlider *s = (wxSlider *) mUIParent->FindWindow(ID_Slider + i);
         s->SetValue(val);
      }
   }

   return true;
}

bool NyquistEffect::TransferDataFromPromptWindow()
{
   mInputCmd = mCommandText->GetValue();
   mVersion = mVersionCheckBox->GetValue() ? 3 : 4;

   return ParseCommand(mInputCmd);
}

bool NyquistEffect::TransferDataFromEffectWindow()
{
   if (mControls.GetCount() == 0)
   {
      return true;
   }

   for (unsigned int i = 0; i < mControls.GetCount(); i++)
   {
      NyqControl *ctrl = &mControls[i];

      if (ctrl->type == NYQ_CTRL_STRING)
      {
         continue;
      }

      if (ctrl->val == UNINITIALIZED_CONTROL)
      {
         ctrl->val = GetCtrlValue(ctrl->valStr);
      }

      if (ctrl->type == NYQ_CTRL_CHOICE)
      {
         continue;
      }

      if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->lowStr.IsSameAs(wxT("nil"), false)) {
         ctrl->low = INT_MIN;
      }
      else if (ctrl->type == NYQ_CTRL_FLOAT_TEXT && ctrl->lowStr.IsSameAs(wxT("nil"), false)) {
         ctrl->low = -(FLT_MAX);
      }
      else {
         ctrl->low = GetCtrlValue(ctrl->lowStr);
      }

      if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->highStr.IsSameAs(wxT("nil"), false)) {
         ctrl->high = INT_MAX;
      }
      else if (ctrl->type == NYQ_CTRL_FLOAT_TEXT && ctrl->highStr.IsSameAs(wxT("nil"), false)) {
         ctrl->high = FLT_MAX;
      }
      else {
         ctrl->high = GetCtrlValue(ctrl->highStr);
      }

      if (ctrl->high < ctrl->low)
      {
         ctrl->high = ctrl->low + 1;
      }

      if (ctrl->val < ctrl->low)
      {
         ctrl->val = ctrl->low;
      }

      if (ctrl->val > ctrl->high)
      {
         ctrl->val = ctrl->high;
      }

      ctrl->ticks = 1000;
      if (ctrl->type == NYQ_CTRL_INT &&
          (ctrl->high - ctrl->low < ctrl->ticks))
      {
         ctrl->ticks = (int)(ctrl->high - ctrl->low);
      }
   }

   return true;
}

void NyquistEffect::BuildPromptWindow(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);

         S.AddVariableText(_("Enter Nyquist Command: "));

         S.AddSpace(1, 1);

         mVersionCheckBox = S.AddCheckBox(_("&Use legacy (version 3) syntax."),
                                          (mVersion == 3) ? wxT("true") : wxT("false"));  
      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxEXPAND, 1);
      {
          mCommandText = S.AddTextWindow(wxT(""));
          mCommandText->SetMinSize(wxSize(500, 200));
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxALIGN_CENTER, 0);
      {
         S.Id(ID_Load).AddButton(_("&Load"));
         S.Id(ID_Save).AddButton(_("&Save"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   mCommandText->SetFocus();
}

void NyquistEffect::BuildEffectWindow(ShuttleGui & S)
{
   S.SetStyle(wxVSCROLL | wxTAB_TRAVERSAL);
   wxScrolledWindow *scroller = S.StartScroller(2);
   {
      S.StartMultiColumn(4);
      {
         for (size_t i = 0; i < mControls.GetCount(); i++)
         {
            NyqControl & ctrl = mControls[i];

            S.AddPrompt(ctrl.name + wxT(":"));

            if (ctrl.type == NYQ_CTRL_STRING)
            {
               S.AddSpace(10, 10);
            
               wxTextCtrl *item = S.Id(ID_Text + i).AddTextBox(wxT(""), wxT(""), 12);
               item->SetValidator(wxGenericValidator(&ctrl.valStr));
            }
            else if (ctrl.type == NYQ_CTRL_CHOICE)
            {
               S.AddSpace(10, 10);

               wxArrayString choices = wxStringTokenize(ctrl.label, wxT(","));
               S.Id(ID_Choice + i).AddChoice(wxT(""), wxT(""), &choices);
            }
            else
            {
               // Integer or Real
               if (ctrl.type == NYQ_CTRL_INT_TEXT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
               {
                  S.AddSpace(10, 10);
               }

               wxTextCtrl *item = S.Id(ID_Text+i).AddTextBox(wxT(""), wxT(""), 
                                                             (ctrl.type == NYQ_CTRL_INT_TEXT ||
                                                              ctrl.type == NYQ_CTRL_FLOAT_TEXT) ? 25 : 12);

               double range = ctrl.high - ctrl.low;

               if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
               {
                  // > 12 decimal places can cause rounding errors in display.
                  FloatingPointValidator<double> vld(12, &ctrl.val);
                  vld.SetRange(ctrl.low, ctrl.high);

                  // Set number of decimal places
                  int style = range < 10 ? NUM_VAL_THREE_TRAILING_ZEROES :
                              range < 100 ? NUM_VAL_TWO_TRAILING_ZEROES :
                              NUM_VAL_ONE_TRAILING_ZERO;
                  vld.SetStyle(style);

                  item->SetValidator(vld);
               }
               else
               {
                  IntegerValidator<double> vld(&ctrl.val);
                  vld.SetRange((int) ctrl.low, (int) ctrl.high);
                  item->SetValidator(vld);
               }

               if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_REAL)
               {
                  S.SetStyle(wxSL_HORIZONTAL);
                  S.Id(ID_Slider + i).AddSlider(wxT(""), 0, ctrl.ticks, 0);
                  S.SetSizeHints(150, -1);
               }
            }

            if (ctrl.type == NYQ_CTRL_CHOICE || ctrl.label.IsEmpty())
            {
               S.AddSpace(10, 10);
            }
            else
            {
               S.AddUnits(ctrl.label);
            }
         }
      }
      S.EndMultiColumn();
   }
   S.EndScroller();

   scroller->SetScrollRate(0, 20);

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   scroller->SetName(wxT("\a"));
   scroller->SetLabel(wxT("\a"));
}

// NyquistEffect implementation

bool NyquistEffect::IsOk()
{
   return mOK;
}

void NyquistEffect::OnLoad(wxCommandEvent & WXUNUSED(evt))
{
   if (mCommandText->IsModified())
   {
      if (wxMessageBox(_("Current program has been modified.\nDiscard changes?"),
                       GetName(),
                       wxYES_NO) == wxNO)
      {
         return;
      }
   }

   FileDialog dlog(mUIParent,
                   _("Load Nyquist script"),
                   mFileName.GetPath(),
                   wxEmptyString,
                   _("Nyquist scripts (*.ny)|*.ny|Lisp scripts (*.lsp)|*.lsp|Text files (*.txt)|*.txt|All files|*"),
                   wxFD_OPEN | wxRESIZE_BORDER);

   if (dlog.ShowModal() != wxID_OK)
   {
      return;
   }

   mFileName = dlog.GetPath();

   if (!mCommandText->LoadFile(mFileName.GetFullPath()))
   {
      wxMessageBox(_("File could not be loaded"), GetName());
   }
}

void NyquistEffect::OnSave(wxCommandEvent & WXUNUSED(evt))
{
   FileDialog dlog(mUIParent,
                   _("Save Nyquist script"),
                   mFileName.GetPath(),
                   mFileName.GetFullName(),
                   _("Nyquist scripts (*.ny)|*.ny|Lisp scripts (*.lsp)|*.lsp|All files|*"),
                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);

   if (dlog.ShowModal() != wxID_OK)
   {
      return;
   }

   mFileName = dlog.GetPath();

   if (!mCommandText->SaveFile(mFileName.GetFullPath()))
   {
      wxMessageBox(_("File could not be saved"), GetName());
   }
}

void NyquistEffect::OnSlider(wxCommandEvent & evt)
{
   int i = evt.GetId() - ID_Slider;
   NyqControl & ctrl = mControls[i];

   int val = evt.GetInt();
   double range = ctrl.high - ctrl.low;
   double newVal = (val / (double)ctrl.ticks) * range + ctrl.low;

   // Determine precision for displayed number
   int precision = range < 1.0 ? 3 :
                   range < 10.0 ? 2 :
                   range < 100.0 ? 1 :
                   0;

   // If the value is at least one tick different from the current value
   // change it (this prevents changes from manually entered values unless
   // the slider actually moved)
   if (fabs(newVal - ctrl.val) >= (1 / (double)ctrl.ticks) * range &&
       fabs(newVal - ctrl.val) >= pow(0.1, precision) / 2)
   {
      // First round to the appropriate precision
      newVal *= pow(10.0, precision);
      newVal = floor(newVal + 0.5);
      newVal /= pow(10.0, precision);

      ctrl.val = newVal;

      mUIParent->FindWindow(ID_Text + i)->GetValidator()->TransferToWindow();
   }
}

void NyquistEffect::OnChoice(wxCommandEvent & evt)
{
   mControls[evt.GetId() - ID_Choice].val = (double) evt.GetInt();
}

void NyquistEffect::OnText(wxCommandEvent & evt)
{
   int i = evt.GetId() - ID_Text;

   NyqControl & ctrl = mControls[i];

   if (wxDynamicCast(evt.GetEventObject(), wxWindow)->GetValidator()->TransferFromWindow())
   {
      if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_INT)
      {
         int pos = (int)floor((ctrl.val - ctrl.low) /
                              (ctrl.high - ctrl.low) * ctrl.ticks + 0.5);

         wxSlider *slider = (wxSlider *)mUIParent->FindWindow(ID_Slider + i);
         slider->SetValue(pos);
      }
   }
}

///////////////////////////////////////////////////////////////////////////////
//
// NyquistOutputDialog
//
///////////////////////////////////////////////////////////////////////////////


BEGIN_EVENT_TABLE(NyquistOutputDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, NyquistOutputDialog::OnOk)
END_EVENT_TABLE()

NyquistOutputDialog::NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                                       const wxString & title,
                                       const wxString & prompt,
                                       const wxString &message)
:  wxDialogWrapper(parent, id, title)
{
   SetName(GetTitle());

   wxBoxSizer *mainSizer;
   {
      auto uMainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      mainSizer = uMainSizer.get();
      wxButton   *button;
      wxControl  *item;

      item = safenew wxStaticText(this, -1, prompt);
      item->SetName(prompt);  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      mainSizer->Add(item, 0, wxALIGN_LEFT | wxLEFT | wxTOP | wxRIGHT, 10);

      // TODO use ShowInfoDialog() instead.
      // Beware this dialog MUST work with screen readers.
      item = safenew wxTextCtrl(this, -1, message,
         wxDefaultPosition, wxSize(400, 200),
         wxTE_MULTILINE | wxTE_READONLY);
      mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 10);

      {
         auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

         /* i18n-hint: In most languages OK is to be translated as OK.  It appears on a button.*/
         button = safenew wxButton(this, wxID_OK, _("OK"));
         button->SetDefault();
         hSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

         mainSizer->Add(hSizer.release(), 0, wxALIGN_CENTRE | wxLEFT | wxBOTTOM | wxRIGHT, 5);
      }

      SetAutoLayout(true);
      SetSizer(uMainSizer.release());
   }

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

