/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class NyquistEffect
\brief An Effect that calls up a Nyquist (XLISP) plug-in, i.e. many possible
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
#include <wx/sstream.h>
#include <wx/textdlg.h>
#include <wx/txtstrm.h>
#include <wx/valgen.h>
#include <wx/wfstream.h>
#include <wx/numformatter.h>

#include "../../AudacityApp.h"
#include "../../FileException.h"
#include "../../FileNames.h"
#include "../../Internat.h"
#include "../../LabelTrack.h"
#include "../../prefs/SpectrogramSettings.h"
#include "../../Project.h"
#include "../../ShuttleGui.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../widgets/valnum.h"
#include "../../widgets/ErrorDialog.h"
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

static const wxChar *KEY_Version = wxT("Version");
static const wxChar *KEY_Command = wxT("Command");

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffect
//
///////////////////////////////////////////////////////////////////////////////

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
   mOutputTrack[0] = mOutputTrack[1] = nullptr;

   mAction = _("Applying Nyquist Effect...");
   mInputCmd = wxEmptyString;
   mCmd = wxEmptyString;
   mIsPrompt = false;
   mExternal = false;
   mCompiler = false;
   mTrace = false;
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
   if (fName == NYQUIST_PROMPT_ID) {
      /* i18n-hint: "Nyquist" is an embedded interpreted programming language in
       Audacity, named in honor of the Swedish-American Harry Nyquist (or Nyqvist).
       In the translations of this and other strings, you may transliterate the
       name into another alphabet.  */
      mName = XO("Nyquist Prompt");
      mType = EffectTypeProcess;
      mOK = true;
      mIsPrompt = true;

      return;
   }

   if (fName == NYQUIST_WORKER_ID) {
      // Effect spawned from Nyquist Prompt
/* i18n-hint: It is acceptable to translate this the same as for "Nyquist Prompt" */
      mName = XO("Nyquist Worker");
      return;
   }

   mFileName = fName;
   mName = mFileName.GetName();
   mFileModified = mFileName.GetModificationTime();
   ParseFile();

   if (!mOK && mInitError.empty())
      mInitError = _("Ill-formed Nyquist plug-in header");
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

wxString NyquistEffect::ManualPage()
{
      return mIsPrompt
         ? wxT("Nyquist_Prompt")
         : mManPage;
}

wxString NyquistEffect::HelpPage()
{
   wxArrayString paths = NyquistEffect::GetNyquistSearchPath();
   wxString fileName;

   for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++) {
      fileName = wxFileName(paths[i] + wxT("/") + mHelpFile).GetFullPath();
      if (wxFileExists(fileName)) {
         mHelpFileExists = true;
         return fileName;
      }
   }
   return wxEmptyString;
}

// EffectDefinitionInterface implementation

EffectType NyquistEffect::GetType()
{
   return mType;
}

wxString NyquistEffect::GetFamilyId()
{
   return NYQUISTEFFECTS_FAMILY;
}

wxString NyquistEffect::GetFamilyName()
{
   return NYQUISTEFFECTS_FAMILY;
}

bool NyquistEffect::IsInteractive()
{
   if (mIsPrompt)
   {
      return true;
   }

   return mControls.size() != 0;
}

bool NyquistEffect::IsDefault()
{
   return mIsPrompt;
}

// EffectClientInterface implementation
bool NyquistEffect::DefineParams( ShuttleParams & S )
{
   // For now we assume Nyquist can do get and set better than DefineParams can,
   // And so we ONLY use it for geting the signature.
   auto pGa = dynamic_cast<ShuttleGetAutomation*>(&S);
   if( pGa ){
      GetAutomationParameters( *(pGa->mpEap) );
      return true;
   }
   auto pSa = dynamic_cast<ShuttleSetAutomation*>(&S);
   if( pSa ){
      SetAutomationParameters( *(pSa->mpEap) );
      return true;
   }
   auto pSd  = dynamic_cast<ShuttleGetDefinition*>(&S);
   if( pSd == nullptr )
      return true;
   //wxASSERT( pSd );

   if (mExternal)
      return true;

   if (mIsPrompt)
   {
      S.Define( mInputCmd, KEY_Command, "" );
      S.Define( mVersion, KEY_Version, 3 );
      return true;
   }

   for (size_t c = 0, cnt = mControls.size(); c < cnt; c++)
   {
      NyqControl & ctrl = mControls[c];
      double d = ctrl.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
      {
         d = GetCtrlValue(ctrl.valStr);
      }

      if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
      {
         S.Define( d, static_cast<const wxChar*>( ctrl.var.c_str() ), (double)0.0, ctrl.low, ctrl.high, 1.0);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         int x=d;
         S.Define( x, static_cast<const wxChar*>( ctrl.var.c_str() ), 0, ctrl.low, ctrl.high, 1);
         //parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         const wxArrayString &choices = ctrl.choices;
         int x=d;
         //parms.WriteEnum(ctrl.var, (int) d, choices);
         S.DefineEnum( x, static_cast<const wxChar*>( ctrl.var.c_str() ), 0, choices );
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         S.Define( ctrl.valStr, ctrl.var, "" , ctrl.lowStr, ctrl.highStr );
         //parms.Write(ctrl.var, ctrl.valStr);
      }
   }
   return true;
}

bool NyquistEffect::GetAutomationParameters(CommandParameters & parms)
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

   for (size_t c = 0, cnt = mControls.size(); c < cnt; c++)
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
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         const wxArrayString &choices = ctrl.choices;
         parms.WriteEnum(ctrl.var, (int) d, choices);
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         parms.Write(ctrl.var, ctrl.valStr);
      }
   }

   return true;
}

bool NyquistEffect::SetAutomationParameters(CommandParameters & parms)
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
   for (size_t c = 0, cnt = mControls.size(); c < cnt; c++)
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
         const wxArrayString &choices = ctrl.choices;
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
   for (size_t c = 0, cnt = mControls.size(); c < cnt; c++)
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
         int val {0};
         const wxArrayString &choices = ctrl.choices;
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
   // TODO: Document: Init() is called each time the effect is called but
   // AFTER the UI (if any) has been created, so headers that affect the UI
   // are only initialised at the start of the session.

   // EffectType may not be defined in script, so
   // reset each time we call the Nyquist Prompt.
   if (mIsPrompt) {
      mType = EffectTypeProcess;
      mName = XO("Nyquist Prompt");
      mDebugButton = true;    // Debug button always enabled for Nyquist Prompt.
      mEnablePreview = true;  // Preview button always enabled for Nyquist Prompt.
   }

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
         Effect::MessageBox(_("To use 'Spectral effects', enable 'Spectral Selection'\n"
                        "in the track Spectrogram settings and select the\n"
                        "frequency range for the effect to act on."), 
            wxOK | wxICON_EXCLAMATION | wxCENTRE, _("Error"));

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
   return (mIsPrompt && mControls.size() > 0);
}

static void RegisterFunctions();

bool NyquistEffect::Process()
{
   RegisterFunctions();

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
   if (!mHelpFile.IsEmpty() && !mHelpFileExists) {
      mDebugOutput = wxString::Format(_("error: File \"%s\" specified in header but not found in plug-in path.\n"), mHelpFile);
   }

   if (mVersion >= 4)
   {
      AudacityProject *project = GetActiveProject();

      mProps = wxEmptyString;

      mProps += wxString::Format(wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"), AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);

      mProps += wxString::Format(wxT("(setf *DECIMAL-SEPARATOR* #\\%c)\n"), wxNumberFormatter::GetDecimalSeparator());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"), EscapeString(FileNames::BaseDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"), EscapeString(FileNames::DataDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"), EscapeString(FileNames::HtmlHelpDir().RemoveLast()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"), EscapeString(FileNames::TempDir()));

      wxArrayString paths = NyquistEffect::GetNyquistSearchPath();
      wxString list;
      for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++)
      {
         list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
      }
      list = list.RemoveLast();

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUG-IN)\n"), list);


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

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DATE)\n"), now.FormatDate());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'TIME)\n"), now.FormatTime());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-DATE)\n"), now.FormatISODate());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'ISO-TIME)\n"), now.FormatISOTime());
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'YEAR)\n"), year);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'DAY)\n"), dom);   // day of month
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* %d 'MONTH)\n"), month);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'MONTH-NAME)\n"), now.GetMonthName(month));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-TIME* \"%s\" 'DAY-NAME)\n"), now.GetWeekDayName(day));

      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'PROJECTS)\n"), (int) gAudacityProjects.size());
      mProps += wxString::Format(wxT("(putprop '*PROJECT* \"%s\" 'NAME)\n"), project->GetName());

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
                                 Internat::ToString(project->GetRate()));
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TRACKS)\n"), numTracks);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'WAVETRACKS)\n"), numWave);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'LABELTRACKS)\n"), numLabel);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'MIDITRACKS)\n"), numMidi);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'TIMETRACKS)\n"), numTime);

      double previewLen = 6.0;
      gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'PREVIEW-DURATION)\n"),
                                 Internat::ToString(previewLen));

      // *PREVIEWP* is true when previewing (better than relying on track view).
      wxString isPreviewing = (this->IsPreviewing())? wxT("T") : wxT("NIL");
      mProps += wxString::Format(wxT("(setf *PREVIEWP* %s)\n"), isPreviewing);

      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'START)\n"),
                                 Internat::ToString(mT0));
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'END)\n"),
                                 Internat::ToString(mT1));
      mProps += wxString::Format(wxT("(putprop '*SELECTION* (list %s) 'TRACKS)\n"), waveTrackList);
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
               Effect::MessageBox(_("Sorry, cannot apply effect on stereo tracks where the tracks don't match."),
                            wxOK | wxCENTRE);
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
            Effect::MessageBox(message, wxOK | wxCENTRE, _("Nyquist Error"));
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

         auto cleanup = finally( [&] {
            nyx_capture_output(NULL, (void *)NULL);
            nyx_set_os_callback(NULL, (void *)NULL);
            nyx_cleanup();
         } );


         if (mVersion >= 4)
         {
            mPerTrackProps = wxEmptyString;
            wxString lowHz = wxT("nil");
            wxString highHz = wxT("nil");
            wxString centerHz = wxT("nil");
            wxString bandwidth = wxT("nil");

#if defined(EXPERIMENTAL_SPECTRAL_EDITING)
            if (mF0 >= 0.0) {
               lowHz.Printf(wxT("(float %s)"), Internat::ToString(mF0));
            }

            if (mF1 >= 0.0) {
               highHz.Printf(wxT("(float %s)"), Internat::ToString(mF1));
            }

            if ((mF0 >= 0.0) && (mF1 >= 0.0)) {
               centerHz.Printf(wxT("(float %s)"), Internat::ToString(sqrt(mF0 * mF1)));
            }

            if ((mF0 > 0.0) && (mF1 >= mF0)) {
               // with very small values, bandwidth calculation may be inf.
               // (Observed on Linux)
               double bw = log(mF1 / mF0) / log(2.0);
               if (!std::isinf(bw)) {
                  bandwidth.Printf(wxT("(float %s)"), Internat::ToString(bw));
               }
            }

#endif
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'LOW-HZ)\n"), lowHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'CENTER-HZ)\n"), centerHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'HIGH-HZ)\n"), highHz);
            mPerTrackProps += wxString::Format(wxT("(putprop '*SELECTION* %s 'BANDWIDTH)\n"), bandwidth);
         }

         success = ProcessOne();

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

   // Show debug window if trace set in plug-in header and something to show.
   mDebug = (mTrace && !mDebugOutput.IsEmpty())? true : mDebug;

   if (mDebug && !mRedirectOutput) {
      NyquistOutputDialog dlog(mUIParent, -1,
                               mName,
                               _("Debug Output: "),
                               mDebugOutput);
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   ReplaceProcessedTracks(success);

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
   if (!res || !mIsPrompt || mControls.size() == 0)
   {
      return res;
   }

   NyquistEffect effect(NYQUIST_WORKER_ID);

   effect.SetCommand(mInputCmd);
   effect.mDebug = (mUIResultID == eDebugID);

   return Delegate(effect, parent, true);
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

   EnableDebug(mDebugButton);
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
   mError = false;
   mFailedFileName.Clear();

   nyx_rval rval;

   wxString cmd;

   // TODO: Document.
   // Nyquist default latency is 300 ms, which is rather conservative and
   // too long when playback set to ALSA (bug 570), so we'll use 100 ms like Audacity.
   cmd += wxT("(snd-set-latency  0.1)");

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
      wxString spectralEditp;

      switch (mCurTrack[0]->GetKind())
      {
         case Track::Wave:
            type = wxT("wave");
            spectralEditp = mCurTrack[0]->GetSpectrogramSettings().SpectralSelectionEnabled()? wxT("T") : wxT("NIL");
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
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'NAME)\n"), mCurTrack[0]->GetName());
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'TYPE)\n"), type);
      // Note: "View" property may change when Audacity's choice of track views has stabilized.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view);
      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);

      //NOTE: Audacity 2.1.3 True if spectral selection is enabled regardless of track view.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'SPECTRAL-EDIT-ENABLED)\n"), spectralEditp);

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
                              Internat::ToString(startTime));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'END-TIME)\n"),
                              Internat::ToString(endTime));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'GAIN)\n"),
                              Internat::ToString(mCurTrack[0]->GetGain()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'PAN)\n"),
                              Internat::ToString(mCurTrack[0]->GetPan()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* (float %s) 'RATE)\n"),
                              Internat::ToString(mCurTrack[0]->GetRate()));

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
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'FORMAT)\n"), bitFormat);

      float maxPeakLevel = 0.0;  // Deprecated as of 2.1.3
      wxString clips, peakString, rmsString;
      for (size_t i = 0; i < mCurNumChannels; i++) {
         auto ca = mCurTrack[i]->SortedClipArray();
         float maxPeak = 0.0;

         // A list of clips for mono, or an array of lists for multi-channel.
         if (mCurNumChannels > 1) {
            clips += wxT("(list ");
         }
         // Each clip is a list (start-time, end-time)
         for (const auto clip: ca) {
            clips += wxString::Format(wxT("(list (float %s) (float %s))"),
                                      Internat::ToString(clip->GetStartTime()),
                                      Internat::ToString(clip->GetEndTime()));
         }
         if (mCurNumChannels > 1) clips += wxT(" )");

         float min, max;
         auto pair = mCurTrack[i]->GetMinMax(mT0, mT1); // may throw
         min = pair.first, max = pair.second;
         maxPeak = wxMax(wxMax(fabs(min), fabs(max)), maxPeak);
         maxPeakLevel = wxMax(maxPeakLevel, maxPeak);

         // On Debian, NaN samples give maxPeak = 3.40282e+38 (FLT_MAX)
         if (!std::isinf(maxPeak) && !std::isnan(maxPeak) && (maxPeak < FLT_MAX)) {
            peakString += wxString::Format(wxT("(float %s) "), Internat::ToString(maxPeak));
         } else {
            peakString += wxT("nil ");
         }

         float rms = mCurTrack[i]->GetRMS(mT0, mT1); // may throw
         if (!std::isinf(rms) && !std::isnan(rms)) {
            rmsString += wxString::Format(wxT("(float %s) "), Internat::ToString(rms));
         } else {
            rmsString += wxT("nil ");
         }
      }
      // A list of clips for mono, or an array of lists for multi-channel.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s%s ) 'CLIPS)\n"),
                              (mCurNumChannels == 1) ? wxT("(list ") : wxT("(vector "),
                              clips);

      (mCurNumChannels > 1)?
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (vector %s) 'PEAK)\n"), peakString) :
         cmd += wxString::Format(wxT("(putprop '*SELECTION* %s 'PEAK)\n"), peakString);

      if (!std::isinf(maxPeakLevel) && !std::isnan(maxPeakLevel) && (maxPeakLevel < FLT_MAX)) {
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (float %s) 'PEAK-LEVEL)\n"),
                                 Internat::ToString(maxPeakLevel));
      }

      (mCurNumChannels > 1)?
         cmd += wxString::Format(wxT("(putprop '*SELECTION* (vector %s) 'RMS)\n"), rmsString) :
         cmd += wxString::Format(wxT("(putprop '*SELECTION* %s 'RMS)\n"), rmsString);
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
                          (int)mCurNumChannels,
                          curLen, mCurTrack[0]->GetRate());
   }

   // Restore the Nyquist sixteenth note symbol for Generate plug-ins.
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=490.
   if (GetType() == EffectTypeGenerate) {
      cmd += wxT("(setf s 0.25)\n");
   }

   if (mDebug || mTrace) {
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

   for (unsigned int j = 0; j < mControls.size(); j++) {
      if (mControls[j].type == NYQ_CTRL_REAL || mControls[j].type == NYQ_CTRL_FLOAT_TEXT) {
         // We use Internat::ToString() rather than "%f" here because we
         // always have to use the dot as decimal separator when giving
         // numbers to Nyquist, whereas using "%f" will use the user's
         // decimal separator which may be a comma in some countries.
         cmd += wxString::Format(wxT("(setf %s %s)\n"),
                                 mControls[j].var,
                                 Internat::ToString(mControls[j].val, 14));
      }
      else if (mControls[j].type == NYQ_CTRL_INT ||
            mControls[j].type == NYQ_CTRL_INT_TEXT ||
            mControls[j].type == NYQ_CTRL_CHOICE) {
         cmd += wxString::Format(wxT("(setf %s %d)\n"),
                                 mControls[j].var,
                                 (int)(mControls[j].val));
      }
      else if (mControls[j].type == NYQ_CTRL_STRING) {
         cmd += wxT("(setf ");
         // restrict variable names to 7-bit ASCII:
         cmd += mControls[j].var;
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

      if (mDebug || mTrace) {
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

   // Put the fetch buffers in a clean initial state
   for (size_t i = 0; i < mCurNumChannels; i++)
      mCurBuffer[i].Free();

   // Guarantee release of memory when done
   auto cleanup = finally( [&] {
      for (size_t i = 0; i < mCurNumChannels; i++)
         mCurBuffer[i].Free();
   } );

   // Evaluate the expression, which may invoke the get callback, but often does
   // not, leaving that to delayed evaluation of the output sound
   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   // If we're not showing debug window, log errors and warnings:
   if (!mDebugOutput.IsEmpty() && !mDebug && !mTrace) {
      /* i18n-hint: An effect "returned" a message.*/
      wxLogMessage(_("\'%s\' returned:\n%s"), mName, mDebugOutput);
   }

   // Audacity has no idea how long Nyquist processing will take, but
   // can monitor audio being returned.
   // Anything other than audio should be returned almost instantly
   // so notify the user that process has completed (bug 558)
   if ((rval != nyx_audio) && ((mCount + mCurNumChannels) == mNumSelectedChannels)) {
      if (mCurNumChannels == 1) {
         TrackProgress(mCount, 1.0, _("Processing complete."));
      }
      else {
         TrackGroupProgress(mCount, 1.0, _("Processing complete."));
      }
   }

   if (rval == nyx_error) {
      // Return value is not valid type.
      // Show error in debug window if trace enabled, otherwise log.
      if (mTrace) {
         /* i18n-hint: "%s" is replaced by name of plug-in.*/
         mDebugOutput = wxString::Format(_("nyx_error returned from %s.\n"),
                                         mName.IsEmpty()? _("plug-in") : mName) + mDebugOutput;
         mDebug = true;
         return false;
      }
      else {
         wxLogMessage(wxT("Nyquist returned nyx_error."));
      }
      return true;
   }

   if (rval == nyx_string) {
      wxString msg = NyquistToWxString(nyx_get_string());
      if (!msg.IsEmpty())  // Not currently a documented feature, but could be useful as a No-Op.
         Effect::MessageBox(msg);

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
      Effect::MessageBox(str);
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   if (rval == nyx_int) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %d")),
                 nyx_get_int());
      Effect::MessageBox(str);
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

         ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str), -2);
      }
      return (GetType() != EffectTypeProcess || mIsPrompt);
   }

   wxASSERT(rval == nyx_audio);

   int outChannels = nyx_get_audio_num_channels();
   if (outChannels > (int)mCurNumChannels) {
      Effect::MessageBox(_("Nyquist returned too many audio channels.\n"));
      return false;
   }

   if (outChannels == -1) {
      Effect::MessageBox(_("Nyquist returned one audio channel as an array.\n"));
      return false;
   }

   if (outChannels == 0) {
      Effect::MessageBox(_("Nyquist returned an empty array.\n"));
      return false;
   }

   std::unique_ptr<WaveTrack> outputTrack[2];

   double rate = mCurTrack[0]->GetRate();
   for (int i = 0; i < outChannels; i++) {
      sampleFormat format = mCurTrack[i]->GetSampleFormat();

      if (outChannels == (int)mCurNumChannels) {
         rate = mCurTrack[i]->GetRate();
      }

      outputTrack[i] = mFactory->NewWaveTrack(format, rate);
      outputTrack[i]->SetWaveColorIndex( mCurTrack[i]->GetWaveColorIndex() );

      // Clean the initial buffer states again for the get callbacks
      // -- is this really needed?
      mCurBuffer[i].Free();
   }

   // Now fully evaluate the sound
   int success;
   {
      auto vr0 = valueRestorer( mOutputTrack[0], outputTrack[0].get() );
      auto vr1 = valueRestorer( mOutputTrack[1], outputTrack[1].get() );
      success = nyx_get_audio(StaticPutCallback, (void *)this);
   }

   // See if GetCallback found read errors
   if (mFailedFileName.IsOk())
      // re-construct an exception
      // I wish I had std::exception_ptr instead
      // and could re-throw any AudacityException
      throw FileException{
         FileException::Cause::Read, mFailedFileName };
   else if (mError)
      // what, then?
      success = false;

   if (!success)
      return false;

   for (int i = 0; i < outChannels; i++) {
      outputTrack[i]->Flush();
      mOutputTime = outputTrack[i]->GetEndTime();

      if (mOutputTime <= 0) {
         Effect::MessageBox(_("Nyquist returned nil audio.\n"));
         return true;
      }
   }

   for (size_t i = 0; i < mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == (int)mCurNumChannels) {
         out = outputTrack[i].get();
      }
      else {
         out = outputTrack[0].get();
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
       // TODO: internationalization of strings from Nyquist effects, at least
       // from those shipped with Audacity
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

wxArrayString NyquistEffect::ParseChoice(const wxString & text)
{
   if (text[0] == wxT('(')) {
      // New style:  expecting a Lisp-like list of strings
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &choices = tzer.tokens;
      for (auto &choice : choices)
         choice = UnQuote(choice);
      return choices;
   }
   else {
      // Old style: expecting a comma-separated list of
      // un-internationalized names, ignoring leading and trailing spaces
      // on each; and the whole may be quoted
      auto choices = wxStringTokenize(
         text[0] == wxT('"') ? text.Mid(1, text.Length() - 2) : text,
         wxT(",")
      );
      for (auto &choice : choices)
         choice = choice.Trim(true).Trim(false);
      return choices;
   }
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

wxString NyquistEffect::UnQuote(
   const wxString &s, bool allowParens, bool translate)
{
   int len = s.Length();
   if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
      auto unquoted = s.Mid(1, len - 2);
      return translate ? GetCustomTranslation( unquoted ) : unquoted;
   }
   else if (allowParens &&
            len >= 2 && s[0] == wxT('(') && s[len - 1] == wxT(')')) {
      Tokenizer tzer;
      tzer.Tokenize(s, true, 1, 1);
      auto &tokens = tzer.tokens;
      if (tokens.size() > 1)
         // Assume the first token was _ -- we don't check that
         // And the second is the string, which is internationalized
         return UnQuote( tokens[1], false, true );
      else
         return {};
   }
   else
      // If string was not quoted, assume no translation exists
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

bool NyquistEffect::Tokenizer::Tokenize(
   const wxString &line, bool eof,
   size_t trimStart, size_t trimEnd)
{
   auto endToken = [&]{
      if (!tok.empty()) {
         tokens.push_back(tok);
         tok = wxT("");
      }
   };

   for (auto c :
        make_iterator_range(line.begin() + trimStart, line.end() - trimEnd)) {
      if (q && c == wxT('\\')) {
         // begin escaped character, only within quotes
         sl = true;
         continue;
      }

      if (!sl && !paren && c == wxT('"')) {
         if (!q)
            // finish previous token; begin token, including the delimiter
            endToken(), q = true, tok += c;
         else
            // end token, including the delimiter
            tok += c, q = false, endToken();
      }
      else if (!q && !paren && (c == wxT(' ') || c == wxT('\t')))
         // Separate tokens; don't accumulate this character
         endToken();
      else if (!q && c == wxT('(')) {
         if (++paren == 1)
            // finish previous token; begin list, including the delimiter
            endToken(), tok += c;
         else
            // defer tokenizing of nested list to a later pass over the token
            tok += c;
      }
      else if (!q && c == wxT(')')) {
         if (--paren == 0)
            // finish list, including the delimiter
            tok += c, endToken();
         else if (paren < 0)
            // forgive unbalanced right paren
            paren = 0, endToken();
         else
            // nested list; deferred tokenizing
            tok += c;
      }
      else
         tok += c;

      sl = false;
   }

   if (eof || (!q && !paren)) {
      endToken();
      return true;
   }
   else {
      // End of line but not of file, and a string or list is yet unclosed
      // If a string, accumulate a newline character
      if (q)
         tok += wxT('\n');
      return false;
   }
}

bool NyquistEffect::Parse(
   Tokenizer &tzer, const wxString &line, bool eof, bool first)
{
   if ( !tzer.Tokenize(line, eof, first ? 1 : 0, 0) )
      return false;

   const auto &tokens = tzer.tokens;
   int len = tokens.size();
   if (len < 1) {
      return true;
   }

   // Consistency decision is for "plug-in" as the correct spelling
   // "plugin" (deprecated) is allowed as an undocumented convenience.
   if (len == 2 && tokens[0] == wxT("nyquist") &&
      (tokens[1] == wxT("plug-in") || tokens[1] == wxT("plugin"))) {
      mOK = true;
      return true;
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
      return true;
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
      return true;
   }

   // TODO: Update documentation.

   if (len >= 2 && tokens[0] == wxT("debugflags")) {
      for (int i = 1; i < len; i++) {
         // "trace" sets *tracenable* (LISP) or *sal-traceback* (SAL)
         // and displays debug window IF there is anything to show.
         if (tokens[i] == wxT("trace")) {
            mTrace = true;
         }
         else if (tokens[i] == wxT("notrace")) {
            mTrace = false;
         }
         else if (tokens[i] == wxT("compiler")) {
            mCompiler = true;
         }
         else if (tokens[i] == wxT("nocompiler")) {
            mCompiler = false;
         }
      }
      return true;
   }

   // We support versions 1, 2 and 3
   // (Version 2 added support for string parameters.)
   // (Version 3 added support for choice parameters.)
   // (Version 4 added support for project/track/selection information.)
   if (len >= 2 && tokens[0] == wxT("version")) {
      long v;
      tokens[1].ToLong(&v);
      if (v < 1 || v > 4) {
         // This is an unsupported plug-in version
         mOK = false;
         mInitError.Format(
            _("This version of Audacity does not support Nyquist plug-in version %ld"),
            v
         );
         return true;
      }
      mVersion = (int) v;
   }

   if (len >= 2 && tokens[0] == wxT("name")) {
      mName = UnQuote(tokens[1]);
      if (mName.EndsWith(wxT("...")))
         mName = mName.RemoveLast(3);
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("action")) {
      mAction = UnQuote(tokens[1]);
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("info")) {
      mInfo = UnQuote(tokens[1]);
      return true;
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
      return true;
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
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("restoresplits")) {
      long v;
      // Splits are restored by default. Set to 0 to prevent.
      tokens[1].ToLong(&v);
      mRestoreSplits = !!v;
      return true;
   }
#endif

   if (len >= 2 && tokens[0] == wxT("author")) {
      mAuthor = UnQuote(tokens[1]);
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("copyright")) {
      mCopyright = UnQuote(tokens[1]);
      return true;
   }

   // TODO: Document.
   // Page name in Audacity development manual
   if (len >= 2 && tokens[0] == wxT("manpage")) {
      // do not translate
      mManPage = UnQuote(tokens[1], false, false);
      return true;
   }

   // TODO: Document.
   // Local Help file
   if (len >= 2 && tokens[0] == wxT("helpfile")) {
      // do not translate
      mHelpFile = UnQuote(tokens[1], false, false);
      return true;
   }

   // TODO: Document.
   // Debug button may be disabled for release plug-ins.
   if (len >= 2 && tokens[0] == wxT("debugbutton")) {
      if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mDebugButton = false;
      }
      return true;
   }

   if (len >= 6 && tokens[0] == wxT("control")) {
      NyqControl ctrl;

      ctrl.var = tokens[1];
      ctrl.name = UnQuote( tokens[2] );
      // 3 is type, below
      ctrl.label = tokens[4];

      // valStr may or may not be a quoted string
      ctrl.valStr = tokens[5];
      ctrl.val = GetCtrlValue(ctrl.valStr);
      if (ctrl.valStr[0] == wxT('(') || ctrl.valStr[0] == wxT('"'))
         ctrl.valStr = UnQuote( ctrl.valStr );

      // 6 is minimum, below
      // 7 is maximum, below

      if (tokens[3] == wxT("string")) {
         ctrl.type = NYQ_CTRL_STRING;
         ctrl.label = UnQuote( ctrl.label );
      }
      else if (tokens[3] == wxT("choice")) {
         ctrl.type = NYQ_CTRL_CHOICE;
         ctrl.choices = ParseChoice(ctrl.label);
         ctrl.label = wxT("");
      }
      else {
         ctrl.label = UnQuote( ctrl.label );

         if (len < 8) {
            return true;
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
            str.Printf(_("Bad Nyquist 'control' type specification: '%s' in plug-in file '%s'.\nControl not created."),
                       tokens[3], mFileName.GetFullPath());

            // Too disturbing to show alert before Audacity frame is up.
            //    Effect::MessageBox(str, wxT("Nyquist Warning"), wxOK | wxICON_EXCLAMATION);

            // Note that the AudacityApp's mLogger has not yet been created,
            // so this brings up an alert box, but after the Audacity frame is up.
            wxLogWarning(str);
            return true;
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
            ctrl.high = ctrl.low;
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
         mControls.push_back(ctrl);
      }
   }

   if (len >= 2 && tokens[0] == wxT("categories")) {
      for (size_t i = 1; i < tokens.GetCount(); ++i) {
         mCategories.Add(tokens[i]);
      }
   }
   return true;
}

bool NyquistEffect::ParseProgram(wxInputStream & stream)
{
   if (!stream.IsOk())
   {
      mInitError = _("Could not open file");
      return false;
   }

   wxTextInputStream pgm(stream, wxT(" \t"), wxConvUTF8);

   mCmd = wxT("");
   mIsSal = false;
   mControls.clear();
   mCategories.Clear();
   mIsSpectral = false;
   mManPage = wxEmptyString; // If not wxEmptyString, must be a page in the Audacity manual.
   mHelpFile = wxEmptyString; // If not wxEmptyString, must be a valid HTML help file.
   mHelpFileExists = false;
   mDebug = false;
   mTrace = false;
   mDebugButton = true;    // Debug button enabled by default.
   mEnablePreview = true;  // Preview button enabled by default.

   mFoundType = false;
   while (!stream.Eof() && stream.IsOk())
   {
      bool dollar = false;
      wxString line = pgm.ReadLine().Trim(false);
      if (line.Length() > 1 &&
          // New in 2.3.0:  allow magic comment lines to start with $
          // The trick is that xgettext will not consider such lines comments
          // and will extract the strings they contain
          (line[0] == wxT(';') ||
           (dollar = (line[0] == wxT('$')))))
      {
         Tokenizer tzer;
         unsigned nLines = 1;
         bool done;
         do
            // Allow run-ons only for new $ format header lines
            done = Parse(tzer, line, !dollar || stream.Eof(), nLines == 1);
         while(!done &&
            (line = pgm.ReadLine().Trim(false), ++nLines, true));

         // Don't pass these lines to the interpreter, so it doesn't get confused
         // by $, but pass blanks,
         // so that SAL effects compile with proper line numbers
         while (nLines --)
            mCmd += wxT('\n');
      }
      else
      {
         if(!mFoundType && line.Length() > 0) {
            if (line[0] == wxT('(') ||
                (line[0] == wxT('#') && line.Length() > 1 && line[1] == wxT('|')))
            {
               mIsSal = false;
               mFoundType = true;
            }
            else if (line.Upper().Find(wxT("RETURN")) != wxNOT_FOUND)
            {
               mIsSal = true;
               mFoundType = true;
            }
         }
         mCmd += line + wxT("\n");
      }
   }
   if (!mFoundType && mIsPrompt)
   {
      /* i1n-hint: SAL and LISP are names for variant syntaxes for the
       Nyquist programming language.  Leave them, and 'return', untranslated. */
      Effect::MessageBox(_("Your code looks like SAL syntax, but there is no \'return\' statement.\n\
For SAL, use a return statement such as:\n\treturn *track* * 0.1\n\
or for LISP, begin with an open parenthesis such as:\n\t(mult *track* 0.1)\n ."),
                  Effect::DefaultMessageBoxStyle,
                   _("Error in Nyquist code"));
      /* i18n-hint: refers to programming "languages" */
      mInitError = _("Could not determine language");
      return false;
      // Else just throw it at Nyquist to see what happens
   }

   return true;
}

void NyquistEffect::ParseFile()
{
   wxFileInputStream stream(mFileName.GetFullPath());

   ParseProgram(stream);
}

bool NyquistEffect::ParseCommand(const wxString & cmd)
{
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

      if (mCurBufferLen[ch] < (size_t) len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      mCurBufferLen[ch] =
         limitSampleBufferSize( mCurBufferLen[ch],
                                mCurStart[ch] + mCurLen - mCurBufferStart[ch] );

      mCurBuffer[ch].Allocate(mCurBufferLen[ch], floatSample);
      try {
         mCurTrack[ch]->Get(
            mCurBuffer[ch].ptr(), floatSample,
            mCurBufferStart[ch], mCurBufferLen[ch]);
      }
      catch ( const FileException& e ) {
         if ( e.cause == FileException::Cause::Read )
            mFailedFileName = e.fileName;
         mError = true;
         return -1;
      }
      catch ( ... ) {
         mError = true;
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
   // Don't let C++ exceptions propagate through the Nyquist library
   return GuardedCall<int>( [&] {
      if (channel == 0) {
         double progress = mScale*((float)(start+len)/totlen);

         if (progress > mProgressOut) {
            mProgressOut = progress;
         }

         if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
            return -1;
         }
      }

      mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len);

      return 0; // success
   }, MakeSimpleGuard( -1 ) ); // translate all exceptions into failure
}

void NyquistEffect::StaticOutputCallback(int c, void *This)
{
   ((NyquistEffect *)This)->OutputCallback(c);
}

void NyquistEffect::OutputCallback(int c)
{
   // Always collect Nyquist error messages for normal plug-ins
   if (!mRedirectOutput) {
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
   pathList.Add(FileNames::PlugInDir());

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
   for (size_t i = 0, cnt = mControls.size(); i < cnt; i++)
   {
      NyqControl & ctrl = mControls[i];

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         const wxArrayString &choices = ctrl.choices;

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
   if (mControls.size() == 0)
   {
      return true;
   }

   for (unsigned int i = 0; i < mControls.size(); i++)
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
         for (size_t i = 0; i < mControls.size(); i++)
         {
            NyqControl & ctrl = mControls[i];

            S.AddPrompt(ctrl.name + wxT(":"));

            if (ctrl.type == NYQ_CTRL_STRING)
            {
               S.AddSpace(10, 10);
            
               wxTextCtrl *item = S.Id(ID_Text + i).AddTextBox( {}, wxT(""), 12);
               item->SetValidator(wxGenericValidator(&ctrl.valStr));
            }
            else if (ctrl.type == NYQ_CTRL_CHOICE)
            {
               S.AddSpace(10, 10);

               const wxArrayString &choices = ctrl.choices;
               S.Id(ID_Choice + i).AddChoice( {}, wxT(""), &choices);
            }
            else
            {
               // Integer or Real
               if (ctrl.type == NYQ_CTRL_INT_TEXT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
               {
                  S.AddSpace(10, 10);
               }

               wxTextCtrl *item = S.Id(ID_Text+i).AddTextBox( {}, wxT(""),
                                                             (ctrl.type == NYQ_CTRL_INT_TEXT ||
                                                              ctrl.type == NYQ_CTRL_FLOAT_TEXT) ? 25 : 12);

               double range = ctrl.high - ctrl.low;

               if (ctrl.type == NYQ_CTRL_REAL || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
               {
                  // > 12 decimal places can cause rounding errors in display.
                  FloatingPointValidator<double> vld(12, &ctrl.val);
                  vld.SetRange(ctrl.low, ctrl.high);

                  // Set number of decimal places
                  auto style = range < 10 ? NumValidatorStyle::THREE_TRAILING_ZEROES :
                              range < 100 ? NumValidatorStyle::TWO_TRAILING_ZEROES :
                              NumValidatorStyle::ONE_TRAILING_ZERO;
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
                  S.Id(ID_Slider + i).AddSlider( {}, 0, ctrl.ticks, 0);
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
      if (Effect::MessageBox(_("Current program has been modified.\nDiscard changes?"),
                       wxYES_NO) == wxNO)
      {
         return;
      }
   }

   FileDialogWrapper dlog(mUIParent,
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
      Effect::MessageBox(_("File could not be loaded"));
   }
}

void NyquistEffect::OnSave(wxCommandEvent & WXUNUSED(evt))
{
   FileDialogWrapper dlog(mUIParent,
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
      Effect::MessageBox(_("File could not be saved"));
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
: wxDialogWrapper{ parent, id, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER }
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

      // TODO: use ShowInfoDialog() instead.
      // Beware this dialog MUST work with screen readers.
      item = safenew wxTextCtrl(this, -1, message,
                            wxDefaultPosition, wxSize(480, 250),
                            wxTE_MULTILINE | wxTE_READONLY);
      mainSizer->Add(item, 1, wxEXPAND | wxALL, 10);

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

// Registration of extra functions in XLisp.
#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"

static LVAL gettext()
{
   auto string = UTF8CTOWX(getstring(xlgastring()));
   xllastarg();
   return cvstring(GetCustomTranslation(string).mb_str(wxConvUTF8));
}

static LVAL ngettext()
{
   auto string1 = UTF8CTOWX(getstring(xlgastring()));
   auto string2 = UTF8CTOWX(getstring(xlgastring()));
   auto number = getfixnum(xlgafixnum());
   xllastarg();
   return cvstring(
      wxGetTranslation(string1, string2, number).mb_str(wxConvUTF8));
}

static void RegisterFunctions()
{
   // Add functions to XLisp.  Do this only once,
   // before the first call to nyx_init.
   static bool firstTime = true;
   if (firstTime) {
      firstTime = false;

      static const FUNDEF functions[] = {
         { "_", SUBR, gettext },
         { "ngettext", SUBR, ngettext },
      };

      xlbindfunctions( functions, sizeof(functions)/sizeof(*functions) );
   }
}
