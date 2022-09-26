/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProgram.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistProgram.h"

#include "NyquistProperties.h"
#include "nyx.h"

#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/txtstrm.h>
#include <wx/textctrl.h>

#include "../EffectManager.h"
#include "../../LabelTrack.h"
#include "../../ShuttleGui.h"
#include "SyncLock.h"
#include "ViewInfo.h"
#include "../../WaveTrack.h"

std::pair<bool, FilePath> NyquistProgram::CheckHelpPage() const
{
   auto paths = NyquistProperties::GetNyquistSearchPath();
   wxString fileName;

   for (size_t i = 0, cnt = paths.size(); i < cnt; i++) {
      fileName = wxFileName(paths[i] + wxT("/") + mHelpFile).GetFullPath();
      if (wxFileExists(fileName))
      {
         return { true, fileName };
      }
   }
   return { false, wxEmptyString };
}

bool NyquistProgram::Process(const AudacityProject *const project,
   NyquistEnvironment &environment, Context &context,
   EffectSettings &settings) const
{
   auto &mEffect = mControls.mEffect;

   const auto &mRedirectOutput = environment.mRedirectOutput;

   auto &mStop = environment.mStop;
   auto &mBreak = environment.mBreak;
   auto &mCont = environment.mCont;

   const auto &mIsPreviewing = context.mContext.mIsPreviewing;
   const auto &mOutputTracks = context.mContext.mOutputTracks;
   const auto &mNumWaveGroups = context.mContext.mNumWaveGroups;
   const auto &mUIParent = context.mContext.mUIParent;
   const auto &mF0 = context.mContext.mF0;
   const auto &mF1 = context.mContext.mF1;
   auto &mT0 = context.mContext.mT0;
   auto &mT1 = context.mContext.mT1;
   auto &mDebug = context.mContext.mDebug;

   // If in tool mode, then we don't do anything with the track and selection.
   const bool bOnePassTool = (GetType() == EffectTypeTool);

   bool success = true;

   // Beware!  A global!
   int nEffectsSoFar = EffectBase::nEffectsDone;

   EffectManager & em = EffectManager::Get();
   em.SetSkipStateFlag(false);

   // This code was added in a fix for bug 2392 (no preview for Nyquist)
   // It was commented out in a fix for bug 2428 (no progress dialog from a macro)
   //if (mExternal) {
   //  mProgress->Hide();
   //}

   mStop = false;
   mBreak = false;
   mCont = false;

   auto &mCount = context.mCount;
   auto &mProps = context.mProps;
   const auto &mNumSelectedChannels = context.mNumSelectedChannels;
   const auto &mProjectChanged = context.mProjectChanged;
   const auto &mOutputTime = context.mOutputTime;
   auto &mFirstInGroup = context.mFirstInGroup;

   TranslatableString initMessage;
   if (!mHelpFile.empty() && !mHelpFileExists)
      initMessage = XO(
"error: File \"%s\" specified in header but not found in plug-in path.\n")
         .Format( mHelpFile );
   auto scope{ environment.Scope(std::move(initMessage)) };

   mProps.clear();
   if (mVersion >= 4)
      mProps = NyquistProperties::Global()
         + NyquistProperties::Project(*project, mIsPreviewing)
         + NyquistProperties::Selection(*project,
            mT0, mT1, mF0, mF1, mNumSelectedChannels)
      ;

   // Nyquist Prompt does not require a selection, but effects do.
   if (!bOnePassTool && (mNumSelectedChannels == 0)) {
      auto message = XO("Audio selection required.");
      mEffect.MessageBox(
         message,
         wxOK | wxCENTRE | wxICON_EXCLAMATION,
         XO("Nyquist Error") );
   }

   std::optional<TrackIterRange<WaveTrack>> pRange;
   if (!bOnePassTool)
      pRange.emplace(mOutputTracks->Selected< WaveTrack >() + &Track::IsLeader);

   Track *gtLast = NULL;

   NyquistTrack nyquistTrack{ mEffect,
      (GetType() == EffectTypeProcess ? 0.5 : 1.0) / mNumWaveGroups
   };

   for (;
        bOnePassTool || pRange->first != pRange->second;
        (void) (!pRange || (++pRange->first, true))
   ) {
      if (!nyquistTrack.NextTrack(pRange ? *pRange->first : nullptr,
         mT0, mT1, mMaxLen)
      ){
         success = false;
         goto finish;
      }
      auto subscope{ environment.Subscope(scope) };
      if ( (mT1 >= mT0) || bOnePassTool ) {
         if (!bOnePassTool) {
            // Check whether we're in the same group as the last selected track
            Track *gt = *SyncLock::Group(nyquistTrack.CurTracks()[0]).first;
            mFirstInGroup = !gtLast || (gtLast != gt);
            gtLast = gt;
         }

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
         Finally Do{ [&]{
            // Reset previous locale
            wxSetlocale(LC_NUMERIC, prevlocale);
         } };

         success = ProcessOne(environment, context, nyquistTrack);
         if (!success || bOnePassTool)
            goto finish;
         nyquistTrack.AccumulateProgress();
      }
      mCount += nyquistTrack.CurNumChannels();
   }

   if (mOutputTime > 0.0)
      mT1 = mT0 + mOutputTime;

finish:

   // Show debug window if trace set in plug-in header and something to show.
   auto &debugOutput = environment.DebugOutput();
   mDebug = (mDebug || (mTrace && !debugOutput.empty()));

   if (mDebug && !mRedirectOutput) {
      NyquistOutputDialog dlog(mUIParent, -1,
                               mName,
                               XO("Debug Output: "),
                               Verbatim(debugOutput));
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   // Has rug been pulled from under us by some effect done within Nyquist??
   if( !bOnePassTool && ( nEffectsSoFar == EffectBase::nEffectsDone ))
      mEffect.ReplaceProcessedTracks(success);
   else{
      mEffect.ReplaceProcessedTracks(false); // Do not use the results.
      // Selection is to be set to whatever it is in the project.
      if (project) {
         auto &selectedRegion = ViewInfo::Get( *project ).selectedRegion;
         mT0 = selectedRegion.t0();
         mT1 = selectedRegion.t1();
      }
      else {
         mT0 = 0;
         mT1 = -1;
      }

   }

   if (!mProjectChanged)
      em.SetSkipStateFlag(true);

   return success;
}

bool NyquistProgram::ProcessOne(NyquistEnvironment &environment,
   Context &context, NyquistTrack &nyquistTrack) const
{
   auto &mEffect = mControls.mEffect;

   const auto mCurTrack = nyquistTrack.CurTracks();
   const auto mCurLen = nyquistTrack.CurLength();
   const auto mCurNumChannels = nyquistTrack.CurNumChannels();

   const auto &mProps = context.mProps;
   const auto &mT0 = context.mContext.mT0;
   const auto &mT1 = context.mContext.mT1;
   const auto &mInputTracks = context.mContext.mInputTracks;
   const auto &mOutputTracks = context.mContext.mOutputTracks;
   const auto &mCount = context.mCount;
   const auto &mNumSelectedChannels = context.mNumSelectedChannels;
   auto &mProjectChanged = context.mProjectChanged;
   auto &mOutputTime = context.mOutputTime;
   auto &mFirstInGroup = context.mFirstInGroup;
   auto &mTrackIndex = context.mTrackIndex;
   auto &mDebug = context.mContext.mDebug;
   const auto &mExternal = context.mExternal;

   nyx_rval rval;
   auto cmd = wxString{ "(snd-set-latency  0.1)" }
      // Assign the symbols for the track, and the sixteenth note
      + NyquistProperties::TrackNameAssignment(GetType(), mVersion)
      + mProps
   ;

   if ((mVersion >= 4) && (GetType() != EffectTypeTool))
      cmd += NyquistProperties::Track(nyquistTrack, mT0, mT1, ++mTrackIndex);

   // If in tool mode, then we don't do anything with the track and selection.
   if (GetType() == EffectTypeTool) {
      nyx_set_audio_params(44100, 0);
   }
   else if (GetType() == EffectTypeGenerate) {
      nyx_set_audio_params(mCurTrack[0]->GetRate(), 0);
   }
   else {
      auto curLen = mCurLen.as_long_long();
      nyx_set_audio_params(mCurTrack[0]->GetRate(), curLen);

      nyx_set_input_audio(NyquistTrack::StaticGetCallback, &nyquistTrack,
         (int)mCurNumChannels, curLen, mCurTrack[0]->GetRate());
   }

   // Restore the Nyquist sixteenth note symbol for Generate plug-ins.
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=490.
   if (GetType() == EffectTypeGenerate)
      cmd += NyquistFormatting::Assignments{
         NyquistProperties::restoreSixteenth };

   cmd += GetControls().Expression(GetBindings());

   const bool trace = mDebug || mTrace;
   if (!(mIsSal && trace))
      cmd += NyquistProperties::TraceAssignments(trace, mExternal);
   if (mIsSal)
      cmd += NyquistProperties::SalCommand(trace, mCompiler, mCmd);
   else
      cmd += mCmd;

   // Evaluate the expression, which may invoke the get callback, but often does
   // not, leaving that to delayed evaluation of the output sound
   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   // If we're not showing debug window, log errors and warnings:
   const auto &output = environment.DebugOutput();
   if (!output.empty() && !mDebug && !mTrace) {
      /* i18n-hint: An effect "returned" a message.*/
      wxLogMessage(wxT("\'%s\' returned:\n%s"),
         mName.Translation(), output);
   }

   // Audacity has no idea how long Nyquist processing will take, but
   // can monitor audio being returned.
   // Anything other than audio should be returned almost instantly
   // so notify the user that process has completed (bug 558)
   if ((rval != nyx_audio) && ((mCount + mCurNumChannels) == mNumSelectedChannels)) {
      if (mCurNumChannels == 1) {
         mEffect.TrackProgress(mCount, 1.0, XO("Processing complete."));
      }
      else {
         mEffect.TrackGroupProgress(mCount, 1.0, XO("Processing complete."));
      }
   }

   if ((rval == nyx_audio) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      environment.PrependDebug(
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return audio from Nyquist.\n"));
      rval = nyx_error;
   }

   if ((rval == nyx_labels) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      environment.PrependDebug(
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return labels from Nyquist.\n"));
      rval = nyx_error;
   }

   if (rval == nyx_error) {
      // Return value is not valid type.
      // Show error in debug window if trace enabled, otherwise log.
      if (mTrace) {
         environment.PrependDebug(
            /* i18n-hint: "%s" is replaced by name of plug-in.*/
            XO("nyx_error returned from %s.\n")
               .Format( mName.empty() ? XO("plug-in") : mName ));
         mDebug = true;
      }
      else {
         wxLogMessage(
            "Nyquist returned nyx_error:\n%s", environment.DebugOutput());
      }
      return false;
   }

   if (rval == nyx_list) {
      wxLogMessage("Nyquist returned nyx_list");
      if (GetType() == EffectTypeTool) {
         mProjectChanged = true;
      } else {
         mEffect.MessageBox(XO("Nyquist returned a list.") );
      }
      return true;
   }

   const auto acceptAll = context.mAcceptsAll;

   if (rval == nyx_string) {
      // Assume the string has already been translated within the Lisp runtime
      // if necessary, by one of the gettext functions defined below, before it
      // is communicated back to C++
      auto msg = Verbatim( NyquistToWxString(nyx_get_string()) );
      if (!msg.empty()) { // Empty string may be used as a No-Op return value.
         mEffect.MessageBox( msg );
      }
      else if (GetType() == EffectTypeTool) {
         // ;tools may change the project with aud-do commands so
         // it is essential that the state is added to history.
         mProjectChanged = true;
         return true;
      }
      else {
         // A true no-op.
         return true;
      }
   }

   if (rval == nyx_double) {
      auto str = XO("Nyquist returned the value: %f")
         .Format(nyx_get_double());
      mEffect.MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_int) {
      auto str = XO("Nyquist returned the value: %d")
         .Format(nyx_get_int());
      mEffect.MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_labels) {
      mProjectChanged = true;
      unsigned int numLabels = nyx_get_num_labels();
      unsigned int l;
      auto ltrack = * mOutputTracks->Any<LabelTrack>().begin();
      if (!ltrack) {
         auto newTrack = std::make_shared<LabelTrack>();
         //new track name should be unique among the names in the list of input tracks, not output
         newTrack->SetName(mInputTracks->MakeUniqueTrackName(LabelTrack::GetDefaultName()));
         ltrack = static_cast<LabelTrack*>(
            mEffect.AddToOutputTracks(newTrack));
      }

      for (l = 0; l < numLabels; l++) {
         double t0, t1;
         const char *str;

         // PRL:  to do:
         // let Nyquist analyzers define more complicated selections
         nyx_get_label(l, &t0, &t1, &str);

         ltrack->AddLabel(SelectedRegion(t0 + mT0, t1 + mT0), UTF8CTOWX(str));
      }
      return acceptAll;
   }

   wxASSERT(rval == nyx_audio);

   int outChannels = nyx_get_audio_num_channels();
   if (outChannels > (int)mCurNumChannels) {
      mEffect.MessageBox( XO("Nyquist returned too many audio channels.\n") );
      return false;
   }

   if (outChannels == -1) {
      mEffect.MessageBox(
         XO("Nyquist returned one audio channel as an array.\n") );
      return false;
   }
 
   if (outChannels == 0) {
      mEffect.MessageBox( XO("Nyquist returned an empty array.\n") );
      return false;
   }

   // May throw other errors
   auto outputTracks = nyquistTrack.GetResult(outChannels, mOutputTime);
   if (outputTracks.empty())
      return false;

   for (size_t i = 0; i < mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == (int)mCurNumChannels)
         out = outputTracks[i].get();
      else
         out = outputTracks[0].get();

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
         for (auto t : SyncLock::Group(mCurTrack[i]))
         {
            if (!t->GetSelected() && SyncLock::IsSyncLockSelected(t)) {
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

wxString NyquistProgram::NyquistToWxString(const char *nyqString)
{
    wxString str(nyqString, wxConvUTF8);
    if (nyqString != NULL && nyqString[0] && str.empty()) {
        // invalid UTF-8 string, convert as Latin-1
        str = _("[Warning: Nyquist returned invalid UTF-8 string, converted here as Latin-1]");
       // TODO: internationalization of strings from Nyquist effects, at least
       // from those shipped with Audacity
        str += LAT1CTOWX(nyqString);
    }
    return str;
}

bool NyquistProgram::Parse(wxInputStream & stream)
{
   if (!stream.IsOk())
   {
      mInitError = XO("Could not open file");
      return false;
   }

   auto &mEffect = mControls.mEffect;

   wxTextInputStream pgm(stream, wxT(" \t"), wxConvAuto());

   mCmd = wxT("");
   mCmd.Alloc(10000);
   mIsSal = false;
   GetControls().clear();
   GetBindings().clear();
   mCategories.clear();
   mIsSpectral = false;
   mManPage = wxEmptyString; // If not wxEmptyString, must be a page in the Audacity manual.
   mHelpFile = wxEmptyString; // If not wxEmptyString, must be a valid HTML help file.
   mHelpFileExists = false;
   mTrace = false;
   mDebugButton = true;    // Debug button enabled by default.
   mControls.mEnablePreview = true;  // Preview button enabled by default.

   // Bug 1934.
   // All Nyquist plug-ins should have a ';type' field, but if they don't we default to
   // being an Effect.
   mType = EffectTypeProcess;

   mFoundType = false;
   while (!stream.Eof() && stream.IsOk())
   {
      wxString line = pgm.ReadLine();
      if (line.length() > 1 &&
          // New in 2.3.0:  allow magic comment lines to start with $
          // The trick is that xgettext will not consider such lines comments
          // and will extract the strings they contain
          (line[0] == wxT(';') || line[0] == wxT('$')) )
      {
         NyquistParser::Tokenizer tzer;
         unsigned nLines = 1;
         bool done;
         // Allow continuations within control lines.
         bool control =
            line[0] == wxT('$') || line.StartsWith( wxT(";control") );
         do
            done = NyquistParser::Parse(
               tzer, line, !control || stream.Eof(), nLines == 1);
         while(!done &&
            (line = pgm.ReadLine(), ++nLines, true));

         // Don't pass these lines to the interpreter, so it doesn't get confused
         // by $, but pass blanks,
         // so that SAL effects compile with proper line numbers
         while (nLines --)
            mCmd += wxT('\n');
      }
      else
      {
         if(!mFoundType && line.length() > 0) {
            if (line[0] == wxT('(') ||
                (line[0] == wxT('#') && line.length() > 1 && line[1] == wxT('|')))
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
   if (!mFoundType && !mEffect.RecoverParseTypeFailed())
      return false;

   const auto helpStuff = CheckHelpPage();
   mHelpFileExists = helpStuff.first;
   mHelpPage       = helpStuff.second;

   return true;
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
                                       const TranslatableString & title,
                                       const TranslatableString & prompt,
                                       const TranslatableString &message)
: wxDialogWrapper{ parent, id, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER }
{
   SetName();

   ShuttleGui S{ this, eIsCreating };
   {
      S.SetBorder(10);

      S.AddVariableText( prompt, false, wxALIGN_LEFT | wxLEFT | wxTOP | wxRIGHT );

      // TODO: use ShowInfoDialog() instead.
      // Beware this dialog MUST work with screen readers.
      S.Prop( 1 )
         .Position(wxEXPAND | wxALL)
         .MinSize( { 480, 250 } )
         .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
         .AddTextWindow( message.Translation() );

      S.SetBorder( 5 );

      S.StartHorizontalLay(wxALIGN_CENTRE | wxLEFT | wxBOTTOM | wxRIGHT, 0 );
      {
         /* i18n-hint: In most languages OK is to be translated as OK.  It appears on a button.*/
         S.Id(wxID_OK).AddButton( XXO("OK"), wxALIGN_CENTRE, true );
      }
      S.EndHorizontalLay();

   }

   SetAutoLayout(true);
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
}

// ============================================================================
// NyquistOutputDialog implementation
// ============================================================================

void NyquistOutputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
}
