/*!********************************************************************

  Audacity: A Digital Audio Editor

  NyquistParser.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistParser.h"

#include <wx/log.h>
#include <wx/tokenzr.h>
#include <float.h>

NyquistParser::NyquistParser(const wxString &fName, Effect &effect)
   : mName{
      (fName == NYQUIST_PROMPT_ID) ? NYQUIST_PROMPT_NAME
   /* i18n-hint: It is acceptable to translate this the same as for "Nyquist Prompt" */
      : (fName == NYQUIST_WORKER_ID) ? XO("Nyquist Worker")
   // Use the file name verbatim as effect name.
   // This is only a default name, overridden if we find a $name line:
      : Verbatim(wxFileName{ fName }.GetName())
   }
   , mControls{ effect, mBindings }
{
   if (!(fName == NYQUIST_PROMPT_ID || fName == NYQUIST_WORKER_ID)) {
      mFileName = fName;
   }
}

#if 0
///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffect
//
///////////////////////////////////////////////////////////////////////////////

NyquistEffect::NyquistEffect(const wxString &fName)
   : NyquistParser{ fName, *this }
{
   if (!(fName == NYQUIST_PROMPT_ID || fName == NYQUIST_WORKER_ID)) {
      mFileModified = mFileName.GetModificationTime();
      ParseFile();

      if (!mOK && mInitError.empty())
         mInitError = XO("Ill-formed Nyquist plug-in header");
   }
}

NyquistEffect::~NyquistEffect()
{
}

// ComponentInterface implementation

PluginPath NyquistEffect::GetPath() const
{
   return mFileName.GetFullPath();
}

ComponentInterfaceSymbol NyquistEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol NyquistEffect::GetVendor() const
{
   return mAuthor;
}

wxString NyquistEffect::GetVersion() const
{
   // Are Nyquist version strings really supposed to be translatable?
   // See commit a06e561 which used XO for at least one of them
   return mReleaseVersion.Translation();
}

TranslatableString NyquistEffect::GetDescription() const
{
   return mCopyright;
}

ManualPageID NyquistEffect::ManualPage() const
{
   return mManPage;
}


std::pair<bool, FilePath> NyquistEffect::CheckHelpPage() const
{
   auto paths = NyquistEffect::GetNyquistSearchPath();
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


FilePath NyquistEffect::HelpPage() const
{
   return mHelpPage;
}

// EffectDefinitionInterface implementation

EffectType NyquistEffect::GetType() const
{
   return NyquistParser::GetType();
}
#endif

EffectType NyquistParser::GetType() const
{
   return mType;
}

#if 0
EffectType NyquistEffect::GetClassification() const
{
   if (mIsTool)
      return EffectTypeTool;
   return mType;
}

EffectFamilySymbol NyquistEffect::GetFamily() const
{
   return NYQUISTEFFECTS_FAMILY;
}

bool NyquistEffect::IsInteractive() const
{
   return mControls.size() != 0;
}

bool NyquistEffect::IsDefault() const
{
   return false;
}

bool NyquistEffect::VisitSettings(
   SettingsVisitor &visitor, EffectSettings &settings)
{
   if (auto pSa = dynamic_cast<ShuttleSetAutomation*>(&visitor))
      LoadSettings(*pSa->mpEap, settings);
   return true;
}

bool NyquistEffect::VisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &settings) const
{
   // For now we assume Nyquist can do get and set better than VisitSettings can,
   // And so we ONLY use it for getting the signature.
   if (auto pGa = dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
      SaveSettings(settings, *pGa->mpEap);
      return true;
   }
   else if (auto pSd = dynamic_cast<ShuttleGetDefinition*>(&visitor);
       !pSd)
      // must be the NullShuttle
      return true;

   // Get the "definition," only for the help or info commands
   if (mExternal)
      return true;

   return DoVisitSettings(visitor, settings);
}

bool NyquistEffect::DoVisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &) const
{
   mControls.Visit(mBindings, visitor);
   return true;
}

bool NyquistEffect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   return mControls.Save(mBindings, parms);
}

bool NyquistEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<NyquistEffect*>(this)->DoLoadSettings(&parms, settings);
}

bool NyquistEffect::DoLoadSettings(
   const CommandParameters *pParms, EffectSettings &settings)
{
   // Constants to document what the true/false values mean.
   const auto kTestOnly = true;
   const auto kTestAndSet = false;

   // badCount will encompass both actual bad values and missing values.
   // We probably never actually have bad values when using the dialogs
   // since the dialog validation will catch them.
   int badCount;
   // When batch processing, we just ignore missing/bad parameters.
   // We'll end up using defaults in those cases.
   if (!IsBatchProcessing()) {
      badCount = mControls.Load(mBindings, *pParms, kTestOnly);
      if (badCount > 0)
         return false;
   }

   badCount = mControls.Load(mBindings, *pParms, kTestAndSet);
   // We never do anything with badCount here.
   // It might be non zero, for missing parameters, and we allow that,
   // and don't distinguish that from an out-of-range value.
   return true;
}

// Effect Implementation
bool NyquistEffect::Init()
{
   // As of Audacity 2.1.2 rc1, 'spectral' effects are allowed only if
   // the selected track(s) are in a spectrogram view, and there is at
   // least one frequency bound and Spectral Selection is enabled for the
   // selected track(s) - (but don't apply to Nyquist Prompt).

   if (mIsSpectral) {
      auto *project = FindProject();
      bool bAllowSpectralEditing = false;
      bool hasSpectral = false;

      for ( auto t :
               TrackList::Get( *project ).Selected< const WaveTrack >() ) {
         // Find() not Get() to avoid creation-on-demand of views in case we are
         // only previewing
         auto pView = WaveTrackView::Find( t );
         if ( pView ) {
            const auto displays = pView->GetDisplays();
            if (displays.end() != std::find(
               displays.begin(), displays.end(),
               WaveTrackSubView::Type{ WaveTrackViewConstants::Spectrum, {} }))
               hasSpectral = true;
         }
         if ( hasSpectral &&
             (t->GetSpectrogramSettings().SpectralSelectionEnabled())) {
            bAllowSpectralEditing = true;
            break;
         }
      }

      if (!bAllowSpectralEditing || ((mF0 < 0.0) && (mF1 < 0.0))) {
         if (!hasSpectral) {
            Effect::MessageBox(
            XO("Enable track spectrogram view before\n"
            "applying 'Spectral' effects."),
            wxOK | wxICON_EXCLAMATION | wxCENTRE,
            XO("Error") );
         } else {
            Effect::MessageBox(
               XO("To use 'Spectral effects', enable 'Spectral Selection'\n"
                           "in the track Spectrogram settings and select the\n"
                           "frequency range for the effect to act on."),
               wxOK | wxICON_EXCLAMATION | wxCENTRE,
               XO("Error") );
         }
         return false;
      }
   }

   if (!mExternal) {
      //TODO: (bugs):
      // 1) If there is more than one plug-in with the same name, GetModificationTime may pick the wrong one.
      // 2) If the ;type is changed after the effect has been registered, the plug-in will appear in the wrong menu.

      //TODO: If we want to auto-add parameters from spectral selection,
      //we will need to modify this test.
      //Note that removing it stops the caching of parameter values,
      //(during this session).
      if (mFileName.GetModificationTime().IsLaterThan(mFileModified))
      {
         // If the effect has internal state, save and restore it.
         // If the effect is stateless, saving and restoring don't matter.
         auto dummySettings = MakeSettings();
         constexpr auto key = L"TemporarySettings";
         SaveUserPreset(key, dummySettings);

         mMaxLen = NYQ_MAX_LEN;
         ParseFile();
         mFileModified = mFileName.GetModificationTime();

         LoadUserPreset(key, dummySettings);
      }
   }

   return true;
}

static void RegisterFunctions();

bool NyquistEffect::Process(EffectInstance &, EffectSettings &settings)
{
   // Check for reentrant Nyquist commands.
   // I'm choosing to mark skipped Nyquist commands as successful even though
   // they are skipped.  The reason is that when Nyquist calls out to a chain,
   // and that chain contains Nyquist,  it will be clearer if the chain completes
   // skipping Nyquist, rather than doing nothing at all.
   if( mReentryCount > 0 )
      return true;

   // Restore the reentry counter (to zero) when we exit.
   auto countRestorer = valueRestorer( mReentryCount);
   mReentryCount++;
   RegisterFunctions();

   bool success = true;
   int nEffectsSoFar = nEffectsDone;
   mProjectChanged = false;
   EffectManager & em = EffectManager::Get();
   em.SetSkipStateFlag(false);

   // This code was added in a fix for bug 2392 (no preview for Nyquist)
   // It was commented out in a fix for bug 2428 (no progress dialog from a macro)
   //if (mExternal) {
   //  mProgress->Hide();
   //}

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

   // If in tool mode, then we don't do anything with the track and selection.
   const bool bOnePassTool = (GetType() == EffectTypeTool);

   // We must copy all the tracks, because Paste needs label tracks to ensure
   // correct sync-lock group behavior when the timeline is affected; then we just want
   // to operate on the selected wave tracks
   if ( !bOnePassTool )
      CopyInputTracks(true);

   mNumSelectedChannels = bOnePassTool
      ? 0
      : mOutputTracks->Selected< const WaveTrack >().size();

   mDebugOutput = {};
   if (!mHelpFile.empty() && !mHelpFileExists) {
      mDebugOutput = XO(
"error: File \"%s\" specified in header but not found in plug-in path.\n")
         .Format( mHelpFile );
   }

   using namespace NyquistFormatting;

   if (mVersion >= 4)
   {
      auto project = FindProject();

      mProps = wxEmptyString;

      mProps += wxString::Format(wxT("(putprop '*AUDACITY* (list %d %d %d) 'VERSION)\n"), AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION);
      wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
      lang = (lang.empty())
         ? Languages::GetSystemLanguageCode(FileNames::AudacityPathList())
         : lang;
      mProps += wxString::Format(wxT("(putprop '*AUDACITY* \"%s\" 'LANGUAGE)\n"), lang);

      mProps += wxString::Format(wxT("(setf *DECIMAL-SEPARATOR* #\\%c)\n"), wxNumberFormatter::GetDecimalSeparator());

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'BASE)\n"), EscapeString(FileNames::BaseDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DATA)\n"), EscapeString(FileNames::DataDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HELP)\n"), EscapeString(FileNames::HtmlHelpDir().RemoveLast()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'TEMP)\n"), EscapeString(TempDirectory::TempDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'SYS-TEMP)\n"), EscapeString(wxStandardPaths::Get().GetTempDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'DOCUMENTS)\n"), EscapeString(wxStandardPaths::Get().GetDocumentsDir()));
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'HOME)\n"), EscapeString(wxGetHomeDir()));

      auto paths = NyquistEffect::GetNyquistSearchPath();
      wxString list;
      for (size_t i = 0, cnt = paths.size(); i < cnt; i++)
      {
         list += wxT("\"") + EscapeString(paths[i]) + wxT("\" ");
      }
      list = list.RemoveLast();

      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUGIN)\n"), list);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* (list %s) 'PLUG-IN)\n"), list);
      mProps += wxString::Format(wxT("(putprop '*SYSTEM-DIR* \"%s\" 'USER-PLUG-IN)\n"),
                                 EscapeString(FileNames::PlugInDir()));

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

      mProps += wxString::Format(wxT("(putprop '*PROJECT* %d 'PROJECTS)\n"),
         (int) AllProjects{}.size());
      mProps += wxString::Format(wxT("(putprop '*PROJECT* \"%s\" 'NAME)\n"), EscapeString(project->GetProjectName()));

      int numTracks = 0;
      int numWave = 0;
      int numLabel = 0;
      int numMidi = 0;
      int numTime = 0;
      wxString waveTrackList;   // track positions of selected audio tracks.

      {
         auto countRange = TrackList::Get( *project ).Leaders();
         for (auto t : countRange) {
            t->TypeSwitch( [&](const WaveTrack *) {
               numWave++;
               if (t->GetSelected())
                  waveTrackList += wxString::Format(wxT("%d "), 1 + numTracks);
            });
            numTracks++;
         }
         numLabel = countRange.Filter<const LabelTrack>().size();
   #if defined(USE_MIDI)
         numMidi = countRange.Filter<const NoteTrack>().size();
   #endif
         numTime = countRange.Filter<const TimeTrack>().size();
      }

      // We use Internat::ToString() rather than "%g" here because we
      // always have to use the dot as decimal separator when giving
      // numbers to Nyquist, whereas using "%g" will use the user's
      // decimal separator which may be a comma in some countries.
      mProps += wxString::Format(wxT("(putprop '*PROJECT* (float %s) 'RATE)\n"),
         Internat::ToString(ProjectRate::Get(*project).GetRate()));
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

   // Nyquist Prompt does not require a selection, but effects do.
   if (!bOnePassTool && (mNumSelectedChannels == 0)) {
      auto message = XO("Audio selection required.");
      Effect::MessageBox(
         message,
         wxOK | wxCENTRE | wxICON_EXCLAMATION,
         XO("Nyquist Error") );
   }

   std::optional<TrackIterRange<WaveTrack>> pRange;
   if (!bOnePassTool)
      pRange.emplace(mOutputTracks->Selected< WaveTrack >() + &Track::IsLeader);

   // Keep track of whether the current track is first selected in its sync-lock group
   // (we have no idea what the length of the returned audio will be, so we have
   // to handle sync-lock group behavior the "old" way).
   mFirstInGroup = true;
   Track *gtLast = NULL;

   for (;
        bOnePassTool || pRange->first != pRange->second;
        (void) (!pRange || (++pRange->first, true))
   ) {
      // Prepare to accumulate more debug output in OutputCallback
      mDebugOutputStr = mDebugOutput.Translation();
      mDebugOutput = Verbatim( "%s" ).Format( std::cref( mDebugOutputStr ) );

      mCurTrack[0] = pRange ? *pRange->first : nullptr;
      mCurNumChannels = 1;
      if ( (mT1 >= mT0) || bOnePassTool ) {
         if (bOnePassTool) {
         }
         else {
            auto channels = TrackList::Channels(mCurTrack[0]);
            if (channels.size() > 1) {
               // TODO: more-than-two-channels
               // Pay attention to consistency of mNumSelectedChannels
               // with the running tally made by this loop!
               mCurNumChannels = 2;

               mCurTrack[1] = * ++ channels.first;
               if (mCurTrack[1]->GetRate() != mCurTrack[0]->GetRate()) {
                  Effect::MessageBox(
                     XO(
"Sorry, cannot apply effect on stereo tracks where the tracks don't match."),
                     wxOK | wxCENTRE );
                  success = false;
                  goto finish;
               }
               mCurStart[1] = mCurTrack[1]->TimeToLongSamples(mT0);
            }

            // Check whether we're in the same group as the last selected track
            Track *gt = *SyncLock::Group(mCurTrack[0]).first;
            mFirstInGroup = !gtLast || (gtLast != gt);
            gtLast = gt;

            mCurStart[0] = mCurTrack[0]->TimeToLongSamples(mT0);
            auto end = mCurTrack[0]->TimeToLongSamples(mT1);
            mCurLen = end - mCurStart[0];

            if (mCurLen > NYQ_MAX_LEN) {
               float hours = (float)NYQ_MAX_LEN / (44100 * 60 * 60);
               const auto message =
                  XO(
"Selection too long for Nyquist code.\nMaximum allowed selection is %ld samples\n(about %.1f hours at 44100 Hz sample rate).")
                     .Format((long)NYQ_MAX_LEN, hours);
               Effect::MessageBox(
                  message,
                  wxOK | wxCENTRE,
                  XO("Nyquist Error") );
               if (!mProjectChanged)
                  em.SetSkipStateFlag(true);
               return false;
            }

            mCurLen = std::min(mCurLen, mMaxLen);
         }

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

         if (!success || bOnePassTool) {
            goto finish;
         }
         mProgressTot += mProgressIn + mProgressOut;
      }

      mCount += mCurNumChannels;
   }

   if (mOutputTime > 0.0) {
      mT1 = mT0 + mOutputTime;
   }

finish:

   // Show debug window if trace set in plug-in header and something to show.
   mDebug = (mTrace && !mDebugOutput.Translation().empty())
      ? true : mDebug;

   if (mDebug && !mRedirectOutput) {
      NyquistOutputDialog dlog(mUIParent, -1,
                               mName,
                               XO("Debug Output: "),
                               mDebugOutput);
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   // Has rug been pulled from under us by some effect done within Nyquist??
   if( !bOnePassTool && ( nEffectsSoFar == nEffectsDone ))
      ReplaceProcessedTracks(success);
   else{
      ReplaceProcessedTracks(false); // Do not use the results.
      // Selection is to be set to whatever it is in the project.
      auto project = FindProject();
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

int NyquistEffect::ShowHostInterface(
   wxWindow &parent, const EffectDialogFactory &factory,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   int res = Effect::ShowHostInterface(
      parent, factory, pInstance, access, forceModal);
   // Remember if the user clicked debug
   mDebug = (res == eDebugID);
   return res;
}

bool NyquistEffect::EnablesDebug() const
{
   return mDebugButton;
}

// NyquistEffect implementation

bool NyquistEffect::ProcessOne()
{
   mpException = {};

   nyx_rval rval;

   wxString cmd;
   cmd += wxT("(snd-set-latency  0.1)");

   // A tool may be using AUD-DO which will potentially invalidate *TRACK*
   // so tools do not get *TRACK*.
   if (GetType() == EffectTypeTool)
      cmd += wxT("(setf S 0.25)\n");  // No Track.
   else if (mVersion >= 4) {
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
   }

   using namespace NyquistFormatting;

   if( (mVersion >= 4) && (GetType() != EffectTypeTool) ) {
      // Set the track TYPE and VIEW properties
      wxString type;
      wxString view;
      wxString bitFormat;
      wxString spectralEditp;

      mCurTrack[0]->TypeSwitch(
         [&](const WaveTrack *wt) {
            type = wxT("wave");
            spectralEditp = mCurTrack[0]->GetSpectrogramSettings().SpectralSelectionEnabled()? wxT("T") : wxT("NIL");
            view = wxT("NIL");
            // Find() not Get() to avoid creation-on-demand of views in case we are
            // only previewing
            if ( const auto pView = WaveTrackView::Find( wt ) ) {
               auto displays = pView->GetDisplays();
               auto format = [&]( decltype(displays[0]) display ) {
                  // Get the English name of the view type, without menu codes,
                  // as a string that Lisp can examine
                  return wxString::Format( wxT("\"%s\""),
                     display.name.Stripped().Debug() );
               };
               if (displays.empty())
                  ;
               else if (displays.size() == 1)
                  view = format( displays[0] );
               else {
                  view = wxT("(list");
                  for ( auto display : displays )
                     view += wxString(wxT(" ")) + format( display );
                  view += wxT(")");
               }
            }
         },
#if defined(USE_MIDI)
         [&](const NoteTrack *) {
            type = wxT("midi");
            view = wxT("\"Midi\"");
         },
#endif
         [&](const LabelTrack *) {
            type = wxT("label");
            view = wxT("\"Label\"");
         },
         [&](const TimeTrack *) {
            type = wxT("time");
            view = wxT("\"Time\"");
         }
      );

      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'INDEX)\n"), ++mTrackIndex);
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'NAME)\n"), EscapeString(mCurTrack[0]->GetName()));
      cmd += wxString::Format(wxT("(putprop '*TRACK* \"%s\" 'TYPE)\n"), type);
      // Note: "View" property may change when Audacity's choice of track views has stabilized.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'VIEW)\n"), view);
      cmd += wxString::Format(wxT("(putprop '*TRACK* %d 'CHANNELS)\n"), mCurNumChannels);

      //NOTE: Audacity 2.1.3 True if spectral selection is enabled regardless of track view.
      cmd += wxString::Format(wxT("(putprop '*TRACK* %s 'SPECTRAL-EDIT-ENABLED)\n"), spectralEditp);

      auto channels = TrackList::Channels( mCurTrack[0] );
      double startTime = channels.min( &Track::GetStartTime );
      double endTime = channels.max( &Track::GetEndTime );

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
         // Limit number of clips added to avoid argument stack overflow error (bug 2300).
         for (size_t i=0; i<ca.size(); i++) {
            if (i < 1000) {
               clips += wxString::Format(wxT("(list (float %s) (float %s))"),
                                         Internat::ToString(ca[i]->GetPlayStartTime()),
                                         Internat::ToString(ca[i]->GetPlayEndTime()));
            } else if (i == 1000) {
               // If final clip is NIL, plug-in developer knows there are more than 1000 clips in channel.
               clips += "NIL";
            } else if (i > 1000) {
               break;
            }
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
            rmsString += wxT("NIL ");
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

   cmd += mControls.Expression(mBindings);

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
      mCurBuffer[i].reset();

   // Guarantee release of memory when done
   auto cleanup = finally( [&] {
      for (size_t i = 0; i < mCurNumChannels; i++)
         mCurBuffer[i].reset();
   } );

   // Evaluate the expression, which may invoke the get callback, but often does
   // not, leaving that to delayed evaluation of the output sound
   rval = nyx_eval_expression(cmd.mb_str(wxConvUTF8));

   // If we're not showing debug window, log errors and warnings:
   const auto output = mDebugOutput.Translation();
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
         TrackProgress(mCount, 1.0, XO("Processing complete."));
      }
      else {
         TrackGroupProgress(mCount, 1.0, XO("Processing complete."));
      }
   }

   if ((rval == nyx_audio) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      mDebugOutput =
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return audio from Nyquist.\n")
         + mDebugOutput;
      rval = nyx_error;
   }

   if ((rval == nyx_labels) && (GetType() == EffectTypeTool)) {
      // Catch this first so that we can also handle other errors.
      mDebugOutput =
         /* i18n-hint: Don't translate ';type tool'.  */
         XO("';type tool' effects cannot return labels from Nyquist.\n")
         + mDebugOutput;
      rval = nyx_error;
   }

   if (rval == nyx_error) {
      // Return value is not valid type.
      // Show error in debug window if trace enabled, otherwise log.
      if (mTrace) {
         /* i18n-hint: "%s" is replaced by name of plug-in.*/
         mDebugOutput = XO("nyx_error returned from %s.\n")
            .Format( mName.empty() ? XO("plug-in") : mName )
         + mDebugOutput;
         mDebug = true;
      }
      else {
         wxLogMessage(
            "Nyquist returned nyx_error:\n%s", mDebugOutput.Translation());
      }
      return false;
   }

   if (rval == nyx_list) {
      wxLogMessage("Nyquist returned nyx_list");
      if (GetType() == EffectTypeTool) {
         mProjectChanged = true;
      } else {
         Effect::MessageBox(XO("Nyquist returned a list.") );
      }
      return true;
   }

   const bool acceptAll = AcceptsAllNyquistTypes();

   if (rval == nyx_string) {
      // Assume the string has already been translated within the Lisp runtime
      // if necessary, by one of the gettext functions defined below, before it
      // is communicated back to C++
      auto msg = Verbatim( NyquistToWxString(nyx_get_string()) );
      if (!msg.empty()) { // Empty string may be used as a No-Op return value.
         Effect::MessageBox( msg );
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

      return acceptAll;
   }

   if (rval == nyx_double) {
      auto str = XO("Nyquist returned the value: %f")
         .Format(nyx_get_double());
      Effect::MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_int) {
      auto str = XO("Nyquist returned the value: %d")
         .Format(nyx_get_int());
      Effect::MessageBox( str );
      return acceptAll;
   }

   if (rval == nyx_labels) {
      mProjectChanged = true;
      unsigned int numLabels = nyx_get_num_labels();
      unsigned int l;
      auto ltrack = * mOutputTracks->Any< LabelTrack >().begin();
      if (!ltrack) {
         auto newTrack = std::make_shared<LabelTrack>();
         //new track name should be unique among the names in the list of input tracks, not output
         newTrack->SetName(inputTracks()->MakeUniqueTrackName(LabelTrack::GetDefaultName()));
         ltrack = static_cast<LabelTrack*>(
            AddToOutputTracks(newTrack));
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
      Effect::MessageBox( XO("Nyquist returned too many audio channels.\n") );
      return false;
   }

   if (outChannels == -1) {
      Effect::MessageBox(
         XO("Nyquist returned one audio channel as an array.\n") );
      return false;
   }

   if (outChannels == 0) {
      Effect::MessageBox( XO("Nyquist returned an empty array.\n") );
      return false;
   }

   std::shared_ptr<WaveTrack> outputTrack[2];

   double rate = mCurTrack[0]->GetRate();
   for (int i = 0; i < outChannels; i++) {
      if (outChannels == (int)mCurNumChannels) {
         rate = mCurTrack[i]->GetRate();
      }

      outputTrack[i] = mCurTrack[i]->EmptyCopy();
      outputTrack[i]->SetRate( rate );

      // Clean the initial buffer states again for the get callbacks
      // -- is this really needed?
      mCurBuffer[i].reset();
   }

   // Now fully evaluate the sound
   int success;
   {
      auto vr0 = valueRestorer( mOutputTrack[0], outputTrack[0].get() );
      auto vr1 = valueRestorer( mOutputTrack[1], outputTrack[1].get() );
      success = nyx_get_audio(StaticPutCallback, (void *)this);
   }

   // See if GetCallback found read errors
   {
      auto pException = mpException;
      mpException = {};
      if (pException)
         std::rethrow_exception( pException );
   }

   if (!success)
      return false;

   for (int i = 0; i < outChannels; i++) {
      outputTrack[i]->Flush();
      mOutputTime = outputTrack[i]->GetEndTime();

      if (mOutputTime <= 0) {
         Effect::MessageBox( XO("Nyquist returned nil audio.\n") );
         return false;
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

bool NyquistEffect::AcceptsAllNyquistTypes()
{
   // True if not process type.
   // If not returning audio from process effect,
   // return first result then stop (disables preview)
   return GetType() != EffectTypeProcess;
}

// ============================================================================
// NyquistEffect Implementation
// ============================================================================

wxString NyquistEffect::NyquistToWxString(const char *nyqString)
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

#endif

std::vector<EnumValueSymbol> NyquistParser::ParseChoice(const wxString & text)
{
   std::vector<EnumValueSymbol> results;
   if (text[0] == wxT('(')) {
      // New style:  expecting a Lisp-like list of strings
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &choices = tzer.tokens;
      wxString extra;
      for (auto &choice : choices) {
         auto label = UnQuote(choice, true, &extra);
         if (extra.empty())
            results.push_back( TranslatableString{ label, {} } );
         else
            results.push_back(
               { extra, TranslatableString{ label, {} } } );
      }
   }
   else {
      // Old style: expecting a comma-separated list of
      // un-internationalized names, ignoring leading and trailing spaces
      // on each; and the whole may be quoted
      auto choices = wxStringTokenize(
         text[0] == wxT('"') ? text.Mid(1, text.length() - 2) : text,
         wxT(",")
      );
      for (auto &choice : choices)
         results.push_back( { choice.Trim(true).Trim(false) } );
   }
   return results;
}

FileExtensions NyquistParser::ParseFileExtensions(const wxString & text)
{
   // todo: error handling
   FileExtensions results;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      for (const auto &token : tzer.tokens)
         results.push_back( UnQuote( token ) );
   }
   return results;
}

FileNames::FileType NyquistParser::ParseFileType(const wxString & text)
{
   // todo: error handling
   FileNames::FileType result;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &tokens = tzer.tokens;
      if ( tokens.size() == 2 )
         result =
            { UnQuoteMsgid( tokens[0] ), ParseFileExtensions( tokens[1] ) };
   }
   return result;
}

FileNames::FileTypes NyquistParser::ParseFileTypes(const wxString & text)
{
   // todo: error handling
   FileNames::FileTypes results;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &types = tzer.tokens;
      if ( !types.empty() && types[0][0] == wxT('(') )
         for (auto &type : types)
            results.push_back( ParseFileType( type ) );
   }
   if ( results.empty() ) {
      // Old-style is a specially formatted string, maybe translated
      // Parse it for compatibility
      auto str = UnQuote( text );
      auto pieces = wxSplit( str, '|' );
      // Should have an even number
      auto size = pieces.size();
      if ( size % 2 == 1 )
         --size, pieces.pop_back();
      for ( size_t ii = 0; ii < size; ii += 2 ) {
         FileExtensions extensions;
         auto extensionStrings = wxSplit( pieces[ii + 1], ';' );
         for ( const auto &extensionString : extensionStrings )
            if ( extensionString.StartsWith( wxT("*.") ) ) {
               auto ext = extensionString.substr( 2 );
               if (ext == wxT("*"))
                  // "*.*" to match all
                  ext.clear();
               extensions.push_back( ext );
            }
         results.push_back( { Verbatim( pieces[ii] ), extensions } );
      }
   }
   return results;
}

TranslatableString NyquistParser::UnQuoteMsgid(
   const wxString &s, bool allowParens, wxString *pExtraString)
{
   if (pExtraString)
      *pExtraString = wxString{};

   int len = s.length();
   if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
      auto unquoted = s.Mid(1, len - 2);
      // Sorry, no context strings, yet
      // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
      return TranslatableString{ unquoted, {} };
   }
   else if (allowParens &&
            len >= 2 && s[0] == wxT('(') && s[len - 1] == wxT(')')) {
      Tokenizer tzer;
      tzer.Tokenize(s, true, 1, 1);
      auto &tokens = tzer.tokens;
      if (tokens.size() > 1) {
         if (pExtraString && tokens[1][0] == '(') {
            // A choice with a distinct internal string form like
            // ("InternalString" (_ "Visible string"))
            // Recur to find the two strings
            *pExtraString = UnQuote(tokens[0], false);
            return UnQuoteMsgid(tokens[1]);
         }
         else {
            // Assume the first token was _ -- we don't check that
            // And the second is the string, which is internationalized
            // Sorry, no context strings, yet
            return UnQuoteMsgid( tokens[1], false );
         }
      }
      else
         return {};
   }
   else
      // If string was not quoted, assume no translation exists
      return Verbatim( s );
}

wxString NyquistParser::UnQuote(
   const wxString &s, bool allowParens, wxString *pExtraString)
{
   return UnQuoteMsgid( s, allowParens, pExtraString ).Translation();
}

bool NyquistParser::Tokenizer::Tokenize(
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
      if (q && !sl && c == wxT('\\')) {
         // begin escaped character, only within quotes
         sl = true;
         continue;
      }

      if (!sl && c == wxT('"')) {
         // Unescaped quote
         if (!q) {
            // start of string
            if (!paren)
               // finish previous token
               endToken();
            // Include the delimiter in the token
            tok += c;
            q = true;
         }
         else {
            // end of string
            // Include the delimiter in the token
            tok += c;
            if (!paren)
               endToken();
            q = false;
         }
      }
      else if (!q && !paren && (c == wxT(' ') || c == wxT('\t')))
         // Unenclosed whitespace
         // Separate tokens; don't accumulate this character
         endToken();
      else if (!q && c == wxT(';'))
         // semicolon not in quotes, but maybe in parentheses
         // Lisp style comments with ; (but not with #| ... |#) are allowed
         // within a wrapped header multi-line, so that i18n hint comments may
         // be placed before strings and found by xgettext
         break;
      else if (!q && c == wxT('(')) {
         // Start of list or sublist
         if (++paren == 1)
            // finish previous token; begin list, including the delimiter
            endToken(), tok += c;
         else
            // defer tokenizing of nested list to a later pass over the token
            tok += c;
      }
      else if (!q && c == wxT(')')) {
         // End of list or sublist
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
      else {
         if (sl && paren)
            // Escaped character in string inside list, to be parsed again
            // Put the escape back for the next pass
            tok += wxT('\\');
         if (sl && !paren && c == 'n')
            // Convert \n to newline, the only special escape besides \\ or \"
            // But this should not be used if a string needs to localize.
            // Instead, simply put a line break in the string.
            c = '\n';
         tok += c;
      }

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

static void SetControlBounds(NyqControl &ctrl);

bool NyquistParser::Parse(
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
      wxString tok = tokens[1];
      mIsTool = false;
      if (tok == wxT("tool")) {
         mIsTool = true;
         mType = EffectTypeTool;
         // we allow
         // ;type tool
         // ;type tool process
         // ;type tool generate
         // ;type tool analyze
         // The last three are placed in the tool menu, but are processed as
         // process, generate or analyze.
         if (len >= 3)
            tok = tokens[2];
      }

      if (tok == wxT("process")) {
         mType = EffectTypeProcess;
      }
      else if (tok == wxT("generate")) {
         mType = EffectTypeGenerate;
      }
      else if (tok == wxT("analyze")) {
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
         mInitError = XO(
"This version of Audacity does not support Nyquist plug-in version %ld")
            .Format( v );
         return true;
      }
      mVersion = (int) v;
   }

   if (len >= 2 && tokens[0] == wxT("name")) {
      // Names do not yet support context strings for translations, or
      // internal names distinct from visible English names.
      // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
      auto name = UnQuote(tokens[1]);
      // Strip ... from name if it's present, perhaps in third party plug-ins
      // Menu system puts ... back if there are any controls
      // This redundant naming convention must NOT be followed for
      // shipped Nyquist effects with internationalization.  Else the msgid
      // later looked up will lack the ... and will not be found.
      if (name.EndsWith(wxT("...")))
         name = name.RemoveLast(3);
      mName = TranslatableString{ name, {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("action")) {
      mAction = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("info")) {
      mInfo = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("preview")) {
      if (tokens[1] == wxT("enabled") || tokens[1] == wxT("true")) {
         mControls.mEnablePreview = true;
         mLinear = false;
      }
      else if (tokens[1] == wxT("linear")) {
         mControls.mEnablePreview = true;
         mLinear = true;
      }
      else if (tokens[1] == wxT("selection")) {
         mControls.mEnablePreview = true;
         mPreview = true;
      }
      else if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mControls.mEnablePreview = false;
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
      mAuthor = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("release")) {
      // Value must be quoted if the release version string contains spaces.
      mReleaseVersion =
         TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("copyright")) {
      mCopyright = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   // Page name in Audacity development manual
   if (len >= 2 && tokens[0] == wxT("manpage")) {
      // do not translate
      mManPage = UnQuote(tokens[1], false);
      return true;
   }

   // Local Help file
   if (len >= 2 && tokens[0] == wxT("helpfile")) {
      // do not translate
      mHelpFile = UnQuote(tokens[1], false);
      return true;
   }

   // Debug button may be disabled for release plug-ins.
   if (len >= 2 && tokens[0] == wxT("debugbutton")) {
      if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mDebugButton = false;
      }
      return true;
   }

   using namespace NyquistFormatting;

   if (len >= 3 && tokens[0] == wxT("control")) {
      NyqControl ctrl;
      NyqValue binding;

      if (len == 3 && tokens[1] == wxT("text")) {
         ctrl.var = tokens[1];
         ctrl.label = UnQuote( tokens[2] );
         ctrl.type = NYQ_CTRL_TEXT;
      }
      else if (len >= 5)
      {
         ctrl.var = tokens[1];
         ctrl.name = UnQuote( tokens[2] );
         // 3 is type, below
         ctrl.label = tokens[4];

         // valStr may or may not be a quoted string
         binding.valStr = len > 5 ? tokens[5] : wxString{};
         binding.val = GetCtrlValue(binding.valStr);
         if (binding.valStr.length() > 0 &&
               (binding.valStr[0] == wxT('(') ||
                binding.valStr[0] == wxT('"')))
            binding.valStr = UnQuote( binding.valStr );

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
         else if (tokens[3] == wxT("file")) {
            ctrl.type = NYQ_CTRL_FILE;
            ctrl.fileTypes = ParseFileTypes(tokens[6]);
            // will determine file dialog styles:
            ctrl.highStr = UnQuote( tokens[7] );
            ctrl.label = UnQuote(ctrl.label);
         }
         else {
            ctrl.label = UnQuote( ctrl.label );

            if (len < 8) {
               return true;
            }

            if ((tokens[3] == wxT("float")) ||
                  (tokens[3] == wxT("real"))) // Deprecated
               ctrl.type = NYQ_CTRL_FLOAT;
            else if (tokens[3] == wxT("int"))
               ctrl.type = NYQ_CTRL_INT;
            else if (tokens[3] == wxT("float-text"))
               ctrl.type = NYQ_CTRL_FLOAT_TEXT;
            else if (tokens[3] == wxT("int-text"))
               ctrl.type = NYQ_CTRL_INT_TEXT;
            else if (tokens[3] == wxT("time"))
                ctrl.type = NYQ_CTRL_TIME;
            else
            {
               wxString str;
               str.Printf(wxT("Bad Nyquist 'control' type specification: '%s' in plug-in file '%s'.\nControl not created."),
                        tokens[3], mFileName.GetFullPath());

               // Too disturbing to show alert before Audacity frame is up.
               //    Effect::MessageBox(
               //       str,
               //       wxOK | wxICON_EXCLAMATION,
               //       XO("Nyquist Warning") );

               // Note that the AudacityApp's mLogger has not yet been created,
               // so this brings up an alert box, but after the Audacity frame is up.
               wxLogWarning(str);
               return true;
            }

            ctrl.lowStr = UnQuote( tokens[6] );
            if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
               ctrl.low = INT_MIN;
            }
            else if (ctrl.type == NYQ_CTRL_FLOAT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
               ctrl.low = -(FLT_MAX);
            }
            else if (ctrl.type == NYQ_CTRL_TIME && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
                ctrl.low = 0.0;
            }
            else {
               ctrl.low = GetCtrlValue(ctrl.lowStr);
            }

            ctrl.highStr = UnQuote( tokens[7] );
            if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
               ctrl.high = INT_MAX;
            }
            else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
                      ctrl.highStr.IsSameAs(wxT("nil"), false))
            {
               ctrl.high = FLT_MAX;
            }
            else {
               ctrl.high = GetCtrlValue(ctrl.highStr);
            }

            if (ctrl.high < ctrl.low) {
               ctrl.high = ctrl.low;
            }

            if (binding.val < ctrl.low) {
               binding.val = ctrl.low;
            }

            if (binding.val > ctrl.high) {
               binding.val = ctrl.high;
            }

            ctrl.ticks = 1000;
            if (ctrl.type == NYQ_CTRL_INT &&
               (ctrl.high - ctrl.low < ctrl.ticks)) {
               ctrl.ticks = (int)(ctrl.high - ctrl.low);
            }
         }
      }

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      // Skip creation of controls and bindings for spectral editing
      // Will instead initialize Lisp variables specially before processing,
      // taking values from the given selection
      if (!(ctrl.var == "control-f0" || ctrl.var == "control-f1"))
#endif
      {
         SetControlBounds(ctrl);
         mControls.emplace_back(std::move(ctrl));
         mBindings.push_back(binding);
      }
   }

   // Deprecated
   if (len >= 2 && tokens[0] == wxT("categories")) {
      for (size_t i = 1; i < tokens.size(); ++i) {
         mCategories.push_back(tokens[i]);
      }
   }
   return true;
}

void SetControlBounds(NyqControl &ctrl)
{
   using namespace NyquistFormatting;
   if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
      ctrl.low = INT_MIN;
   }
   else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
            ctrl.lowStr.IsSameAs(wxT("nil"), false))
   {
      ctrl.low = -(FLT_MAX);
   }
   else
   {
      ctrl.low = GetCtrlValue(ctrl.lowStr);
   }

   if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
      ctrl.high = INT_MAX;
   }
   else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
            ctrl.highStr.IsSameAs(wxT("nil"), false))
   {
      ctrl.high = FLT_MAX;
   }
   else
   {
      ctrl.high = GetCtrlValue(ctrl.highStr);
   }

   if (ctrl.high < ctrl.low)
   {
      ctrl.high = ctrl.low + 1;
   }

   ctrl.ticks = 1000;
   if (ctrl.type == NYQ_CTRL_INT &&
       (ctrl.high - ctrl.low < ctrl.ticks))
   {
      ctrl.ticks = (int)(ctrl.high - ctrl.low);
   }
}
