/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistPrompt.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistPrompt.h"

#include <wx/textdlg.h>
#include "BasicUI.h"
#include "../EffectManager.h"
#include "PluginManager.h"
#include "../../ShuttleAutomation.h"
#include "../../ShuttleGui.h"

enum
{
   ID_Load = 10001,
   ID_Save,
};

static const wxChar *KEY_Command = wxT("Command");
static const wxChar *KEY_Parameters = wxT("Parameters");

BEGIN_EVENT_TABLE(NyquistPrompt, NyquistEffect)
   EVT_BUTTON(ID_Load, NyquistPrompt::OnLoad)
   EVT_BUTTON(ID_Save, NyquistPrompt::OnSave)
END_EVENT_TABLE()

NyquistPrompt::NyquistPrompt() : NyquistEffect{ NYQUIST_PROMPT_ID }
{
   // Interactive Nyquist
   mType = EffectTypeTool;
   mIsTool = true;
   mPromptName = mName;
   mPromptType = mType;
   mOK = true;
}

PluginPath NyquistPrompt::GetPath() const
{
   return NYQUIST_PROMPT_ID;
}

ComponentInterfaceSymbol NyquistPrompt::GetSymbol() const
{
   return { NYQUIST_PROMPT_ID, NYQUIST_PROMPT_NAME };
}

VendorSymbol NyquistPrompt::GetVendor() const
{
   return XO("Audacity");
}

ManualPageID NyquistPrompt::ManualPage() const
{
   return wxString("Nyquist_Prompt");
}

bool NyquistPrompt::IsInteractive() const
{
   return true;
}

bool NyquistPrompt::IsDefault() const
{
   return true;
}

bool NyquistPrompt::DoVisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &settings) const
{
   visitor.Define( mInputCmd, KEY_Command, wxString{} );
   visitor.Define( mParameters, KEY_Parameters, wxString{} );
   return true;
}

bool NyquistPrompt::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
{
   parms.Write(KEY_Command, mInputCmd);
   parms.Write(KEY_Parameters, mParameters);

   return true;
}

bool NyquistPrompt::DoLoadSettings(
   const CommandParameters *pParms, EffectSettings &settings)
{
   // Due to a constness problem that happens when using the prompt, we need
   // to be ready to switch the params to a local instance.
   CommandParameters localParms;

   pParms->Read(KEY_Command, &mInputCmd, wxEmptyString);
   pParms->Read(KEY_Parameters, &mParameters, wxEmptyString);

   if (!mInputCmd.empty())
   {
      ParseCommand(mInputCmd);
   }

   if (!mParameters.empty())
   {
      pParms = &localParms;
      localParms.SetParameters(mParameters);
   }

   if (!IsBatchProcessing())
   {
      mType = EffectTypeTool;
   }

   mPromptType = mType;
   mIsTool = (mPromptType == EffectTypeTool);
   mExternal = true;

   if (!IsBatchProcessing())
   {
      return true;
   }

   return NyquistEffect::DoLoadSettings(pParms, settings);
}

bool NyquistPrompt::Init()
{
   // When Nyquist Prompt spawns an effect GUI, Init() is called for Nyquist Prompt,
   // and then again for the spawned (mExternal) effect.

   // EffectType may not be defined in script, so
   // reset each time we call the Nyquist Prompt.
   mName = mPromptName;
   // Reset effect type each time we call the Nyquist Prompt.
   mType = mPromptType;
   mIsSpectral = false;
   mDebugButton = true;    // Debug button always enabled for Nyquist Prompt.
   mEnablePreview = true;  // Preview button always enabled for Nyquist Prompt.
   mVersion = 4;
   return true;
}

bool NyquistPrompt::Process(EffectInstance &instance, EffectSettings &settings)
{
   if (mControls.size() > 0 && !IsBatchProcessing()) {
      auto &nyquistSettings = GetSettings(settings);
      auto cleanup = finally([&]{
         // Free up memory
         nyquistSettings.proxySettings = {};
      });
      NyquistEffect proxy{ NYQUIST_WORKER_ID };
      proxy.SetCommand(mInputCmd);
      proxy.SetDebug(nyquistSettings.proxyDebug);
      proxy.SetControls(move(nyquistSettings.controls));
      auto result = Delegate(proxy, nyquistSettings.proxySettings);
      if (result) {
         mT0 = proxy.GetT0();
         mT1 = proxy.GetT1();
      }
      return result;
   }
   return NyquistEffect::Process(instance, settings);
}

int NyquistPrompt::ShowHostInterface(
   wxWindow &parent, const EffectDialogFactory &factory,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   int res = wxID_APPLY;
   if (!Effect::TestUIFlags(EffectManager::kRepeatNyquistPrompt)) {
      // Show the normal prompt interface
      res = Effect::ShowHostInterface(
         parent, factory, pInstance, access, forceModal);
   }

   // Remember if the user clicked debug
   mDebug = (res == eDebugID);

   // We're done if the user clicked "Close",
   // or the program currently loaded into the prompt doesn't have a UI.
   if (!res || mControls.size() == 0 || !pInstance)
      return res;

   // Nyquist prompt was OK, but gave us some magic ;control comments to
   // reinterpret into a second dialog

   NyquistEffect effect(NYQUIST_WORKER_ID);
   effect.SetCommand(mInputCmd);
   Finally Do{[&]{
      // A second dialog will use effect as a pushed event handler.
      // wxWidgets delays window destruction until idle time.
      // Yield to destroy the dialog while effect is still in scope.
      BasicUI::Yield();
   }};

   // Must give effect its own settings to interpret, not those in access
   // Let's also give it its own instance
   auto newSettings = effect.MakeSettings();
   auto pNewInstance = effect.MakeInstance();
   auto newAccess = std::make_shared<SimpleEffectSettingsAccess>(newSettings);

   if (IsBatchProcessing()) {
      effect.SetBatchProcessing();

      CommandParameters cp;
      cp.SetParameters(mParameters);
      effect.LoadSettings(cp, newSettings);

      // Show the normal (prompt or effect) interface
      res = effect.ShowHostInterface(
         parent, factory, pNewInstance, *newAccess, forceModal);
      if (res) {
         CommandParameters cp;
         effect.SaveSettings(newSettings, cp);
         cp.GetParameters(mParameters);
      }
   }
   else {
      if (!factory)
         return 0;
      res = effect.ShowHostInterface(
         parent, factory, pNewInstance, *newAccess, false );
      if (!res)
         return 0;

      // Wrap the new settings in the old settings
      access.ModifySettings([&](EffectSettings &settings){
         auto &nyquistSettings = GetSettings(settings);
         nyquistSettings.proxySettings = std::move(newSettings);
         nyquistSettings.proxyDebug = this->mDebug;
         nyquistSettings.controls = effect.MoveControls();
      });
   }
   if (!pNewInstance)
      // Propagate the failure from nested ShowHostInterface
      pInstance.reset();
   return res;
}

bool NyquistPrompt::AcceptsAllNyquistTypes()
{
   // Allow all output from Nyquist Prompt.
   return true;
}

bool NyquistPrompt::RecoverParseTypeFailed()
{
   /* i1n-hint: SAL and LISP are names for variant syntaxes for the
    Nyquist programming language.  Leave them, and 'return', untranslated. */
   Effect::MessageBox(
      XO(
"Your code looks like SAL syntax, but there is no \'return\' statement.\n\
For SAL, use a return statement such as:\n\treturn *track* * 0.1\n\
or for LISP, begin with an open parenthesis such as:\n\t(mult *track* 0.1)\n ."),
      Effect::DefaultMessageBoxStyle,
      XO("Error in Nyquist code") );
   /* i18n-hint: refers to programming "languages" */
   mInitError = XO("Could not determine language");
   return false;
}

#if 0
bool NyquistEffect::RecoverParseTypeFailed()
{
   // Just throw it at Nyquist to see what happens
   return true;
}

void NyquistEffect::ParseFile()
{
   wxFileInputStream rawStream(mFileName.GetFullPath());
   wxBufferedInputStream stream(rawStream, 10000);

   ParseProgram(stream);
}

bool NyquistEffect::ParseCommand(const wxString & cmd)
{
   wxStringInputStream stream(cmd + wxT(" "));

   return ParseProgram(stream);
}

int NyquistEffect::StaticGetCallback(float *buffer, int channel,
                                     int64_t start, int64_t len, int64_t totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->GetCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::GetCallback(float *buffer, int ch,
                               int64_t start, int64_t len, int64_t WXUNUSED(totlen))
{
   if (mCurBuffer[ch]) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         mCurBuffer[ch].reset();
      }
   }

   if (!mCurBuffer[ch]) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

      if (mCurBufferLen[ch] < (size_t) len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      mCurBufferLen[ch] =
         limitSampleBufferSize( mCurBufferLen[ch],
                                mCurStart[ch] + mCurLen - mCurBufferStart[ch] );

      // C++20
      // mCurBuffer[ch] = std::make_unique_for_overwrite(mCurBufferLen[ch]);
      mCurBuffer[ch] = Buffer{ safenew float[ mCurBufferLen[ch] ] };
      try {
         mCurTrack[ch]->GetFloats( mCurBuffer[ch].get(),
            mCurBufferStart[ch], mCurBufferLen[ch]);
      }
      catch ( ... ) {
         // Save the exception object for re-throw when out of the library
         mpException = std::current_exception();
         return -1;
      }
   }

   // We have guaranteed above that this is nonnegative and bounded by
   // mCurBufferLen[ch]:
   auto offset = ( mCurStart[ch] + start - mCurBufferStart[ch] ).as_size_t();
   const void *src = &mCurBuffer[ch][offset];
   std::memcpy(buffer, src, len * sizeof(float));

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
                                     int64_t start, int64_t len, int64_t totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->PutCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::PutCallback(float *buffer, int channel,
                               int64_t start, int64_t len, int64_t totlen)
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
      mDebugOutputStr += (wxChar)c;
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

FilePaths NyquistEffect::GetNyquistSearchPath()
{
   const auto &audacityPathList = FileNames::AudacityPathList();
   FilePaths pathList;

   for (size_t i = 0; i < audacityPathList.size(); i++)
   {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      FileNames::AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
   }
   pathList.push_back(FileNames::PlugInDir());

   return pathList;
}
#endif

bool NyquistPrompt::TransferDataToWindow(const EffectSettings &)
{
   mUIParent->TransferDataToWindow();
   mCommandText->ChangeValue(mInputCmd);
   EnablePreview(mEnablePreview);
   return true;
}

bool NyquistPrompt::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
      return false;

   mInputCmd = mCommandText->GetValue();

   // Un-correct smart quoting, bothersomely applied in wxTextCtrl by
   // the native widget of MacOS 10.9 SDK
   const wxString left = wxT("\u201c"), right = wxT("\u201d"), dumb = '"';
   mInputCmd.Replace(left, dumb, true);
   mInputCmd.Replace(right, dumb, true);

   const wxString leftSingle = wxT("\u2018"), rightSingle = wxT("\u2019"),
      dumbSingle = '\'';
   mInputCmd.Replace(leftSingle, dumbSingle, true);
   mInputCmd.Replace(rightSingle, dumbSingle, true);

   return ParseCommand(mInputCmd);
}

#if 0
bool NyquistEffect::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
      return false;

   if (mControls.size() == 0)
   {
      return true;
   }

   for (unsigned int i = 0; i < mControls.size(); i++)
   {
      NyqControl *ctrl = &mControls[i];

      if (ctrl->type == NYQ_CTRL_STRING || ctrl->type == NYQ_CTRL_TEXT)
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

      if (ctrl->type == NYQ_CTRL_FILE)
      {
         resolveFilePath(ctrl->valStr);

         wxString path;
         if (ctrl->valStr.StartsWith("\"", &path))
         {
            // Validate if a list of quoted paths.
            if (path.EndsWith("\"", &path))
            {
               path.Replace("\"\"", "\"");
               wxStringTokenizer tokenizer(path, "\"");
               while (tokenizer.HasMoreTokens())
               {
                  wxString token = tokenizer.GetNextToken();
                  if(!validatePath(token))
                  {
                     const auto message =
                        XO("\"%s\" is not a valid file path.").Format( token );
                     Effect::MessageBox(
                        message,
                        wxOK | wxICON_EXCLAMATION | wxCENTRE,
                        XO("Error") );
                     return false;
                  }
               }
               continue;
            }
            else
            {
               const auto message =
                  /* i18n-hint: Warning that there is one quotation mark rather than a pair.*/
                  XO("Mismatched quotes in\n%s").Format( ctrl->valStr );
               Effect::MessageBox(
                  message,
                  wxOK | wxICON_EXCLAMATION | wxCENTRE,
                  XO("Error") );
               return false;
            }
         }
         // Validate a single path.
         else if (validatePath(ctrl->valStr))
         {
            continue;
         }

         // Validation failed
         const auto message =
            XO("\"%s\" is not a valid file path.").Format( ctrl->valStr );
         Effect::MessageBox(
            message,
            wxOK | wxICON_EXCLAMATION | wxCENTRE,
            XO("Error") );
         return false;
      }

      if (ctrl->type == NYQ_CTRL_TIME)
      {
         NumericTextCtrl *n = (NumericTextCtrl *) mUIParent->FindWindow(ID_Time + i);
         ctrl->val = n->GetValue();
      }

      if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->lowStr.IsSameAs(wxT("nil"), false)) {
         ctrl->low = INT_MIN;
      }
      else if ((ctrl->type == NYQ_CTRL_FLOAT_TEXT || ctrl->type == NYQ_CTRL_TIME) &&
               ctrl->lowStr.IsSameAs(wxT("nil"), false))
      {
         ctrl->low = -(FLT_MAX);
      }
      else
      {
         ctrl->low = GetCtrlValue(ctrl->lowStr);
      }

      if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->highStr.IsSameAs(wxT("nil"), false)) {
         ctrl->high = INT_MAX;
      }
      else if ((ctrl->type == NYQ_CTRL_FLOAT_TEXT || ctrl->type == NYQ_CTRL_TIME) &&
               ctrl->highStr.IsSameAs(wxT("nil"), false))
      {
         ctrl->high = FLT_MAX;
      }
      else
      {
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

#endif

std::unique_ptr<EffectUIValidator> NyquistPrompt::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &)
{
   S.StartVerticalLay();
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);

         S.AddVariableText(XO("Enter Nyquist Command: "));

         S.AddSpace(1, 1);
      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxEXPAND, 1);
      {
          mCommandText = S.Focus()
            .MinSize( { 500, 200 } )
            .AddTextWindow(wxT(""));
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxALIGN_CENTER, 0);
      {
         S.Id(ID_Load).AddButton(XXO("&Load"));
         S.Id(ID_Save).AddButton(XXO("&Save"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
   return nullptr;
}

static const FileNames::FileType
   /* i18n-hint: Nyquist is the name of a programming language */
     NyquistScripts = { XO("Nyquist scripts"), { wxT("ny") }, true }
   /* i18n-hint: Lisp is the name of a programming language */
   , LispScripts = { XO("Lisp scripts"), { wxT("lsp") }, true }
;

void NyquistPrompt::OnLoad(wxCommandEvent & WXUNUSED(evt))
{
   if (mCommandText->IsModified())
   {
      if (wxNO == Effect::MessageBox(
         XO("Current program has been modified.\nDiscard changes?"),
         wxYES_NO ) )
      {
         return;
      }
   }

   FileDialogWrapper dlog(
      mUIParent,
      XO("Load Nyquist script"),
      mFileName.GetPath(),
      wxEmptyString,
      {
         NyquistScripts,
         LispScripts,
         FileNames::TextFiles,
         FileNames::AllFiles
      },
      wxFD_OPEN | wxRESIZE_BORDER);

   if (dlog.ShowModal() != wxID_OK)
   {
      return;
   }

   mFileName = dlog.GetPath();

   if (!mCommandText->LoadFile(mFileName.GetFullPath()))
   {
      Effect::MessageBox( XO("File could not be loaded") );
   }
}

void NyquistPrompt::OnSave(wxCommandEvent & WXUNUSED(evt))
{
   FileDialogWrapper dlog(
      mUIParent,
      XO("Save Nyquist script"),
      mFileName.GetPath(),
      mFileName.GetFullName(),
      {
         NyquistScripts,
         LispScripts,
         FileNames::AllFiles
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);

   if (dlog.ShowModal() != wxID_OK)
   {
      return;
   }

   mFileName = dlog.GetPath();

   if (!mCommandText->SaveFile(mFileName.GetFullPath()))
   {
      Effect::MessageBox( XO("File could not be saved") );
   }
}
