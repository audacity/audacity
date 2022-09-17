/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistPrompt.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistPrompt.h"
#include "NyquistParser.h"

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
   auto &parser = GetParser();
   parser.mType = EffectTypeTool;
   parser.mIsTool = true;
   mPromptName = parser.mName;
   mPromptType = parser.mType;
   parser.mOK = true;
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
   auto &parser = GetParser();

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
      parser.mType = EffectTypeTool;
   }

   mPromptType = parser.mType;
   parser.mIsTool = (mPromptType == EffectTypeTool);
   mExternal = true;

   if (!IsBatchProcessing())
   {
      return true;
   }

   return NyquistEffect::DoLoadSettings(pParms, settings);
}

bool NyquistPrompt::Init()
{
   auto &parser = GetParser();

   // When Nyquist Prompt spawns an effect GUI, Init() is called for Nyquist Prompt,
   // and then again for the spawned (mExternal) effect.

   // EffectType may not be defined in script, so
   // reset each time we call the Nyquist Prompt.
   parser.mName = mPromptName;
   // Reset effect type each time we call the Nyquist Prompt.
   parser.mType = mPromptType;
   parser.mIsSpectral = false;
   parser.mDebugButton = true;    // Debug button always enabled for Nyquist Prompt.
   parser.mVersion = 4;
   return true;
}

bool NyquistPrompt::Process(EffectInstance &instance, EffectSettings &settings)
{
   if (IsInteractive() && !IsBatchProcessing()) {
      auto &nyquistSettings = GetSettings(settings);
      auto cleanup = finally([&]{
         // Free up memory
         nyquistSettings.proxySettings = {};
      });
      NyquistEffect proxy{ NYQUIST_WORKER_ID };
      proxy.SetCommand(mInputCmd);
      proxy.SetDebug(nyquistSettings.proxyDebug);
      proxy.SetControls(move(nyquistSettings.controls));
      proxy.SetBindings(move(nyquistSettings.bindings));
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
   if (!res || !IsInteractive() || !pInstance)
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
         nyquistSettings.bindings = effect.MoveBindings();
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
   auto &parser = GetParser();

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
   parser.mInitError = XO("Could not determine language");
   return false;
}

bool NyquistPrompt::TransferDataToWindow(const EffectSettings &)
{
   mUIParent->TransferDataToWindow();
   mCommandText->ChangeValue(mInputCmd);
   EnablePreview(GetControls().mEnablePreview);
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
   auto &parser = GetParser();
   auto &mFileName = parser.mFileName;

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
   auto &parser = GetParser();
   auto &mFileName = parser.mFileName;

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
