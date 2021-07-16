/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityCommand.cpp

  James Crook

*******************************************************************//**

\class AudacityCommand
\brief Base class for command in Audacity.

*//****************************************************************//**

\class AudacityCommandDialog
\brief Default dialog used for commands.  Is populated using 
ShuttleGui.

*//*******************************************************************/


#include "AudacityCommand.h"

#include "CommandContext.h"

#include <algorithm>

#include <wx/defs.h>
#include <wx/sizer.h>
#include <wx/stockitem.h>
#include <wx/string.h>
#include <wx/tglbtn.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/log.h>

#include "ConfigInterface.h"

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/AudacityMessageBox.h"

#include <unordered_map>

namespace {

AudacityCommand::VetoDialogHook &GetVetoDialogHook()
{
   static AudacityCommand::VetoDialogHook sHook = nullptr;
   return sHook;
}

}

auto AudacityCommand::SetVetoDialogHook( VetoDialogHook hook )
   -> VetoDialogHook
{
   auto &theHook = GetVetoDialogHook();
   auto result = theHook;
   theHook = hook;
   return result;
}

AudacityCommand::AudacityCommand()
{
   mProgress = NULL;
   mUIParent = NULL;
   mUIDialog = NULL;
   mUIDebug = false;
   mIsBatch = false;
   mNeedsInit = true;
}

AudacityCommand::~AudacityCommand()
{
   if (mUIDialog)
      mUIDialog->Close();
}


PluginPath AudacityCommand::GetPath(){        return BUILTIN_GENERIC_COMMAND_PREFIX + GetSymbol().Internal(); }
VendorSymbol AudacityCommand::GetVendor(){      return XO("Audacity");}
wxString AudacityCommand::GetVersion(){     return AUDACITY_VERSION_STRING;}


bool AudacityCommand::Init(){
   if( !mNeedsInit )
      return true;
   mNeedsInit = false;
   ShuttleDefaults DefaultSettingShuttle;
   return DefineParams( DefaultSettingShuttle );
}

bool AudacityCommand::ShowInterface(wxWindow *parent, bool WXUNUSED(forceModal))
{
   if (mUIDialog)
   {
      if ( mUIDialog->Close(true) )
         mUIDialog = nullptr;
      return false;
   }

   // mUIDialog is null
   auto cleanup = valueRestorer( mUIDialog );
   
   mUIDialog = CreateUI(parent, this);
   if (!mUIDialog)
      return false;

   mUIDialog->Layout();
   mUIDialog->Fit();
   mUIDialog->SetMinSize(mUIDialog->GetSize());

   // The Screenshot command might be popping this dialog up, just to capture it.
   auto hook = GetVetoDialogHook();
   if( hook && hook( mUIDialog ) )
      return false;

   bool res = mUIDialog->ShowModal() != 0;
   return res;
}

wxDialog *AudacityCommand::CreateUI(wxWindow *parent, AudacityCommand * WXUNUSED(client))
{
   Destroy_ptr<AudacityCommandDialog> dlg { safenew AudacityCommandDialog{
      parent, GetName(), this}};

   if (dlg->Init())
   {
      // release() is safe because parent will own it
      return dlg.release();
   }
   return NULL;
}

bool AudacityCommand::GetAutomationParameters(wxString & parms)
{
   CommandParameters eap;

   if (mUIDialog && !TransferDataFromWindow())
   {
      return false;
   }

   ShuttleGetAutomation S;
   S.mpEap = &eap;
   bool bResult = DefineParams( S );
   wxASSERT_MSG( bResult, "You did not define DefineParameters() for this command" );
   static_cast<void>(bResult); // fix unused variable warning in release mode

   return eap.GetParameters(parms);
}

bool AudacityCommand::SetAutomationParameters(const wxString & parms)
{
   wxString preset = parms;

   CommandParameters eap(parms);
   ShuttleSetAutomation S;

   S.SetForWriting( &eap );
   bool bResult = DefineParams( S );
   wxASSERT_MSG( bResult, "You did not define DefineParameters() for this command" );
   static_cast<void>(bResult); // fix unused variable warning in release mode
   if (!S.bOK)
   {
      AudacityCommand::MessageBox(
         XO(
"%s: Could not load settings below. Default settings will be used.\n\n%s")
            .Format( GetName(), preset ) );

      // fror now always succeed, so that we can prompt the user.
      return true;
   }

   return TransferDataToWindow();
}

bool AudacityCommand::DoAudacityCommand(wxWindow *parent,
                      const CommandContext & context,
                      bool shouldPrompt /* = true */)
{
   // Note: Init may read parameters from preferences
   if (!Init())
   {
      return false;
   }

   // Prompting will be bypassed when applying a command that has already 
   // been configured, e.g. repeating the last effect on a different selection.
   // Prompting may call AudacityCommand::Preview
   if (shouldPrompt && /*IsInteractive() && */!PromptUser(parent))
   {
      return false;
   }

   auto cleanup = finally( [&] {
      End();
   } );

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipAudacityCommand();
   if (skipFlag == false)
   {
      auto name = GetName();
      ProgressDialog progress{
         name,
         XO("Applying %s...").Format( name ),
         pdlgHideStopButton
      };
      auto vr = valueRestorer( mProgress, &progress );

      returnVal = Apply(context);
   }
   return returnVal;
}

// This is used from Macros.
bool AudacityCommand::PromptUser(wxWindow *parent)
{
   return ShowInterface(parent, IsBatchProcessing());
}

bool AudacityCommand::TransferDataToWindow()
{
   if (mUIParent && !mUIParent->TransferDataToWindow())
      return false;
   return true;
}

bool AudacityCommand::TransferDataFromWindow()
{
   if (mUIParent && (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()))
      return false;
   return true;
}

int AudacityCommand::MessageBox(
   const TranslatableString& message, long style,
   const TranslatableString &titleStr)
{
   auto title = titleStr.empty()
      ? GetName()
      : XO("%s: %s").Format( GetName(), titleStr );
   return AudacityMessageBox(message, title, style, mUIParent);
}

BEGIN_EVENT_TABLE(AudacityCommandDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudacityCommandDialog::OnOk)
   EVT_BUTTON(wxID_HELP, AudacityCommandDialog::OnHelp)
   EVT_BUTTON(wxID_CANCEL, AudacityCommandDialog::OnCancel)
END_EVENT_TABLE()

AudacityCommandDialog::AudacityCommandDialog(wxWindow * parent,
                           const TranslatableString & title,
                           AudacityCommand * pCommand,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
   wxASSERT( pCommand );
   mpCommand = pCommand;
   mAdditionalButtons = additionalButtons |eCancelButton;
   if( !pCommand->ManualPage().empty() )
      mAdditionalButtons |= eHelpButton;
}

bool AudacityCommandDialog::Init()
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);

      long buttons = eOkButton;
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
   return true;
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void AudacityCommandDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxASSERT( mpCommand );
   mpCommand->PopulateOrExchange( S );
}

bool AudacityCommandDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);
   return true;
}

bool AudacityCommandDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);
   return true;
}

bool AudacityCommandDialog::Validate()
{
   return true;
}

void AudacityCommandDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our AudacityCommands dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (FindWindow(wxID_OK)->IsEnabled() && Validate() && TransferDataFromWindow())
   {
      EndModal(true);
   }
}


void AudacityCommandDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   EndModal(false);
}

void AudacityCommandDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   if( mpCommand )
   {
      // otherwise use ShowHelp
      HelpSystem::ShowHelp(FindWindow(wxID_HELP), mpCommand->ManualPage(), true);
   }
}


