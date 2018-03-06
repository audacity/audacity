/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class MacroCommandDialog
\brief Provides a list of configurable commands for use with MacroCommands

Provides a list of commands, mostly effects, which can be chained
together in a simple linear sequence.  Can configure parameters on each
selected command.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/string.h>
#include <wx/dialog.h>


#include "Project.h"
#include "BatchCommandDialog.h"
#include "commands/CommandManager.h"
#include "effects/EffectManager.h"
#include "BatchCommands.h"
#include "ShuttleGui.h"
#include "widgets/HelpSystem.h"


#define CommandsListID        7001
#define EditParamsButtonID    7002
#define UsePresetButtonID     7003

BEGIN_EVENT_TABLE(MacroCommandDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK,                     MacroCommandDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                 MacroCommandDialog::OnCancel)
   EVT_BUTTON(wxID_HELP,                   MacroCommandDialog::OnHelp)
   EVT_BUTTON(EditParamsButtonID,          MacroCommandDialog::OnEditParams)
   EVT_BUTTON(UsePresetButtonID,           MacroCommandDialog::OnUsePreset)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID, MacroCommandDialog::OnItemSelected)
   EVT_LIST_ITEM_SELECTED(CommandsListID,  MacroCommandDialog::OnItemSelected)
END_EVENT_TABLE();

MacroCommandDialog::MacroCommandDialog(wxWindow * parent, wxWindowID id):
   wxDialogWrapper(parent, id, _("Select Command"),
            wxDefaultPosition, wxDefaultSize,
            wxCAPTION | wxRESIZE_BORDER)
{
   SetLabel(_("Select Command"));         // Provide visual label
   SetName(_("Select Command"));          // Provide audible label
   Populate();
}

void MacroCommandDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void MacroCommandDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(true);
   {
      S.StartMultiColumn(4, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mCommand = S.AddTextBox(_("&Command"), wxT(""), 20);
         mCommand->SetEditable(false);
         mEditParams = S.Id(EditParamsButtonID).AddButton(_("&Edit Parameters"));
         mEditParams->Enable(false); // disable button as box is empty
         mUsePreset = S.Id(UsePresetButtonID).AddButton(_("&Use Preset"));
         mUsePreset->Enable(false); // disable button as box is empty
      }
      S.EndMultiColumn();

      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mParameters = S.AddTextBox(_("&Parameters"), wxT(""), 0);
         mParameters->SetEditable(false);
         S.Prop(0).AddPrompt( _("&Details" ) );
         mDetails = S.AddTextWindow( wxT(""));
         mDetails->SetEditable(false);
      }
      S.EndMultiColumn();

      S.Prop(10).StartStatic(_("C&hoose command"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_LIST | wxLC_SINGLE_SEL);
         mChoices = S.Id(CommandsListID).AddListControl();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();

   S.AddStandardButtons( eOkButton | eCancelButton | eHelpButton);

   PopulateCommandList();

   SetMinSize(wxSize(780, 560));
   Fit();
   Center();
}

void MacroCommandDialog::PopulateCommandList()
{
   mCommandNames = MacroCommands::GetAllCommands();

   mChoices->DeleteAllItems();
   for (size_t ii = 0, size = mCommandNames.size(); ii < size; ++ii)
      // insert the user-facing string
      mChoices->InsertItem( ii, std::get<0>( mCommandNames[ii] ) );
}

void MacroCommandDialog::ValidateChoices()
{
}

void MacroCommandDialog::OnChoice(wxCommandEvent & WXUNUSED(event))
{
}

void MacroCommandDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   mSelectedCommand = mInternalCommandName.Strip(wxString::both);
   mSelectedParameters = mParameters->GetValue().Strip(wxString::trailing);
   EndModal(true);
}

void MacroCommandDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

void MacroCommandDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   wxString page = GetHelpPageName();
   HelpSystem::ShowHelp(this, page, true);
}

void MacroCommandDialog::OnItemSelected(wxListEvent &event)
{
   const auto &command = mCommandNames[ event.GetIndex() ];

   EffectManager & em = EffectManager::Get();
   PluginID ID = em.GetEffectByIdentifier( std::get<1>( command ));

   // If ID is empty, then the effect wasn't found, in which case, the user must have
   // selected one of the "special" commands.
   mEditParams->Enable(!ID.IsEmpty());
   mUsePreset->Enable(em.HasPresets(ID));

   if (std::get<0>( command ) == mCommand->GetValue())
      return;

   mCommand->SetValue(std::get<0> (command));
   mInternalCommandName = std::get<1>( command );

   wxString params = MacroCommands::GetCurrentParamsFor(mInternalCommandName);
   if (params.IsEmpty())
   {
      params = em.GetDefaultPreset(ID);
   }

   // Cryptic command and category.
   // Later we can put help information there, perhaps.
   mDetails->SetValue( mInternalCommandName + "\r\n" + std::get<2>(command)  );
   mParameters->SetValue(params);
}

void MacroCommandDialog::OnEditParams(wxCommandEvent & WXUNUSED(event))
{
   wxString command = mInternalCommandName;
   wxString params  = mParameters->GetValue();

   params = MacroCommands::PromptForParamsFor(command, params, this).Trim();

   mParameters->SetValue(params);
   mParameters->Refresh();
}

void MacroCommandDialog::OnUsePreset(wxCommandEvent & WXUNUSED(event))
{
   wxString command = mInternalCommandName;
   wxString params  = mParameters->GetValue();

   wxString preset = MacroCommands::PromptForPresetFor(command, params, this).Trim();

   mParameters->SetValue(preset);
   mParameters->Refresh();
}

void MacroCommandDialog::SetCommandAndParams(const wxString &Command, const wxString &Params)
{
   auto item = make_iterator_range(mCommandNames).index_if(
      [&](const CommandName &name){ return Command == std::get<1>( name); }
   );

   mParameters->SetValue( Params );

   mInternalCommandName = Command;
   if (item < 0)
      mCommand->SetValue( Command );
   else {
      mCommand->SetValue( std::get<0>( mCommandNames[item]) );
      mDetails->SetValue( std::get<1>(mCommandNames[item]) + "\r\n" + std::get<2>(mCommandNames[item])  );
      mChoices->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

      EffectManager & em = EffectManager::Get();
      PluginID ID = em.GetEffectByIdentifier(Command);

      // If ID is empty, then the effect wasn't found, in which case, the user must have
      // selected one of the "special" commands.
      mEditParams->Enable(!ID.IsEmpty());
      mUsePreset->Enable(em.HasPresets(ID));
   }
}
