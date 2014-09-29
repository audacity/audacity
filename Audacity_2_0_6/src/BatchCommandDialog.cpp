/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class BatchCommandDialog
\brief Provides a list of configurable commands for use with BatchCommands

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


#define CommandsListID        7001
#define EditParamsButtonID    7002

BEGIN_EVENT_TABLE(BatchCommandDialog, wxDialog)
   EVT_BUTTON(wxID_OK,                     BatchCommandDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                 BatchCommandDialog::OnCancel)
   EVT_BUTTON(EditParamsButtonID,          BatchCommandDialog::OnEditParams)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID,  BatchCommandDialog::OnItemSelected)
END_EVENT_TABLE();

BatchCommandDialog::BatchCommandDialog(wxWindow * parent, wxWindowID id):
   wxDialog(parent, id, _("Select Command"),
            wxDefaultPosition, wxSize(250,200),
            wxCAPTION | wxRESIZE_BORDER)
{
   SetLabel(_("Select Command"));         // Provide visual label
   SetName(_("Select Command"));          // Provide audible label
   Populate();
}

void BatchCommandDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void BatchCommandDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(true);
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mCommand = S.AddTextBox(_("&Command"), wxT(""), 20);
         mCommand->SetEditable(false);
         mEditParams = S.Id(EditParamsButtonID).AddButton(_("&Edit Parameters"));
         mEditParams->Enable( false ); // disable button as box is empty
      }
      S.EndMultiColumn();

      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mParameters = S.AddTextBox(_("&Parameters"), wxT(""), 0);
         mParameters->SetEditable(false);
      }
      S.EndMultiColumn();

      S.StartStatic(_("C&hoose command"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_LIST | wxLC_SINGLE_SEL);
         mChoices = S.Id(CommandsListID).AddListControl();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();

   S.AddStandardButtons();

   for(int i=0;i<99;i++)
   {
      mChoices->InsertItem( i, wxString::Format(wxT("Item%02i"),i));
   }
   PopulateCommandList();

   SetSize(350, 400);
   SetSizeHints(GetSize());
   Center();
}

void BatchCommandDialog::PopulateCommandList()
{
   wxArrayString commandList = BatchCommands::GetAllCommands();

   unsigned int i;
   mChoices->DeleteAllItems();
   for( i=0;i<commandList.GetCount();i++)
   {
      mChoices->InsertItem( i, commandList[i]);
   }
}

int BatchCommandDialog::GetSelectedItem()
{
   int i;
   mSelectedCommand = wxT("");
   for(i=0;i<mChoices->GetItemCount();i++)
   {
      if( mChoices->GetItemState( i, wxLIST_STATE_FOCUSED) != 0)
      {
         mSelectedCommand = mChoices->GetItemText( i );
         return i;
      }
   }
   return -1;
}

void BatchCommandDialog::ValidateChoices()
{
}

void BatchCommandDialog::OnChoice(wxCommandEvent & WXUNUSED(event))
{
}

void BatchCommandDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   mSelectedCommand = mCommand->GetValue().Strip(wxString::both);
   mSelectedParameters = mParameters->GetValue().Strip(wxString::trailing);
   EndModal(true);
}

void BatchCommandDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

void BatchCommandDialog::OnItemSelected(wxListEvent &event)
{
   int itemNo = event.GetIndex();
   wxString command = mChoices->GetItemText( itemNo );
   mCommand->SetValue( command );
   wxString params = BatchCommands::GetCurrentParamsFor( command );
   mParameters->SetValue( params );
   Effect * f = EffectManager::Get().GetEffectByIdentifier( command );
   mEditParams->Enable( f != NULL );
}

void BatchCommandDialog::OnEditParams(wxCommandEvent & WXUNUSED(event))
{
   wxString command = mCommand->GetValue();
   wxString params  = mParameters->GetValue();
   Effect * f = EffectManager::Get().GetEffectByIdentifier( command );
   if( f==NULL )
      return;
   BatchCommands::SetCurrentParametersFor( f, command, params );
   if( BatchCommands::PromptForParamsFor( command, this ))
   {
      // we've just prompted for the parameters, so the values
      // that are current have changed.
      params = BatchCommands::GetCurrentParamsFor( command );
      mParameters->SetValue( params.Strip(wxString::trailing) );
      mParameters->Refresh();
   }
}

void BatchCommandDialog::SetCommandAndParams(const wxString &Command, const wxString &Params)
{
   mCommand->SetValue( Command );
   mParameters->SetValue( Params );

   int item = mChoices->FindItem(-1, Command);
   if( item != -1 )
   {
      mChoices->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
      mEditParams->Enable( true );
   }
}
