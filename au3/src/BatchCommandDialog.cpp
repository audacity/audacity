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

#include "BatchCommandDialog.h"

#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

//
#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/button.h>

#include "DoEffect.h"
#include "EffectManager.h"
#include "HelpSystem.h"
#include "PluginManager.h"
#include "Project.h"
#include "ShuttleGui.h"

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

MacroCommandDialog::MacroCommandDialog(
    wxWindow* parent, wxWindowID id, AudacityProject& project)
    : wxDialogWrapper(
        parent, id, XO("Select Command"), wxDefaultPosition, wxDefaultSize,
        wxCAPTION | wxRESIZE_BORDER)
    , mProject{project}
    , mCatalog{&project}
{
    SetLabel(XO("Select Command"));        // Provide visual label
    SetName(XO("Select Command"));         // Provide audible label
    Populate();
}

void MacroCommandDialog::Populate()
{
    //------------------------- Main section --------------------
    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

void MacroCommandDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.StartVerticalLay(true);
    {
        S.StartMultiColumn(4, wxEXPAND);
        {
            S.SetStretchyCol(1);
            mCommand = S.AddTextBox(XXO("&Command"), wxT(""), 20);
            mCommand->SetEditable(false);
            mEditParams = S.Id(EditParamsButtonID)
                          .Disable() // disable button as box is empty
                          .AddButton(XXO("&Edit Parameters"));
            mUsePreset = S.Id(UsePresetButtonID)
                         .Disable() // disable button as box is empty
                         .AddButton(XXO("&Use Preset"));
        }
        S.EndMultiColumn();

        S.StartMultiColumn(2, wxEXPAND);
        {
            S.SetStretchyCol(1);
            mParameters = S.AddTextBox(XXO("&Parameters"), wxT(""), 0);
            mParameters->SetEditable(false);
            auto prompt = XXO("&Details");
            S.Prop(0).AddPrompt(prompt);
            mDetails = S
                       .Name(prompt)
                       .AddTextWindow(wxT(""));
            mDetails->SetEditable(false);
        }
        S.EndMultiColumn();

        S.Prop(10).StartStatic(XO("Choose command"), true);
        {
            mChoices = S.Id(CommandsListID)
                       .Style(wxLC_LIST | wxLC_SINGLE_SEL)
                       .AddListControl();
        }
        S.EndStatic();
    }
    S.EndVerticalLay();

    S.AddStandardButtons(eOkButton | eCancelButton | eHelpButton);

    PopulateCommandList();
    if (mChoices->GetItemCount() > 0) {
        // set first item to be selected (and the focus when the
        // list first becomes the focus)
        mChoices->SetItemState(0, wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                               wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
    }

    SetMinSize(wxSize(780, 560));
    Fit();
    Center();
}

void MacroCommandDialog::PopulateCommandList()
{
    mChoices->DeleteAllItems();
    long ii = 0;
    for ( const auto& entry : mCatalog ) {
        // insert the user-facing string
        mChoices->InsertItem(ii++, entry.name.StrippedTranslation());
    }
}

void MacroCommandDialog::ValidateChoices()
{
}

void MacroCommandDialog::OnChoice(wxCommandEvent& WXUNUSED(event))
{
}

void MacroCommandDialog::OnOk(wxCommandEvent& WXUNUSED(event))
{
    mSelectedCommand = mInternalCommandName
                       // .Strip(wxString::both) // PRL: used to do this, here only,
                       // but ultimately mSelectedCommand is looked up in the catalog without
                       // similar adjustment of whitespace in the comparison
    ;
    mSelectedParameters = mParameters->GetValue().Strip(wxString::trailing);
    EndModal(true);
}

void MacroCommandDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
    EndModal(false);
}

void MacroCommandDialog::OnHelp(wxCommandEvent& WXUNUSED(event))
{
    const auto& page = GetHelpPageName();
    HelpSystem::ShowHelp(this, page, true);
}

void MacroCommandDialog::OnItemSelected(wxListEvent& event)
{
    const auto& command = mCatalog[ event.GetIndex() ];

    EffectManager& em = EffectManager::Get();
    PluginID ID
        =PluginManager::Get().GetByCommandIdentifier(command.name.Internal());

    // If ID is empty, then the effect wasn't found, in which case, the user must have
    // selected one of the "special" commands.
    mEditParams->Enable(!ID.empty());
    mUsePreset->Enable(em.HasPresets(ID));

    auto value = command.name.StrippedTranslation();
    if (value == mCommand->GetValue()) {
        // This uses the assumption of uniqueness of translated names!
        return;
    }

    mCommand->SetValue(value);
    mInternalCommandName = command.name.Internal();

    wxString params = MacroCommands::GetCurrentParamsFor(mInternalCommandName);
    if (params.empty()) {
        params = em.GetDefaultPreset(ID);
    }

    // using GET to expose a CommandID to the user!
    // Cryptic command and category.
    // Later we can put help information there, perhaps.
    // Macro command details are one place that we do expose Identifier
    // to (more sophisticated) users
    mDetails->SetValue(
        mInternalCommandName.GET() + "\r\n" + command.category.Translation());
    mParameters->SetValue(params);
}

void MacroCommandDialog::OnEditParams(wxCommandEvent& WXUNUSED(event))
{
    auto command = mInternalCommandName;
    wxString params  = mParameters->GetValue();

    params = MacroCommands::PromptForParamsFor(command, params, mProject).Trim();

    mParameters->SetValue(params);
    mParameters->Refresh();
}

void MacroCommandDialog::OnUsePreset(wxCommandEvent& WXUNUSED(event))
{
    auto command = mInternalCommandName;
    wxString params  = mParameters->GetValue();

    wxString preset = MacroCommands::PromptForPresetFor(command, params, this).Trim();

    mParameters->SetValue(preset);
    mParameters->Refresh();
}

void MacroCommandDialog::SetCommandAndParams(const CommandID& Command, const wxString& Params)
{
    auto iter = mCatalog.ByCommandId(Command);

    mParameters->SetValue(Params);

    mInternalCommandName = Command;
    if (iter == mCatalog.end()) {
        // uh oh, using GET to expose an internal name to the user!
        // in default of any better friendly name
        mCommand->SetValue(Command.GET());
    } else {
        mCommand->SetValue(iter->name.StrippedTranslation());
        // using GET to expose a CommandID to the user!
        // Macro command details are one place that we do expose Identifier
        // to (more sophisticated) users
        mDetails->SetValue(
            iter->name.Internal() + "\r\n" + iter->category.Translation());
        mChoices->SetItemState(iter - mCatalog.begin(),
                               wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

        PluginID ID = PluginManager::Get().GetByCommandIdentifier(Command);

        // If ID is empty, then the effect wasn't found, in which case, the user must have
        // selected one of the "special" commands.
        mEditParams->Enable(!ID.empty());
        mUsePreset->Enable(EffectManager::Get().HasPresets(ID));
    }
}
