/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MACRO_COMMAND_DIALOG__
#define __AUDACITY_MACRO_COMMAND_DIALOG__

#include "MemoryX.h"
#include <wx/defs.h>
#include <wx/string.h>


#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

//#include  "wx/log.h"
#include  <wx/sizer.h>
#include  <wx/menuitem.h>
#include  <wx/checklst.h>

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxTextCtrl;
class wxStaticText;
class wxRadioButton;
class wxListCtrl;
class wxListEvent;
class wxButton;
class ShuttleGui;

class MacroCommandDialog final : public wxDialogWrapper {
 public:
   // constructors and destructors
   MacroCommandDialog(wxWindow *parent, wxWindowID id);
   void SetCommandAndParams(const wxString &Command, const wxString &Params);
 public:
   wxString   mSelectedCommand;
   wxString   mSelectedParameters;
 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);
   void OnEditParams(wxCommandEvent &event);
   void OnUsePreset(wxCommandEvent &event);
   void OnChoice(wxCommandEvent &event);
   void OnOk(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnHelp(wxCommandEvent &event);
   void OnItemSelected(wxListEvent &event);
   wxString GetHelpPageName() { return wxT("Scripting Reference") ; }

   void ValidateChoices();
   void PopulateCommandList();
   //int GetSelectedItem();

   wxButton   *mEditParams;
   wxButton   *mUsePreset;
   wxListCtrl *mChoices;
   wxTextCtrl * mCommand;
   wxTextCtrl * mParameters;
   wxTextCtrl * mDetails;

   wxString mInternalCommandName;

   using CommandName = std::tuple<wxString, wxString,wxString>;
   using CommandNameVector = std::vector<CommandName>;
   CommandNameVector mCommandNames;

   DECLARE_EVENT_TABLE()
};


#endif
