/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_COMMAND_DIALOG__
#define __AUDACITY_BATCH_COMMAND_DIALOG__

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

class BatchCommandDialog final : public wxDialogWrapper {
 public:
   // constructors and destructors
   BatchCommandDialog(wxWindow *parent, wxWindowID id);
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
   void OnItemSelected(wxListEvent &event);

   void ValidateChoices();
   void PopulateCommandList();
   int GetSelectedItem();

   wxButton   *mEditParams;
   wxButton   *mUsePreset;
   wxListCtrl *mChoices;
   wxTextCtrl * mCommand;
   wxTextCtrl * mParameters;
   DECLARE_EVENT_TABLE()
};


#endif
