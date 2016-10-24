/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_PROCESS_DIALOG__
#define __AUDACITY_BATCH_PROCESS_DIALOG__

#include <wx/defs.h>
#include <wx/string.h>


#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

//#include  "wx/log.h"
#include  <wx/sizer.h>
#include  <wx/menuitem.h>
#include  <wx/checklst.h>

#include "BatchCommands.h"

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

class BatchProcessDialog final : public wxDialogWrapper {
 public:
   // constructors and destructors
   BatchProcessDialog(wxWindow * parent);
   virtual ~BatchProcessDialog();
 public:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );

   void OnApplyToProject(wxCommandEvent & event);
   void OnApplyToFiles(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   wxButton *mOK;
   wxButton *mCancel;
   wxListCtrl *mChains;
   wxListCtrl *mList;
   BatchCommands mBatchCommands;

   bool mAbort;

   DECLARE_EVENT_TABLE()
};

class EditChainsDialog final : public wxDialogWrapper
{
public:
   EditChainsDialog(wxWindow * parent);
   ~EditChainsDialog();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateChains();
   void PopulateList();
   void AddItem(const wxString &command, wxString const &params);
   bool ChangeOK();

   void OnChainSelected(wxListEvent &event);
   void OnListSelected(wxListEvent &event);
   void OnChainsBeginEdit(wxListEvent &event);
   void OnChainsEndEdit(wxListEvent &event);
   void OnAdd(wxCommandEvent &event);
   void OnRemove(wxCommandEvent &event);
   void OnRename(wxCommandEvent &event);
   void OnSize(wxSizeEvent &event);

   void OnCommandActivated(wxListEvent &event);
   void OnInsert(wxCommandEvent &event);
   void OnDelete(wxCommandEvent &event);
   void OnUp(wxCommandEvent &event);
   void OnDown(wxCommandEvent &event);
   void OnDefaults(wxCommandEvent &event);

   void OnOK(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);

   void OnKeyDown(wxKeyEvent &event);
   void FitColumns();

   wxListCtrl *mChains; /// List of chains.
   wxListCtrl *mList;   /// List of commands in current command chain.
   wxButton *mRemove;
   wxButton *mRename;
   wxButton *mDefaults;

   BatchCommands mBatchCommands;  /// Provides list of available commands.
   wxString mActiveChain;

   int mSelectedCommand;
   bool mChanged;

   DECLARE_EVENT_TABLE()
};

#endif
