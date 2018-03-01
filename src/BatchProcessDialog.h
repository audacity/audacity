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
class wxTextCtrl;
class ShuttleGui;

class BatchProcessDialog : public wxDialogWrapper {
 public:
   // constructors and destructors
   BatchProcessDialog(wxWindow * parent, bool bInherited=false);
   virtual ~BatchProcessDialog();
 public:
   virtual void Populate();
   virtual void PopulateOrExchange( ShuttleGui & S );
   virtual void OnApplyToProject(wxCommandEvent & event);
   virtual void OnApplyToFiles(wxCommandEvent & event);
   virtual void OnCancel(wxCommandEvent & event);

   // These will be reused in the derived class...
   wxListCtrl *mList;
   wxListCtrl *mChains;
   BatchCommands mBatchCommands; /// Provides list of available commands.

   wxButton *mOK;
   wxButton *mCancel;
   wxTextCtrl *mResults;
   bool mAbort;

   DECLARE_EVENT_TABLE()
};

class EditChainsDialog final : public BatchProcessDialog
{
public:
   EditChainsDialog(wxWindow * parent);
   ~EditChainsDialog();

private:
   void Populate() override;
   void PopulateOrExchange(ShuttleGui &S) override;
   void OnApplyToProject(wxCommandEvent & event) override;
   void OnApplyToFiles(wxCommandEvent & event) override;
   void OnCancel(wxCommandEvent &event) override;


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
   void OnEditCommandParams(wxCommandEvent &event);

   void OnDelete(wxCommandEvent &event);
   void OnUp(wxCommandEvent &event);
   void OnDown(wxCommandEvent &event);
   void OnDefaults(wxCommandEvent &event);

   //void OnApplyToProject(wxCommandEvent &event);
   //void OnApplyToFiles(wxCommandEvent &event);

   void OnOK(wxCommandEvent &event);

   void OnKeyDown(wxKeyEvent &event);
   void FitColumns();
   void InsertCommandAt(int item);
   bool SaveChanges();

   // These are already provided by BatchProcessDialog
   //wxListCtrl *mList;   /// List of commands in current command chain.
   //BatchCommands mBatchCommands;  /// Provides list of available commands.
   //wxListCtrl *mChains; /// List of chains.

   wxButton *mRemove;
   wxButton *mRename;
   wxButton *mDefaults;


   wxString mActiveChain;

   int mSelectedCommand;
   bool mChanged;

   using CommandName = std::pair<wxString, wxString>;
   using CommandNameVector = std::vector<CommandName>;
   CommandNameVector mCommandNames;
   
   DECLARE_EVENT_TABLE()
};

#endif
