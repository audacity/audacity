/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MACROS_WINDOW__
#define __AUDACITY_MACROS_WINDOW__

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

class ApplyMacroDialog : public wxDialogWrapper {
 public:
   // constructors and destructors
   ApplyMacroDialog(wxWindow * parent, bool bInherited=false);
   virtual ~ApplyMacroDialog();
 public:
   // Populate methods NOT virtual.
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   virtual void OnApplyToProject(wxCommandEvent & event);
   virtual void OnApplyToFiles(wxCommandEvent & event);
   virtual void OnCancel(wxCommandEvent & event);
   virtual void OnHelp(wxCommandEvent & event);

   virtual wxString GetHelpPageName() {return "Tools_Menu#macros_compact_dialog";};

   void PopulateMacros();
   static wxString MacroIdOfName( const wxString & MacroName );
   void ApplyMacroToProject( int iMacro, bool bHasGui=true );
   void ApplyMacroToProject( const wxString & MacroID, bool bHasGui=true );


   // These will be reused in the derived class...
   wxListCtrl *mList;
   wxListCtrl *mMacros;
   MacroCommands mMacroCommands; /// Provides list of available commands.

   wxButton *mResize;
   wxButton *mOK;
   wxButton *mCancel;
   wxTextCtrl *mResults;
   bool mAbort;
   bool mbExpanded;
   wxString mActiveMacro;

protected:
   const MacroCommandsCatalog mCatalog;

   DECLARE_EVENT_TABLE()
};

class MacrosWindow final : public ApplyMacroDialog
{
public:
   MacrosWindow(wxWindow * parent, bool bExpanded=true);
   ~MacrosWindow();
   void UpdateDisplay( bool bExpanded );

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);
   void OnApplyToProject(wxCommandEvent & event) override;
   void OnApplyToFiles(wxCommandEvent & event) override;
   void OnCancel(wxCommandEvent &event) override;

   virtual wxString GetHelpPageName() override {return 
      mbExpanded ? "Tools_Menu#macross_full_dialog"
         : "Tools_Menu#macros_compact_dialog";};

   void PopulateList();
   void AddItem(const wxString &command, wxString const &params);
   bool ChangeOK();
   void UpdateMenus();

   void OnMacroSelected(wxListEvent &event);
   void OnListSelected(wxListEvent &event);
   void OnMacrosBeginEdit(wxListEvent &event);
   void OnMacrosEndEdit(wxListEvent &event);
   void OnAdd(wxCommandEvent &event);
   void OnRemove(wxCommandEvent &event);
   void OnRename(wxCommandEvent &event);
   void OnExpand(wxCommandEvent &event);
   void OnShrink(wxCommandEvent &event);
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

   // These are already provided by ApplyMacroDialog
   //wxListCtrl *mList;   /// List of commands in current command chain.
   //MacroCommands mMacroCommands;  /// Provides list of available commands.
   //wxListCtrl *mMacros; /// List of chains.

   wxButton *mRemove;
   wxButton *mRename;
   wxButton *mDefaults;

   int mSelectedCommand;
   bool mChanged;

   DECLARE_EVENT_TABLE()
};

#endif
