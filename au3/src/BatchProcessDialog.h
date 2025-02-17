/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MACROS_WINDOW__
#define __AUDACITY_MACROS_WINDOW__

#include <wx/defs.h>

#include "BatchCommands.h"
#include "Prefs.h"
#include "wxPanelWrapper.h"

class wxWindow;
class wxTextCtrl;
class wxListCtrl;
class wxListEvent;
class wxButton;
class wxTextCtrl;
class AudacityProject;
class ShuttleGui;

class ApplyMacroDialog : public wxDialogWrapper
{
public:
    // constructors and destructors
    ApplyMacroDialog(
        wxWindow* parent, AudacityProject& project, bool bInherited=false);
    virtual ~ApplyMacroDialog();
public:
    // Populate methods NOT virtual.
    void Populate();
    void PopulateOrExchange(ShuttleGui& S);
    virtual void OnApplyToProject(wxCommandEvent& event);
    virtual void OnApplyToFiles(wxCommandEvent& event);
    virtual void OnCancel(wxCommandEvent& event);
    virtual void OnHelp(wxCommandEvent& event);

    virtual ManualPageID GetHelpPageName() { return "Apply_Macro"; }

    void PopulateMacros();
    static CommandID MacroIdOfName(const wxString& MacroName);
    void ApplyMacroToProject(int iMacro, bool bHasGui=true);
    void ApplyMacroToProject(const CommandID& MacroID, bool bHasGui=true);

    // These will be reused in the derived class...
    wxListCtrl* mList;
    wxListCtrl* mMacros;
    MacroCommands mMacroCommands; /// Provides list of available commands.

    wxButton* mResize;
    wxButton* mOK;
    wxButton* mCancel;
    wxTextCtrl* mResults;
    bool mAbort;
    bool mbExpanded;
    wxString mActiveMacro;
    wxString mMacroBeingRenamed;

protected:
    AudacityProject& mProject;
    const MacroCommandsCatalog mCatalog;

    DECLARE_EVENT_TABLE()
};

class MacrosWindow final : public ApplyMacroDialog, public PrefsListener
{
public:
    MacrosWindow(
        wxWindow* parent, AudacityProject& project, bool bExpanded=true);
    ~MacrosWindow();
    void UpdateDisplay(bool bExpanded);

private:
    TranslatableString WindowTitle() const;

    void Populate();
    void PopulateOrExchange(ShuttleGui& S);
    void OnApplyToProject(wxCommandEvent& event) override;
    void OnApplyToFiles(wxCommandEvent& event) override;
    void OnCancel(wxCommandEvent& event) override;

    virtual ManualPageID GetHelpPageName() override
    {
        return
            mbExpanded ? "Manage_Macros"
            : "Apply_Macro";
    }

    void PopulateList();
    void AddItem(const CommandID& command, wxString const& params);
    bool ChangeOK();
    void UpdateMenus();
    void ShowActiveMacro();

    void OnMacroSelected(wxListEvent& event);
    void OnListSelected(wxListEvent& event);
    void OnMacrosBeginEdit(wxListEvent& event);
    void OnMacrosEndEdit(wxListEvent& event);
    void OnAdd(wxCommandEvent& event);
    void OnRemove(wxCommandEvent& event);
    void OnRename(wxCommandEvent& event);
    void OnRestore(wxCommandEvent& event);
    void OnImport(wxCommandEvent& event);
    void OnExport(wxCommandEvent& event);
    void OnSave(wxCommandEvent& event);

    void OnExpand(wxCommandEvent& event);
    void OnShrink(wxCommandEvent& event);
    void OnSize(wxSizeEvent& event);

    void OnCommandActivated(wxListEvent& event);
    void OnInsert(wxCommandEvent& event);
    void OnEditCommandParams(wxCommandEvent& event);

    void OnDelete(wxCommandEvent& event);
    void OnUp(wxCommandEvent& event);
    void OnDown(wxCommandEvent& event);

    void OnOK(wxCommandEvent& event);

    void OnKeyDown(wxKeyEvent& event);
    void FitColumns();
    void InsertCommandAt(int item);
    bool SaveChanges();

    // PrefsListener implementation
    void UpdatePrefs() override;

    AudacityProject& mProject;

    wxButton* mRemove;
    wxButton* mRename;
    wxButton* mRestore;
    wxButton* mImport;
    wxButton* mExport;
    wxButton* mEdit;
    wxButton* mSave;

    int mSelectedCommand;
    bool mChanged;

    DECLARE_EVENT_TABLE()
};

#endif
