/**********************************************************************

  Sneedacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __SNEEDACITY_PREFS_DIALOG__
#define __SNEEDACITY_PREFS_DIALOG__

#include <functional>
#include <vector>
#include "../widgets/wxPanelWrapper.h" // to inherit
#include "PrefsPanel.h"

class SneedacityProject;
class wxTreebook;
class wxTreeEvent;
class ShuttleGui;

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

class SneedacityProject;

class SNEEDACITY_DLL_API PrefsDialog /* not final */ : public wxDialogWrapper
{
 public:
   PrefsDialog(wxWindow * parent,
      SneedacityProject *pProject, // may be null
      const TranslatableString &titlePrefix = XO("Preferences:"),
      PrefsPanel::Factories &factories =
         PrefsPanel::DefaultFactories());
   virtual ~PrefsDialog();

   // Defined this so a protected virtual can be invoked after the constructor
   int ShowModal() override;
   void ShuttleAll( ShuttleGui & S);

   void OnCategoryChange(wxCommandEvent & e);
   void OnOK(wxCommandEvent & e);
   void OnCancel(wxCommandEvent & e);
   void OnPreview(wxCommandEvent & e);
   void OnHelp(wxCommandEvent & e);
   void OnTreeKeyDown(wxTreeEvent & e); // Used to dismiss the dialog when enter is pressed with focus on tree

   void SelectPageByName(const wxString &pageName);

   // Accessor to help implementations of SavePreferredPage(),
   // such as by saving a preference after DoModal() returns
   int GetSelectedPage() const;

 protected:
    // Decide which page to open first; return -1 for undecided
    virtual long GetPreferredPage() = 0;

    // Called after OK is clicked and all pages validate
    virtual void SavePreferredPage() = 0;

private:
   void RecordExpansionState();
   PrefsPanel * GetCurrentPanel();
   wxTreebook *mCategories{};
   PrefsPanel *mUniquePage{};
   PrefsPanel::Factories &mFactories;
   const TranslatableString mTitlePrefix;

   DECLARE_EVENT_TABLE()
};

// This adds code appropriate only to the original use of PrefsDialog for
// global settings -- not its reuses elsewhere as in View Settings
class SNEEDACITY_DLL_API GlobalPrefsDialog final : public PrefsDialog
{
public:
   GlobalPrefsDialog(
      wxWindow * parent, SneedacityProject *pProject,
      PrefsPanel::Factories &factories =
         PrefsPanel::DefaultFactories());
   virtual ~GlobalPrefsDialog();
   long GetPreferredPage() override;
   void SavePreferredPage() override;
};

class SneedacityProject;
void SNEEDACITY_DLL_API DoReloadPreferences( SneedacityProject &project );

#endif
