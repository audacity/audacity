/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_PREFS_DIALOG__
#define __AUDACITY_PREFS_DIALOG__

#include <vector>
#include <wx/button.h>
#include <wx/event.h>
#include <wx/dialog.h>
#include <wx/string.h>
#include <wx/treebook.h>
#include <wx/window.h>
#include "../widgets/wxPanelWrapper.h"

class PrefsPanel;
class PrefsPanelFactory;

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

class PrefsDialog /* not final */ : public wxDialogWrapper
{
 public:
    // An array of PrefsNode specifies the tree of pages in pre-order traversal.
    struct PrefsNode {
       PrefsPanelFactory * CONST pFactory;
       CONST int nChildren;
       bool expanded;

       PrefsNode(PrefsPanelFactory *pFactory_,
          int nChildren_ = 0,
          bool expanded_ = true)
          : pFactory(pFactory_), nChildren(nChildren_), expanded(expanded_)
       {}
    };
   typedef std::vector<PrefsNode> Factories;
   static Factories &DefaultFactories();

   PrefsDialog(wxWindow * parent,
      const wxString &titlePrefix = _("Preferences: "),
      Factories &factories = DefaultFactories());
   virtual ~PrefsDialog();

   // Defined this so a protected virtual can be invoked after the constructor
   int ShowModal() override;

   void OnCategoryChange(wxCommandEvent & e);
   void OnOK(wxCommandEvent & e);
   void OnCancel(wxCommandEvent & e);
   void OnApply(wxCommandEvent & e);
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
   wxTreebook *mCategories{};
   PrefsPanel *mUniquePage{};
   Factories &mFactories;
   const wxString mTitlePrefix;

   DECLARE_EVENT_TABLE()
};

// This adds code appropriate only to the original use of PrefsDialog for
// global settings -- not its reuses elsewhere as in View Settings
class GlobalPrefsDialog final : public PrefsDialog
{
public:
   GlobalPrefsDialog(wxWindow * parent, Factories &factories = DefaultFactories());
   virtual ~GlobalPrefsDialog();
   long GetPreferredPage() override;
   void SavePreferredPage() override;
};

#endif
