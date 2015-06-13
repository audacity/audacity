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

class PrefsPanelFactory;

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

class PrefsDialog:public wxDialog
{
 public:
    // An array of PrefsNode specifies the tree of pages in pre-order traversal.
    struct PrefsNode {
       PrefsPanelFactory * CONST pFactory;
       CONST int nChildren;
       bool expanded;

       PrefsNode(PrefsPanelFactory *pFactory_, int nChildren_ = 0)
          : pFactory(pFactory_), nChildren(nChildren_), expanded(false)
       {}
    };
   typedef std::vector<PrefsNode> Factories;
   static Factories &DefaultFactories();

   PrefsDialog(wxWindow * parent, Factories &factories = DefaultFactories());
   virtual ~PrefsDialog();

   void OnCategoryChange(wxCommandEvent & e);
   void OnOK(wxCommandEvent & e);
   void OnCancel(wxCommandEvent & e);
   void OnTreeKeyDown(wxTreeEvent & e); // Used to dismiss the dialog when enter is pressed with focus on tree

   void SelectPageByName(wxString pageName);
   void ShowTempDirPage();

 private:
   void RecordExpansionState();

   wxTreebook *mCategories;
   Factories &mFactories;

   DECLARE_EVENT_TABLE()
};

#endif
