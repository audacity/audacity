/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_PREFS_DIALOG__
#define __AUDACITY_PREFS_DIALOG__

#include <wx/button.h>
#include <wx/event.h>
#include <wx/dialog.h>
#include <wx/string.h>
#include <wx/treebook.h>
#include <wx/window.h>

class PrefsDialog:public wxDialog
{
 public:
   PrefsDialog(wxWindow * parent);
   virtual ~PrefsDialog();

   void OnCategoryChange(wxCommandEvent & e);
   void OnOK(wxCommandEvent & e);
   void OnCancel(wxCommandEvent & e);
   void OnTreeKeyDown(wxTreeEvent & e); // Used to dismiss the dialog when enter is pressed with focus on tree

   void SelectPageByName(wxString pageName);
   void ShowTempDirPage();

 private:
   wxTreebook *mCategories;

   DECLARE_EVENT_TABLE()
};

#endif
