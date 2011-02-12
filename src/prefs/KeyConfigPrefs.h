/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include <wx/defs.h>
#include <wx/listctrl.h>
#include <wx/textctrl.h>
#include <wx/string.h>

#include "../ShuttleGui.h"
#include "../commands/CommandManager.h"

#include "PrefsPanel.h"

class KeyConfigPrefs:public PrefsPanel 
{
 public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   virtual bool Apply();
   virtual void Cancel();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void CreateList();
   void RepopulateBindingsList();
   wxString NameFromKey( const wxString & Key );

   void OnDefaults(wxCommandEvent & e);
   void OnLoad(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);
   void OnSet(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnCategory(wxCommandEvent & e);
   void OnItemSelected(wxListEvent & e);
   void OnKeyDown(wxListEvent & e);

   void OnCaptureKeyDown(wxKeyEvent & e);
   void OnCaptureChar(wxKeyEvent & e);

   wxChoice *mCat;
   wxTextCtrl *mKey;
   wxListCtrl *mList;

   CommandManager *mManager;
   size_t mCommandSelected;

   wxArrayString mCats;
   wxArrayString mNames;
   wxArrayString mKeys;

   DECLARE_EVENT_TABLE();
};
#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 40d9b726-ab6b-431f-b384-e1a66303dba5

