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

   int SortItems(long item1, long item2);

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void CreateList();
   void RepopulateBindingsList();
   wxString NameFromKey( const wxString & key );
   void SetKeyForSelected( const wxString & key );
   void Sort(int column);

   void OnDefaults(wxCommandEvent & e);
   void OnImport(wxCommandEvent & e);
   void OnExport(wxCommandEvent & e);
   void OnSet(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnCategory(wxCommandEvent & e);
   void OnSort(wxListEvent & e);
   void OnItemSelected(wxListEvent & e);
   void OnKeyDown(wxListEvent & e);

   void OnCaptureKeyDown(wxKeyEvent & e);
   void OnCaptureChar(wxKeyEvent & e);

   wxChoice *mCat;
   wxTextCtrl *mKey;
   wxListCtrl *mList;

   CommandManager *mManager;
   int mCommandSelected;

   wxArrayString mCats;
   wxArrayString mNames;
   wxArrayString mLabels;
   wxArrayString mDefaultKeys;
   wxArrayString mKeys;
   wxArrayString mNewKeys; // Used for work in progress.

   wxArrayString *mSortCol;
   int mSortDir;

   DECLARE_EVENT_TABLE();
};
#endif
