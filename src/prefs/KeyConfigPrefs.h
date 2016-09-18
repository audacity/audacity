/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include "../Experimental.h"

class ShuttleGui;

#if defined(EXPERIMENTAL_KEY_VIEW)

#include <wx/defs.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/srchctrl.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

#include "../commands/CommandManager.h"
#include "../widgets/KeyView.h"

#include "PrefsPanel.h"

class wxStaticText;

class KeyConfigPrefs final : public PrefsPanel
{
public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   bool Apply() override;
   void Cancel() override;

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void RefreshBindings();
   wxString NameFromKey(const wxString & key);
   void SetKeyForSelected(const wxString & key);

   void OnViewBy(wxCommandEvent & e);
   void OnDefaults(wxCommandEvent & e);
   void OnImport(wxCommandEvent & e);
   void OnExport(wxCommandEvent & e);
   void OnSet(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSelected(wxCommandEvent & e);

   void OnHotkeyKeyDown(wxKeyEvent & e);
   void OnHotkeyChar(wxKeyEvent & e);
   void OnHotkeyKillFocus(wxFocusEvent & e);

   void OnFilterTimer(wxTimerEvent & e);
   void OnFilterKeyDown(wxKeyEvent & e);
   void OnFilterChar(wxKeyEvent & e);

   KeyView *mView;
   wxTextCtrl *mKey;
   wxButton *mSet;
   wxButton *mClear;

   wxTextCtrl *mFilter;
   wxStaticText *mFilterLabel;
   wxTimer mFilterTimer;
   bool mFilterPending;

   ViewByType mViewType;
   wxRadioButton *mViewByTree;
   wxRadioButton *mViewByName;
   wxRadioButton *mViewByKey;

   CommandManager *mManager;
   int mCommandSelected;

   wxArrayString mNames;
   wxArrayString mDefaultKeys;
   wxArrayString mKeys;
   wxArrayString mNewKeys; // Used for work in progress.

   DECLARE_EVENT_TABLE()
};

#else

#include <wx/defs.h>
#include <wx/listctrl.h>
#include <wx/textctrl.h>
#include <wx/string.h>

#include "../commands/CommandManager.h"

#include "PrefsPanel.h"

class KeyConfigPrefs final : public PrefsPanel
{
 public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   bool Apply() override;
   void Cancel() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void CreateList();
   void RepopulateBindingsList();
   wxString NameFromKey( const wxString & key );
   void SetKeyForSelected( const wxString & key );

   void OnDefaults(wxCommandEvent & e);
   void OnImport(wxCommandEvent & e);
   void OnExport(wxCommandEvent & e);
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
   int mCommandSelected;

   wxArrayString mCats;
   wxArrayString mNames;
   wxArrayString mDefaultKeys;
   wxArrayString mKeys;
   wxArrayString mNewKeys; // Used for work in progress.

   DECLARE_EVENT_TABLE()
};

#endif

class KeyConfigPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
