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
   bool Commit() override;
   void Cancel() override;
   wxString HelpPageName() override;

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void RefreshBindings(bool bSort);
   void FilterKeys( wxArrayString & arr );
   wxString NameFromKey(const wxString & key);
   void SetKeyForSelected(const wxString & key);

   void OnViewBy(wxCommandEvent & e);
   void OnDefaults(wxCommandEvent & e);
   void OnImportDefaults(wxCommandEvent & e);
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
   wxArrayString mDefaultKeys; // The full set.
   wxArrayString mStandardDefaultKeys; // The reduced set.
   wxArrayString mKeys;
   wxArrayString mNewKeys; // Used for work in progress.

   DECLARE_EVENT_TABLE()
};


class KeyConfigPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
