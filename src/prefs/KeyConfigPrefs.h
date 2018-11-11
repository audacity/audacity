/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

class CommandManager;
class ShuttleGui;

#include <wx/defs.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/srchctrl.h>
#include <wx/timer.h>

#include "../widgets/KeyView.h"

#include "PrefsPanel.h"

class wxStaticText;
class wxTextCtrl;
struct NormalizedKeyString;

class KeyConfigPrefs final : public PrefsPanel
{
public:
   KeyConfigPrefs(wxWindow * parent, wxWindowID winid, const CommandID &name);
   bool Commit() override;
   void Cancel() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

private:
   void Populate();
   void RefreshBindings(bool bSort);
   void FilterKeys( std::vector<NormalizedKeyString> & arr );
   CommandID NameFromKey(const NormalizedKeyString & key);
   void SetKeyForSelected(const NormalizedKeyString & key);

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

   CommandIDs mNames;
   std::vector<NormalizedKeyString> mDefaultKeys; // The full set.
   std::vector<NormalizedKeyString> mStandardDefaultKeys; // The reduced set.
   std::vector<NormalizedKeyString> mKeys;
   std::vector<NormalizedKeyString> mNewKeys; // Used for work in progress.

   DECLARE_EVENT_TABLE()
};


/// A PrefsPanelFactory that creates one KeyConfigPrefs panel.
class KeyConfigPrefsFactory final : public PrefsPanelFactory
{
public:
   KeyConfigPrefsFactory(const CommandID &name = {})
      : mName{ name } {}
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;

private:
   CommandID mName;
};
#endif
