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
#include <wx/timer.h> // member variable

#include "PrefsPanel.h"

class wxRadioButton;
class wxStaticText;
class wxTextCtrl;
class KeyView;
struct NormalizedKeyString;
enum ViewByType : int;

#define KEY_CONFIG_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Key Config") }

class KeyConfigPrefs final : public PrefsPanel
{
public:
   KeyConfigPrefs(wxWindow * parent, wxWindowID winid,
      AudacityProject *pProject,
      const CommandID &name);
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   bool Commit() override;
   void Cancel() override;
   ManualPageID HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

private:
   void Populate();
   void RefreshBindings(bool bSort);
   void RefreshKeyInfo();
   void ClearAllKeys();
   bool ContainsIllegalDups(TranslatableString & fMatching, 
      TranslatableString & sMatching) const;
   TranslatableString MergeWithExistingKeys(
      const std::vector<NormalizedKeyString> &toAdd);
   void FilterKeys( std::vector<NormalizedKeyString> & arr );
   CommandID NameFromKey(const NormalizedKeyString & key);
   void SetKeyForSelected(const NormalizedKeyString & key);

   // See bug #2315 for discussion. This should be reviewed
   // and (possibly) removed after wx3.1.3.
   void OnShow(wxShowEvent & e);

   void OnViewBy(wxCommandEvent & e);
   void OnDefaults(wxCommandEvent & e);
   void OnImportDefaults( int id );
   void OnImport(wxCommandEvent & e);
   void OnExport(wxCommandEvent & e);
   void OnSet(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSelected(wxCommandEvent & e);

   void OnHotkeyKeyDown(wxKeyEvent & e);
   void OnHotkeyChar(wxEvent & e);
   void OnHotkeyKillFocus(wxEvent & e);
   void OnHotkeyContext(wxEvent & e);

   void OnFilterTimer(wxTimerEvent & e);
   void OnFilterKeyDown(wxKeyEvent & e);
   void OnFilterChar(wxEvent & e);

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

   AudacityProject *mProject{};

   CommandManager *mManager;
   int mCommandSelected;

   CommandIDs mNames;
   std::vector<NormalizedKeyString> mDefaultKeys; // The full set.
   std::vector<NormalizedKeyString> mStandardDefaultKeys; // The reduced set.
   std::vector<NormalizedKeyString> mKeys;
   std::vector<NormalizedKeyString> mNewKeys; // Used for work in progress.

   DECLARE_EVENT_TABLE()
};


/// A PrefsPanel::Factory that creates one KeyConfigPrefs panel.
/// This factory can be parametrized by name, which specifies a command to be
/// focused initially
extern PrefsPanel::Factory KeyConfigPrefsFactory(
   const CommandID &name = {} );
#endif
