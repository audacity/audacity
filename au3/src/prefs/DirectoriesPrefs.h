/**********************************************************************

  Audacity: A Digital Audio Editor

  DirectoriesPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_DIRECTORIES_PREFS__
#define __AUDACITY_DIRECTORIES_PREFS__

#include "PrefsPanel.h"

class ReadOnlyText;
class ShuttleGui;

class wxStaticText;
class wxTextCtrl;

#define DIRECTORIES_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Directories") }

class DirectoriesPrefs final : public PrefsPanel
{
public:
    DirectoriesPrefs(wxWindow* parent, wxWindowID winid);
    ~DirectoriesPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    bool Validate() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void Populate();

    void OnTempText(wxCommandEvent& evt);
    void OnTempBrowse(wxCommandEvent& evt);

    void OnBrowse(wxCommandEvent& evt);
    void OnText(wxCommandEvent& evt);

    ReadOnlyText* mFreeSpace;
    wxTextCtrl* mTempText;
    wxTextCtrl* mOpenText;
    wxTextCtrl* mSaveText;
    wxTextCtrl* mImportText;
    wxTextCtrl* mExportText;
    wxTextCtrl* mMacrosText;

    DECLARE_EVENT_TABLE()
};

/// A PrefsPanel::Factory that creates one DirectoriesPrefs panel.
/// This one is used not only in the Preferences command.
extern PrefsPanel::Factory DirectoriesPrefsFactory();
#endif
