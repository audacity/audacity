/**********************************************************************

  Audacity: A Digital Audio Editor

  ResetConfig.cpp

  Jithin John

*******************************************************************/
/**

\class ResetConfigDialog
\brief ResetConfigDialog is used for resetting Audacity Preferences.

*/
/*******************************************************************/

#include <wx/checkbox.h>
#include <wx/radiobut.h>
#include "AudacityFileConfig.h"
#include "ProjectSelectionManager.h"
#include "ProjectSettings.h"
#include "ShuttleGui.h"
#include "TempDirectory.h"
#include "../libraries/lib-screen-geometry/Decibels.h"
#include "prefs/PrefsDialog.h"
#include "prefs/ThemePrefs.h"
#include "toolbars/ToolManager.h"
#include "widgets/KeyView.h"

class ResetConfigDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   ResetConfigDialog(wxWindow *parent, AudacityProject &project, const CommandContext &context);

   void MakeResetConfigDialog();

private:
   // WDR: handler declarations
   void OnOK(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnToggleKeyboardSet(wxCommandEvent &event);
   void OnToggleAllConfiguration(wxCommandEvent &event);

   void ApplyKeyboardChanges();
   void FilterKeys(std::vector<NormalizedKeyString> &arr);
   void GetBindings(bool bSort);

   AudacityProject &mProject;
   const CommandContext &mcontext;

   CommandIDs mNames;
   CommandManager *mManager;

   std::vector<NormalizedKeyString> mDefaultKeys; // The full set.
   std::vector<NormalizedKeyString> mKeys;
   std::vector<NormalizedKeyString> mNewKeys;             // Used for work in progress.
   std::vector<NormalizedKeyString> mStandardDefaultKeys; // The reduced set.

   wxCheckBox *mAllConfigurationsCheckbox;
   wxCheckBox *mDirectoriesCheckbox;
   wxCheckBox *mInterfaceCheckBox;
   wxCheckBox *mKeyboardCheckBox;
   wxCheckBox *mRecPlayCheckBox;
   wxRadioButton *mFullCheckBox;
   wxRadioButton *mStandardCheckBox;

private:
   DECLARE_EVENT_TABLE()
};

void RunResetConfig(wxWindow *parent, AudacityProject &project, const CommandContext &context)
{
   ResetConfigDialog dlog{parent, project, context};

   dlog.CentreOnParent();

   dlog.ShowModal();
}

//
// ResetConfigDialog
//

enum
{
   IdDirectoriesCheckbox = 1000,
   IdInterfaceCheckBox,
   IdKeyboardCheckBox,
   IdRecPlayCheckBox,
   IdAllConfigurationCheckbox,
   IdStandardCheckBox,
   IdFullCheckBox,
};

BEGIN_EVENT_TABLE(ResetConfigDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, ResetConfigDialog::OnOK)
EVT_BUTTON(wxID_CANCEL, ResetConfigDialog::OnCancel)
EVT_CHECKBOX(IdKeyboardCheckBox, ResetConfigDialog::OnToggleKeyboardSet)
EVT_CHECKBOX(IdAllConfigurationCheckbox, ResetConfigDialog::OnToggleAllConfiguration)
END_EVENT_TABLE()

ResetConfigDialog::ResetConfigDialog(
    wxWindow *parent, AudacityProject &project, const CommandContext &context)
    : wxDialogWrapper(parent, 0, XO("Reset Configuration"),
                      wxDefaultPosition, wxDefaultSize,
                      wxDEFAULT_DIALOG_STYLE |
                          wxRESIZE_BORDER),
      mProject(project), mcontext(context)
{
   mAllConfigurationsCheckbox = NULL;
   mDirectoriesCheckbox = NULL;
   mInterfaceCheckBox = NULL;
   mKeyboardCheckBox = NULL;
   mRecPlayCheckBox = NULL;
   mStandardCheckBox = NULL;
   mFullCheckBox = NULL;
   SetName();
   MakeResetConfigDialog();
}

// WDR: handler implementations for ResetConfigDialog

void ResetConfigDialog::OnCancel(wxCommandEvent &WXUNUSED(event))
{
   EndModal(0);
}

void ResetConfigDialog::MakeResetConfigDialog()
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   S.StartStatic(XO("\nWARNING : This can reset everything to default such as\n"
                    "the settings, Language, preferences, etc that you have set.\n\n")
                 //"you have set.\n\n")
                 + XO("Select the configurations you want to reset\n\n"));
   {
      S.StartVerticalLay(true);
      {
         S.SetBorder(5);
         //
         // Checkbox for resetting all configurations.
         mAllConfigurationsCheckbox = S.Id(IdAllConfigurationCheckbox)
                                 .AddCheckBox(XXO("All Configurations\n(Does not reset Keyboard Preferences)"), false);
         // Checkbox for resetting the directory preferences.
         mDirectoriesCheckbox = S.Id(IdDirectoriesCheckbox)
                                 .Disable(mAllConfigurationsCheckbox->GetValue())
                                 .AddCheckBox(XXO("Directories Preferences"), false);
         // Checkbox to reset the interface preferences.
         mInterfaceCheckBox = S.Id(IdInterfaceCheckBox)
                                 .Disable(mAllConfigurationsCheckbox->GetValue())
                                 .AddCheckBox(XXO("Interface Preferences"), false);
         // Checkbox for resetting the Playback and Recording preferences.
         mRecPlayCheckBox = S.Id(IdRecPlayCheckBox)
                                 .Disable(mAllConfigurationsCheckbox->GetValue())
                                 .AddCheckBox(XXO("Playback and Recording Preferences"), false);
         // Checkbox for resetting the keyboard preferences.
         mKeyboardCheckBox = S.Id(IdKeyboardCheckBox).AddCheckBox(XXO("Keyboard Preferences"), false);
         S.StartStatic(XO("\nSelect the Layout"));
         {
            S.StartHorizontalLay();
            {
               // Sub option checkboxes for Keyboard Preferences.
               mStandardCheckBox = S.Id(IdStandardCheckBox)
                                       .Disable(!mKeyboardCheckBox->GetValue())
                                       .AddRadioButtonToGroup(XXO("Standard")); // Reset to default Standard Layout.
               mFullCheckBox = S.Id(IdFullCheckBox)
                                 .Disable(!mKeyboardCheckBox->GetValue())
                                 .AddRadioButtonToGroup(XXO("Full")); // Reset to default Full Layout.
               SetSizeHints(GetSize());
            }
            S.EndHorizontalLay();
         }
      S.EndStatic();
      }
      S.EndVerticalLay();
      S.AddStandardButtons(eOkButton | eCancelButton);
      Fit();
   }
   S.EndStatic();
}

void ResetConfigDialog::OnOK(wxCommandEvent &WXUNUSED(event))
{
   
   if (mAllConfigurationsCheckbox->GetValue())
   {
      auto &menuManager = MenuManager::Get(mProject);
      menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypenone;
      menuManager.mLastToolRegistration = MenuCreator::repeattypenone;
      menuManager.mLastGenerator = "";
      menuManager.mLastEffect = "";
      menuManager.mLastAnalyzer = "";
      menuManager.mLastTool = "";

      ResetPreferences();

      // Directory will be reset on next restart.

      FileNames::UpdateDefaultPath(FileNames::Operation::Temp, TempDirectory::DefaultTempDir());

      // There are many more things we could reset here.
      // Beeds discussion as to which make sense to.
      // Maybe in future versions?
      // - Reset Effects
      // - Reset Recording and Playback volumes
      // - Reset Selection formats (and for spectral too)
      // - Reset Play-at-speed speed to x1
      // - Stop playback/recording and unapply pause.
      // - Set Zoom sensibly.

      SyncLockTracks.Reset();
      SoundActivatedRecord.Reset();
      SelectionToolbarMode.Reset();
      gPrefs->Flush();
      DoReloadPreferences(mProject);
      ToolManager::OnResetToolBars(mcontext);

      // These are necessary to preserve the newly correctly laid out toolbars.
      // In particular the Device Toolbar ends up short on next restart,
      // if they are left out.
      AudacityPrefsVersionString.Reset();

      // write out the version numbers to the prefs file for future checking
      AudacityMajor.Reset();
      AudacityRelease.Reset();
      AudacityRevision.Reset();
      gPrefs->Flush();

      ProjectSelectionManager::Get(mProject)
          .AS_SetSnapTo(gPrefs->ReadLong("/SnapTo", SNAP_OFF));
      ProjectSelectionManager::Get(mProject)
          .AS_SetRate(gPrefs->ReadDouble("/DefaultProjectSampleRate", 44100.0));
   }

   if (mDirectoriesCheckbox->GetValue())
   {
      gPrefs->DeleteGroup(wxT("/Directories"));
      gPrefs->Flush();

      FileNames::UpdateDefaultPath(FileNames::Operation::Temp, TempDirectory::DefaultTempDir());
   }

   if (mInterfaceCheckBox->GetValue())
   {
      // Resetting the GUI Preferences

      Language.Reset();
      HelpLocation.Reset();
      ThemeSetting.Reset();
      DecibelScaleCutoff.Reset();
      ShowSplashScreen.Reset();
      ShowExtraMenus.Reset();
      BeepOnCompletion.Reset();
      RetainLabels.Reset();
      GUIBlendThemes.Reset();
      RtlWorkaround.Reset();
      ToolTips.Reset();
      ScrubbingEnabled.Reset();
      gPrefs->Flush();

      ThemePrefs::ApplyUpdatedImages();
      DoReloadPreferences(mProject);
   }

   if (mRecPlayCheckBox->GetValue())
   {
      // Resetting the Playback and Recording preferences
      PreferNewTrackRecord.Reset();
      DropoutDetected.Reset();
      RecordingNameCustom.Reset();
      TrackNumber.Reset();
      DateStamp.Reset();
      TimeStamp.Reset();
      gPrefs->DeleteGroup(wxT("/AudioIO"));
      gPrefs->Flush();
   }

   if (mKeyboardCheckBox->GetValue())
   {
      // Reset Keyboard Preferences to Standard/Full.
      bool defaultSet = false;
      if (mStandardCheckBox->GetValue())
      {
         defaultSet = true;
      }
      mManager = &CommandManager::Get(mProject);

      GetBindings(false);
      gPrefs->DeleteEntry(wxT("/GUI/Shortcuts/FullDefaults"));
      gPrefs->Flush();

      mNewKeys = mDefaultKeys;

      if (defaultSet)
         FilterKeys(mNewKeys);
      for (size_t i = 0; i < mNewKeys.size(); i++)
      {
         mManager->SetKeyFromIndex(i, mNewKeys[i]);
      }
      GetBindings(true);
      ApplyKeyboardChanges();
   }

   EndModal(0);
}

// Toggle Checkbox acceess of sub options according to the state of main Keyboard Layout checkbox.
void ResetConfigDialog::OnToggleKeyboardSet(wxCommandEvent & /* Evt */)
{
   bool mKeyboardCheckBoxVal = (mKeyboardCheckBox->GetValue());
   mStandardCheckBox->Enable(mKeyboardCheckBoxVal);
   mFullCheckBox->Enable(mKeyboardCheckBoxVal);
}

// Toggle Checkbox acceess of other options according to the state of All Configuration checkbox.
void ResetConfigDialog::OnToggleAllConfiguration(wxCommandEvent & /* Evt */)
{
   bool mAllConfigurationsCheckBoxVal = !(mAllConfigurationsCheckbox->GetValue());
   mDirectoriesCheckbox->Enable(mAllConfigurationsCheckBoxVal);
   mInterfaceCheckBox->Enable(mAllConfigurationsCheckBoxVal);
   mRecPlayCheckBox->Enable(mAllConfigurationsCheckBoxVal);
}

// Filter out to get the Standard Layout keys
void ResetConfigDialog::FilterKeys(std::vector<NormalizedKeyString> &arr)
{
   const auto &MaxListOnly = CommandManager::ExcludedList();

   // Remove items that are in MaxList.
   for (size_t i = 0; i < arr.size(); i++)
   {
      if (std::binary_search(MaxListOnly.begin(), MaxListOnly.end(), arr[i]))
         arr[i] = {};
   }
}

// Apply the keyboard layout changes to the file.
void ResetConfigDialog::ApplyKeyboardChanges()
{
   bool bFull = gPrefs->ReadBool(wxT("/GUI/Shortcuts/FullDefaults"), false);
   for (size_t i = 0; i < mNames.size(); i++)
   {
      const auto &dkey = bFull ? mDefaultKeys[i] : mStandardDefaultKeys[i];
      // using GET to interpret CommandID as a config path component
      auto name = wxT("/NewKeys/") + mNames[i].GET();
      const auto &key = mNewKeys[i];

      if (gPrefs->HasEntry(name))
      {
         if (key != NormalizedKeyString{gPrefs->ReadObject(name, key)})
         {
            gPrefs->Write(name, key);
         }
         if (key == dkey)
         {
            gPrefs->DeleteEntry(name);
         }
      }
      else
      {
         if (key != dkey)
         {
            gPrefs->Write(name, key);
         }
      }
   }

   gPrefs->Flush();
}

void ResetConfigDialog::GetBindings(bool bSort)
{
   TranslatableStrings Labels;
   TranslatableStrings Categories;
   TranslatableStrings Prefixes;

   mNames.clear();
   mKeys.clear();
   mDefaultKeys.clear();
   mStandardDefaultKeys.clear();
   mManager->GetAllCommandData(
       mNames,
       mKeys,
       mDefaultKeys,
       Labels,
       Categories,
       Prefixes,
       true);

   mStandardDefaultKeys = mDefaultKeys;
   FilterKeys(mStandardDefaultKeys);

   mNewKeys = mKeys;
}