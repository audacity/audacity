/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson
  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class KeyConfigPrefs
\brief A PrefsPanel for keybindings.

The code for displaying keybindings is similar to code in MousePrefs.
It would be nice to create a NEW 'Bindings' class which both
KeyConfigPrefs and MousePrefs use.

*//*********************************************************************/



#include "KeyConfigPrefs.h"

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/menu.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>

#include "ActiveProject.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectWindows.h"
#include "../commands/CommandManager.h"
#include "XMLFileReader.h"

#include "SelectFile.h"
#include "ShuttleGui.h"

#include "FileNames.h"

#include "BasicMenu.h"
#include "../widgets/KeyView.h"
#include "AudacityMessageBox.h"
#include "wxWidgetsWindowPlacement.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

//
// KeyConfigPrefs
//
#define AssignDefaultsButtonID  17001
#define CurrentComboID          17002
#define SetButtonID             17003
#define ClearButtonID           17004
#define CommandsListID          17005
#define ExportButtonID          17006
#define ImportButtonID          17007
#define FilterID                17008
#define ViewByTreeID            17009
#define ViewByNameID            17010
#define ViewByKeyID             17011
#define FilterTimerID           17012

// EMPTY_SHORTCUT means "user chose to have no shortcut"
#define EMPTY_SHORTCUT          ("")
// NO_SHORTCUT means "user made no choice"
#define NO_SHORTCUT             (wxString)((wxChar)7)

BEGIN_EVENT_TABLE(KeyConfigPrefs, PrefsPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::OnDefaults)
   EVT_BUTTON(SetButtonID, KeyConfigPrefs::OnSet)
   EVT_BUTTON(ClearButtonID, KeyConfigPrefs::OnClear)
   EVT_BUTTON(ExportButtonID, KeyConfigPrefs::OnExport)
   EVT_BUTTON(ImportButtonID, KeyConfigPrefs::OnImport)
   EVT_LISTBOX(CommandsListID, KeyConfigPrefs::OnSelected)
   EVT_RADIOBUTTON(ViewByTreeID, KeyConfigPrefs::OnViewBy)
   EVT_RADIOBUTTON(ViewByNameID, KeyConfigPrefs::OnViewBy)
   EVT_RADIOBUTTON(ViewByKeyID, KeyConfigPrefs::OnViewBy)
   EVT_TIMER(FilterTimerID, KeyConfigPrefs::OnFilterTimer)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(
   wxWindow * parent, wxWindowID winid, AudacityProject *pProject,
   const CommandID &name)
/* i18n-hint: as in computer keyboard (not musical!) */
:  PrefsPanel(parent, winid, XO("Keyboard")),
   mView(NULL),
   mKey(NULL),
   mFilter(NULL),
   mFilterTimer(this, FilterTimerID),
   mFilterPending(false)
   , mProject{ pProject }
{
   Populate();
   if (!name.empty()) {
      auto index = mView->GetIndexByName(name);
      mView->SelectNode(index);
   }

   // See bug #2315 for discussion. This should be reviewed
   // and (possibly) removed after wx3.1.3.
   Bind(wxEVT_SHOW, &KeyConfigPrefs::OnShow, this);
}

ComponentInterfaceSymbol KeyConfigPrefs::GetSymbol() const
{
   return KEY_CONFIG_PREFS_PLUGIN_SYMBOL;
}

TranslatableString KeyConfigPrefs::GetDescription() const
{
   return XO("Preferences for KeyConfig");
}

ManualPageID KeyConfigPrefs::HelpPageName()
{
   return "Keyboard_Preferences";
}

void KeyConfigPrefs::Populate()
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   if (!mProject) {
      S.StartVerticalLay(true);
      {
         S.StartStatic( {}, true);
         {
            S.AddTitle(XO("Keyboard preferences currently unavailable."));
            S.AddTitle(XO("Open a new project to modify keyboard shortcuts."));
         }
         S.EndStatic();
      }
      S.EndVerticalLay();

      return;
   }

   PopulateOrExchange(S);

   mCommandSelected = wxNOT_FOUND;

   mManager = &CommandManager::Get( *mProject );

   // For speed, don't sort here.  We're just creating.
   // Instead sort when we do SetView later in this function.
   RefreshBindings(false);

   if (mViewByTree->GetValue()) {
      mViewType = ViewByTree;
   }
   else if (mViewByName->GetValue()) {
      mViewType = ViewByName;
   }
   else if (mViewByKey->GetValue()) {
      mViewType = ViewByKey;
      mFilterLabel->SetLabel(_("&Hotkey:"));
      mFilter->SetName(wxStripMenuCodes(mFilterLabel->GetLabel()));
   }

   mView->SetView(mViewType);
}

/// Normally in classes derived from PrefsPanel this function
/// is used both to populate the panel and to exchange data with it.
/// With KeyConfigPrefs all the exchanges are handled specially,
/// so this is only used in populating the panel.
void KeyConfigPrefs::PopulateOrExchange(ShuttleGui & S)
{
   ChoiceSetting Setting{ L"/Prefs/KeyConfig/ViewBy",
      {
         { wxT("tree"), XXO("&Tree") },
         { wxT("name"), XXO("&Name") },
         { wxT("key"), XXO("&Key") },
      },
      0 // tree
   };

   S.SetBorder(2);

   S.StartStatic(XO("Key Bindings"), 1);
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.Position(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL).AddTitle(XO("View by:"));

         // Bug 2692: Place button group in panel so tabbing will work and,
         // on the Mac, VoiceOver will announce as radio buttons.
         S.StartPanel();
         {
            S.StartHorizontalLay();
            {
               S.StartRadioButtonGroup(Setting);
               {
                  mViewByTree = S.Id(ViewByTreeID)
                     .Name(XO("View by tree"))
                     .TieRadioButton();
                  mViewByName = S.Id(ViewByNameID)
                     .Name(XO("View by name"))
                     .TieRadioButton();
                  mViewByKey = S.Id(ViewByKeyID)
                     .Name(XO("View by key"))
                     .TieRadioButton();
#if !defined(__WXMAC__) && wxUSE_ACCESSIBILITY
                  // so that name can be set on a standard control
                  if (mViewByTree) mViewByTree->SetAccessible(safenew WindowAccessible(mViewByTree));
                  if (mViewByName) mViewByName->SetAccessible(safenew WindowAccessible(mViewByName));
                  if (mViewByKey) mViewByKey->SetAccessible(safenew WindowAccessible(mViewByKey));
#endif
               }
               S.EndRadioButtonGroup();
            }
            S.EndHorizontalLay();
         }
         S.EndPanel();

         S.AddSpace(wxDefaultCoord, wxDefaultCoord, 1);

         S.StartHorizontalLay(wxALIGN_CENTER_VERTICAL, 0);
         {
            mFilterLabel = S.Position(wxALIGN_CENTER_VERTICAL).AddVariableText(XO("Searc&h:"));

            if (!mFilter) {
               mFilter = safenew wxTextCtrl(S.GetParent(),
                                        FilterID,
                                        wxT(""),
                                        wxDefaultPosition,
#if defined(__WXMAC__)
                                        wxSize(300, -1),
#else
                                        wxSize(210, -1),
#endif
                                        wxTE_PROCESS_ENTER);
               mFilter->SetName(wxStripMenuCodes(mFilterLabel->GetLabel()));
            }
            S.Position(wxALIGN_NOT | wxALIGN_LEFT)
               .ConnectRoot(wxEVT_KEY_DOWN,
                            &KeyConfigPrefs::OnFilterKeyDown)
               .ConnectRoot(wxEVT_CHAR,
                            &KeyConfigPrefs::OnFilterChar)
               .AddWindow(mFilter);
         }
         S.EndHorizontalLay();
      }
      S.EndHorizontalLay();

      S.AddSpace(wxDefaultCoord, 2);

      S.StartHorizontalLay(wxEXPAND, 1);
      {
         if (!mView) {
            mView = safenew KeyView(S.GetParent(), CommandsListID);
            mView->SetName(_("Bindings"));
         }
         S.Prop(true)
            .Position(wxEXPAND)
            .AddWindow(mView);
      }
      S.EndHorizontalLay();

      S.StartThreeColumn();
      {
         if (!mKey) {
            mKey = safenew wxTextCtrl(S.GetParent(),
                                  CurrentComboID,
                                  wxT(""),
                                  wxDefaultPosition,
#if defined(__WXMAC__)
                                  wxSize(300, -1),
#else
                                  wxSize(210, -1),
#endif
                                  wxTE_PROCESS_ENTER);
#if !defined(__WXMAC__) && wxUSE_ACCESSIBILITY
            // so that name can be set on a standard control
            mKey->SetAccessible(safenew WindowAccessible(mKey));
#endif
            mKey->SetName(_("Short cut"));
         }
         S
            .ConnectRoot(wxEVT_KEY_DOWN,
                      &KeyConfigPrefs::OnHotkeyKeyDown)
            .ConnectRoot(wxEVT_CHAR,
                      &KeyConfigPrefs::OnHotkeyChar)
            .ConnectRoot(wxEVT_KILL_FOCUS,
                      &KeyConfigPrefs::OnHotkeyKillFocus)
            .ConnectRoot(wxEVT_CONTEXT_MENU,
                      &KeyConfigPrefs::OnHotkeyContext)
            .AddWindow(mKey);

         /* i18n-hint: (verb)*/
         mSet = S.Id(SetButtonID).AddButton(XXO("&Set"));
         /* i18n-hint: (verb)*/
         mClear = S.Id(ClearButtonID).AddButton(XXO("Cl&ear"));
      }
      S.EndThreeColumn();

#if defined(__WXMAC__)
      S.AddFixedText(XO("Note: Pressing Cmd+Q will quit. All other keys are valid."));
#endif

      S.StartThreeColumn();
      {
         S.Id(ImportButtonID).AddButton(XXO("&Import..."));
         S.Id(ExportButtonID).AddButton(XXO("&Export..."));
         S.Id(AssignDefaultsButtonID).AddButton(XXO("&Defaults"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();


   // Need to layout so that the KeyView is properly sized before populating.
   // Otherwise, the initial selection is not scrolled into view.
   Layout();
}

void KeyConfigPrefs::RefreshBindings(bool bSort)
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
      true); // True to include effects (list items), false otherwise.

   mStandardDefaultKeys = mDefaultKeys;
   FilterKeys( mStandardDefaultKeys );

   mView->RefreshBindings(mNames,
                          Categories,
                          Prefixes,
                          Labels,
                          mKeys,
                          bSort);
   //Not needed as NEW nodes are already shown expanded.
   //mView->ExpandAll();

   mNewKeys = mKeys;
}

// RefreshKeyInfo is used to update mKeys vector only
// Introduced for efficiency purposes to avoid unnecessary usage of RefreshBinding
void KeyConfigPrefs::RefreshKeyInfo()
{
   mKeys.clear();

   for (const auto & name : mNames)
      mKeys.push_back(mManager->GetKeyFromName(name));
}

// Removes all shortcuts
// Doesn't call RefreshBindings()
void KeyConfigPrefs::ClearAllKeys()
{
   const NormalizedKeyString noKey{ NO_SHORTCUT };
   for (const auto & command : mNames)
      mManager->SetKeyFromName(command, noKey);
}

// Checks if the given vector of keys contains illegal duplicates.
// In case it does, stores the prefixed labels of operations 
// with illegal key duplicates in fMatching and sMatching.
// Search for duplicates fully implemented here 
// to avoid possible problems with legal shortcut duplicates.  
bool KeyConfigPrefs::ContainsIllegalDups(
   TranslatableString & fMatching, TranslatableString & sMatching) const
{
   using IndexesArray = std::vector<int>;
   std::unordered_map<NormalizedKeyString, IndexesArray> seen;

   for (size_t i{ 0 }; i < mKeys.size(); i++)
   {
      if (mKeys[i] == EMPTY_SHORTCUT || mKeys[i] == NO_SHORTCUT)
         continue;

      if (seen.count(mKeys[i]) == 0)
         seen.insert({ mKeys[i], {(int)i} });
      else
      {
         IndexesArray checkMe{ seen.at(mKeys[i]) };
         for (int index : checkMe)
         {
            if (mDefaultKeys[i] == EMPTY_SHORTCUT ||
               mDefaultKeys[i] != mDefaultKeys[index])
            {
               fMatching = mManager->GetPrefixedLabelFromName(mNames[i]);
               sMatching = mManager->GetPrefixedLabelFromName(mNames[index]);
               return true;
            }
            else
               seen.at(mKeys[i]).push_back(index);
         }
      }
   }
   return false;
}


// This function tries to add the given shortcuts(keys) "toAdd" 
// to the already existing shortcuts(keys). Shortcuts are added only if 
//      1. the shortcut for the operation isn't defined already
//      2. the added shortcut doesn't create illegal shortcut duplicate
// The names of operations for which the second condition was violated 
// are returned in a single error message
TranslatableString KeyConfigPrefs::MergeWithExistingKeys(
   const std::vector<NormalizedKeyString> &toAdd)
{
   TranslatableString disabledShortcuts;

   auto searchAddInKeys = [&](size_t index)
   {
      for (size_t k{ 0 }; k < toAdd.size(); k++)
         if (k == index)
            continue;
         else if (toAdd[index] == mKeys[k] &&
            (mDefaultKeys[k] == EMPTY_SHORTCUT ||
             mDefaultKeys[k] != mDefaultKeys[index]))
            return (int)k;

      return -1;
   };
   
   const NormalizedKeyString noKey{ EMPTY_SHORTCUT };

   for (size_t i{ 0 }; i < toAdd.size(); i++)
   {
      if (mKeys[i] != NO_SHORTCUT)
         continue;
      else if (toAdd[i] == EMPTY_SHORTCUT)
         mManager->SetKeyFromIndex(i, noKey);
      else
      {
         int sRes{ searchAddInKeys(i) };

         if (sRes == -1)
            mManager->SetKeyFromIndex(i, toAdd[i]);
         else
         {
            TranslatableString name{ mManager->GetKeyFromName(mNames[sRes]).GET(), {} };
            
            disabledShortcuts +=
               XO(
"\n   *   \"%s\"  (because the shortcut \'%s\' is used by \"%s\")\n")
                  .Format(
                     mManager->GetPrefixedLabelFromName(mNames[i]),
                     name,
                     mManager->GetPrefixedLabelFromName(mNames[sRes]) );
            
            mManager->SetKeyFromIndex(i, noKey);
         }
      }
   }

   return disabledShortcuts;
}

// See bug #2315 for discussion. This should be reviewed
// and (possibly) removed after wx3.1.3.
void KeyConfigPrefs::OnShow(wxShowEvent & event)
{
   event.Skip();

   // This is required to prevent a crash if Preferences 
   // were opened without a project.
   if (event.IsShown() && mView != nullptr)
   {
      mView->Refresh();
   }
}

void KeyConfigPrefs::OnImport(wxCommandEvent & WXUNUSED(event))
{
   wxString file = wxT("Audacity-keys.xml");

   file = SelectFile(FileNames::Operation::Open,
      XO("Select an XML file containing Audacity keyboard shortcuts..."),
      wxEmptyString,
      file,
      wxT(""),
      { FileNames::XMLFiles, FileNames::AllFiles },
      wxRESIZE_BORDER,
      this);

   if (!file) {
      return;
   }

   // this RefreshKeyInfo is here to account for 
   // potential OnSet() function executions before importing
   RefreshKeyInfo();

   // saving pre-import settings
   const std::vector<NormalizedKeyString> oldKeys{ mKeys };

   // clearing all pre-import settings
   ClearAllKeys();

   // getting new settings
   XMLFileReader reader;
   if (!reader.Parse(mManager, file)) {
      AudacityMessageBox(
         reader.GetErrorStr(),
         XO("Error Importing Keyboard Shortcuts"),
         wxOK | wxCENTRE,
         this);
   }

   RefreshKeyInfo();

   // checking new setting for duplicates
   // if there are duplicates, throwing error and returning to pre-import state
   TranslatableString fMatching;
   TranslatableString sMatching;

   if (ContainsIllegalDups(fMatching, sMatching))
   {
      // restore the old pre-import hotkeys stored in oldKeys
      for (size_t k{ 0 }; k < mNames.size(); k++)
         mManager->SetKeyFromName(mNames[k], oldKeys[k]);
      mKeys = oldKeys;

      // output an error message
      AudacityMessageBox(
         XO(
"The file with the shortcuts contains illegal shortcut duplicates for \"%s\" and \"%s\".\nNothing is imported.")
            .Format( fMatching, sMatching ),
         XO("Error Importing Keyboard Shortcuts"),
         wxICON_ERROR | wxCENTRE, this);

      // stop the function
      return;
   }

   // adding possible old settings to the new settings and recording the conflicting ones
   TranslatableString disabledShortcuts{ MergeWithExistingKeys(oldKeys) };

   RefreshBindings(true);
   
   TranslatableString message{ 
      XO("Loaded %d keyboard shortcuts\n").Format(mManager->GetNumberOfKeysRead()) };

   if (disabledShortcuts.Translation() != (""))
      message += XO("\nThe following commands are not mentioned in the imported file, "
         "but have their shortcuts removed because of the conflict with other new shortcuts:\n") +
         disabledShortcuts;

   AudacityMessageBox(message, XO("Loading Keyboard Shortcuts"), wxOK | wxCENTRE);
}

void KeyConfigPrefs::OnExport(wxCommandEvent & WXUNUSED(event))
{
   wxString file = wxT("Audacity-keys.xml");

   file = SelectFile(FileNames::Operation::Export,
      XO("Export Keyboard Shortcuts As:"),
      wxEmptyString,
      file,
      wxT("xml"),
      { FileNames::XMLFiles, FileNames::AllFiles },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      this);

   if (!file) {
      return;
   }

   GuardedCall( [&] {
      XMLFileWriter prefFile{ file, XO("Error Exporting Keyboard Shortcuts") };
      mManager->WriteXML(prefFile);
      prefFile.Commit();
   } );
}



// There currently is only one clickable AButton
// so we just do what it needs.
void KeyConfigPrefs::OnDefaults(wxCommandEvent & WXUNUSED(event))
{
   wxMenu Menu;
   Menu.Append( 1, _("Standard") );
   Menu.Append( 2, _("Full") );
   Menu.Bind( wxEVT_COMMAND_MENU_SELECTED, &KeyConfigPrefs::OnImportDefaults, this );
   BasicMenu::Handle( &Menu ).Popup( wxWidgetsWindowPlacement{ this } );
}

void KeyConfigPrefs::FilterKeys( std::vector<NormalizedKeyString> & arr )
{
   const auto &MaxListOnly = CommandManager::ExcludedList();

   // Remove items that are in MaxList.
   for (size_t i = 0; i < arr.size(); i++) {
      if( std::binary_search(MaxListOnly.begin(), MaxListOnly.end(), arr[i]) )
         arr[i] = {};
   }
}

void KeyConfigPrefs::OnImportDefaults(wxCommandEvent & event)
{
   gPrefs->DeleteEntry(wxT("/GUI/Shortcuts/FullDefaults"));
   gPrefs->Flush();

   mNewKeys = mDefaultKeys;
   if( event.GetId() == 1 )
      FilterKeys( mNewKeys );

   for (size_t i = 0; i < mNewKeys.size(); i++) {
      mManager->SetKeyFromIndex(i, mNewKeys[i]);
   }

   RefreshBindings(true);
}

void KeyConfigPrefs::OnHotkeyKeyDown(wxKeyEvent & e)
{
   wxTextCtrl *t = (wxTextCtrl *)e.GetEventObject();

   // Make sure we can navigate away from the hotkey textctrl.
   // On Linux and OSX, it can get stuck, but it doesn't hurt
   // to do it for Windows as well.
   //
   // Mac note:  Don't waste time trying to figure out why the
   // focus goes back to the prefs tree.  Unless Voiceover is
   // active, buttons on the Mac do not accept focus and all the
   // controls between this one and the tree control are buttons.
   if (e.GetKeyCode() == WXK_TAB) {
      t->Navigate(e.ShiftDown()
                 ? wxNavigationKeyEvent::IsBackward
                 : wxNavigationKeyEvent::IsForward);
      return;
   }

   t->SetValue(KeyEventToKeyString(e).Display());
}

void KeyConfigPrefs::OnHotkeyChar(wxEvent & WXUNUSED(e))
{
   // event.Skip() not performed, so event will not be processed further.
}

void KeyConfigPrefs::OnHotkeyKillFocus(wxEvent & e)
{
   if (mKey->GetValue().empty() && mCommandSelected != wxNOT_FOUND) {
      mKey->AppendText(mView->GetKey(mCommandSelected).Display());
   }

   e.Skip();
}

void KeyConfigPrefs::OnHotkeyContext(wxEvent & WXUNUSED(e))
{
   // event.Skip() not performed, so event will not be processed further.
}

void KeyConfigPrefs::OnFilterTimer(wxTimerEvent & WXUNUSED(e))
{
   // The filter timer has expired, so set the filter
   if (mFilterPending)
   {
      // Do not reset mFilterPending here...possible race
      mView->SetFilter(mFilter->GetValue());
   }
}

void KeyConfigPrefs::OnFilterKeyDown(wxKeyEvent & e)
{
   wxTextCtrl *t = (wxTextCtrl *)e.GetEventObject();
   int keycode = e.GetKeyCode();

   // Make sure we can navigate away from the hotkey textctrl.
   // On Linux and OSX, it an get stuck, but it doesn't hurt
   // to do it for Windows as well.
   if (keycode == WXK_TAB) {
      wxNavigationKeyEvent nevent;
      nevent.SetWindowChange(e.ControlDown());
      nevent.SetDirection(!e.ShiftDown());
      nevent.SetEventObject(t);
      nevent.SetCurrentFocus(t);
      t->GetParent()->GetEventHandler()->ProcessEvent(nevent);

      return;
   }

   if (mViewType == ViewByKey) {
      wxString key = KeyEventToKeyString(e).Display();
      t->SetValue(key);

      if (!key.empty()) {
         mView->SetFilter(t->GetValue());
      }
   }
   else
   {
      if (keycode == WXK_RETURN) {
         mFilterPending = false;
         mView->SetFilter(t->GetValue());
      }
      else {
         mFilterPending = true;
         mFilterTimer.Start(500, wxTIMER_ONE_SHOT);

         e.Skip();
      }
   }
}

void KeyConfigPrefs::OnFilterChar(wxEvent & e)
{
   if (mViewType != ViewByKey)
   {
      e.Skip();
   }
}

// Given a hotkey combination, returns the name (description) of the
// corresponding command, or the empty string if none is found.
CommandID KeyConfigPrefs::NameFromKey(const NormalizedKeyString & key)
{
   return mView->GetNameByKey(key);
}

// Sets the selected command to have this key
// This is not yet a committed change, which will happen on a save.
void KeyConfigPrefs::SetKeyForSelected(const NormalizedKeyString & key)
{
   auto name = mView->GetName(mCommandSelected);

   if (!mView->CanSetKey(mCommandSelected))
   {
      AudacityMessageBox(
         XO("You may not assign a key to this entry"),
         XO("Error"),
         wxICON_ERROR | wxCENTRE,
         this);
      return;
   }

   mView->SetKey(mCommandSelected, key);
   mManager->SetKeyFromName(name, key);
   mNewKeys[ make_iterator_range( mNames ).index( name ) ] = key;
}


void KeyConfigPrefs::OnSet(wxCommandEvent & WXUNUSED(event))
{
   if (mCommandSelected == wxNOT_FOUND) {
      AudacityMessageBox(
         XO("You must select a binding before assigning a shortcut"),
         XO("Error"),
         wxICON_WARNING | wxCENTRE,
         this);
      return;
   }

   CommandID newCommand{ mView->GetName(mCommandSelected) };
   NormalizedKeyString enteredKey{ mKey->GetValue() };
   NormalizedKeyString newComDefaultKey{ 
      mManager->GetDefaultKeyFromName(newCommand) };
   CommandIDs oldCommands;

   // collecting commands competing for the same shortcut
   for (size_t i{ 0 }; i < mNames.size(); i++)
   {
      if (mNewKeys[i] == enteredKey)
      {
         // ignore the Set button if the same shortcut is used
         if (mNames[i] == newCommand)
            return;

         if (newComDefaultKey == EMPTY_SHORTCUT ||
            mDefaultKeys[i] != newComDefaultKey)
         {
            oldCommands.push_back(mNames[i]);
         }
      }
   }

   // Prevent same hotkey combination being used twice.
   if (!oldCommands.empty()) {
      auto newlabel = Verbatim( wxT("'%s - %s'") )
         .Format(
            mManager->GetCategoryFromName(newCommand),
            mManager->GetPrefixedLabelFromName(newCommand) );
      auto oldlabel = Verbatim(wxT("'%s - %s'"))
         .Format(
            mManager->GetCategoryFromName(oldCommands[0]),
            mManager->GetPrefixedLabelFromName(oldCommands[0]));

      for (size_t i{ 1 }; i < oldCommands.size(); i++)
         oldlabel += XO("\n\n\t and\n\n\t") +
         Verbatim(wxT("'%s - %s'")).Format(
            mManager->GetCategoryFromName(oldCommands[i]),
            mManager->GetPrefixedLabelFromName(oldCommands[i]));
      
      if (wxCANCEL == AudacityMessageBox(
            XO(
"The keyboard shortcut '%s' is already assigned to:\n\n\t%s\n\n\nClick OK to assign the shortcut to\n\n\t%s\n\ninstead. Otherwise, click Cancel.")
               .Format(
                  mKey->GetValue(),
                  oldlabel,
                  newlabel
               ),
            XO("Warning"),
            wxOK | wxCANCEL | wxICON_STOP | wxCENTRE,
            this))
      {
         return;
      }
      
      for (const auto & command : oldCommands)
      {
         mView->SetKeyByName(command, {});
         mManager->SetKeyFromName(command, {});
         mNewKeys[make_iterator_range(mNames).index(command)] = {};
      }
   }

   SetKeyForSelected(enteredKey);
}

void KeyConfigPrefs::OnClear(wxCommandEvent& WXUNUSED(event))
{
   mKey->Clear();

   if (mCommandSelected != wxNOT_FOUND) {
      SetKeyForSelected({});
   }
}

void KeyConfigPrefs::OnSelected(wxCommandEvent & WXUNUSED(e))
{
   mCommandSelected = mView->GetSelected();
   mKey->Clear();

   if (mCommandSelected != wxNOT_FOUND) {
      bool canset = mView->CanSetKey(mCommandSelected);
      if (canset) {
         mKey->AppendText(mView->GetKey(mCommandSelected).Display());
      }

      mKey->Enable(canset);
      mSet->Enable(canset);
      mClear->Enable(canset);
   }
}

void KeyConfigPrefs::OnViewBy(wxCommandEvent & e)
{
   switch (e.GetId())
   {
      case ViewByTreeID:
         mViewType = ViewByTree;
         mFilterLabel->SetLabel(_("Searc&h:"));
      break;

      case ViewByNameID:
         mViewType = ViewByName;
         mFilterLabel->SetLabel(_("Searc&h:"));
      break;

      case ViewByKeyID:
         mViewType = ViewByKey;
         mFilterLabel->SetLabel(_("&Hotkey:"));
      break;
   }

   mView->SetView(mViewType);
   mFilter->SetName(wxStripMenuCodes(mFilterLabel->GetLabel()));
}

bool KeyConfigPrefs::Commit()
{
   // On the Mac, preferences may be changed without any active
   // projects.  This means that the CommandManager isn't available
   // either.  So we can't attempt to save preferences, otherwise
   // NULL ptr dereferences will happen in ShuttleGui because the
   // radio buttons are never created.  (See Populate() above.)
   if ( !mProject ) {
      return true;
   }

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   bool bFull = gPrefs->ReadBool(wxT("/GUI/Shortcuts/FullDefaults"), false);
   for (size_t i = 0; i < mNames.size(); i++) {
      const auto &dkey = bFull ? mDefaultKeys[i] : mStandardDefaultKeys[i];
      // using GET to interpret CommandID as a config path component
      auto name = wxT("/NewKeys/") + mNames[i].GET();
      const auto &key = mNewKeys[i];

      if (gPrefs->HasEntry(name)) {
         if (key != NormalizedKeyString{ gPrefs->ReadObject(name, key) } ) {
            gPrefs->Write(name, key);
         }
         if (key == dkey) {
            gPrefs->DeleteEntry(name);
         }
      }
      else {
         if (key != dkey) {
            gPrefs->Write(name, key);
         }
      }
   }

   return gPrefs->Flush();
}

void KeyConfigPrefs::Cancel()
{
   // Restore original key values
   for (size_t i = 0; i < mNames.size(); i++) {
      mManager->SetKeyFromIndex(i, mKeys[i]);
   }

   return;
}

PrefsPanel::Factory
KeyConfigPrefsFactory( const CommandID &name )
{
   return [=](wxWindow *parent, wxWindowID winid, AudacityProject *pProject)
   {
      wxASSERT(parent); // to justify safenew
      auto result = safenew KeyConfigPrefs{ parent, winid, pProject, name };
      return result;
   };
}
namespace{
PrefsPanel::Registration sAttachment{ "KeyConfig",
   KeyConfigPrefsFactory()
};
}
