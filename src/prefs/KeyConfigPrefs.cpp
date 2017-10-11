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

#include "../Audacity.h"
#include "../Experimental.h"
#include "KeyConfigPrefs.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>
#include <wx/filedlg.h>
#include <wx/button.h>
#include <wx/msgdlg.h>

#include "../Prefs.h"
#include "../Project.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"
#include "../xml/XMLFileReader.h"

#include "../Internat.h"
#include "../ShuttleGui.h"

#include "../FileNames.h"

#include "../widgets/KeyView.h"

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

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Keyboard")),
   mView(NULL),
   mKey(NULL),
   mFilter(NULL),
   mFilterTimer(this, FilterTimerID),
   mFilterPending(false)
{
   Populate();
}

KeyConfigPrefs::~KeyConfigPrefs()
{
   if (mKey)
   {
      mKey->Disconnect(wxEVT_KEY_DOWN,
            wxKeyEventHandler(KeyConfigPrefs::OnHotkeyKeyDown),
            NULL,
            this);
      mKey->Disconnect(wxEVT_CHAR,
            wxKeyEventHandler(KeyConfigPrefs::OnHotkeyChar),
            NULL,
            this);
      mKey->Disconnect(wxEVT_KILL_FOCUS,
            wxFocusEventHandler(KeyConfigPrefs::OnHotkeyKillFocus),
            NULL,
            this);
   }

   if (mFilter)
   {
      mKey->Disconnect(wxEVT_KEY_DOWN,
            wxKeyEventHandler(KeyConfigPrefs::OnFilterKeyDown),
            NULL,
            this);
      mKey->Disconnect(wxEVT_CHAR,
            wxKeyEventHandler(KeyConfigPrefs::OnFilterChar),
            NULL,
            this);
   }
}

void KeyConfigPrefs::Populate()
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   AudacityProject *project = GetActiveProject();

   if (!project) {
      S.StartVerticalLay(true);
      {
         S.StartStatic(wxEmptyString, true);
         {
            S.AddTitle(_("Keyboard preferences currently unavailable."));
            S.AddTitle(_("Open a new project to modify keyboard shortcuts."));
         }
         S.EndStatic();
      }
      S.EndVerticalLay();

      return;
   }

   PopulateOrExchange(S);

   mCommandSelected = wxNOT_FOUND;

   mManager = project->GetCommandManager();

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
   S.SetBorder(2);

   S.StartStatic(_("Key Bindings"), 1);
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);

         S.StartHorizontalLay(wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 0);
         {
            S.AddTitle(_("View by:"));
            S.StartRadioButtonGroup(wxT("/Prefs/KeyConfig/ViewBy"), wxT("tree"));
            {
               mViewByTree = S.Id(ViewByTreeID).TieRadioButton(_("&Tree"), wxT("tree"));
               mViewByTree->SetName(_("View by tree"));
               mViewByName = S.Id(ViewByNameID).TieRadioButton(_("&Name"), wxT("name"));
               mViewByName->SetName(_("View by name"));
               mViewByKey = S.Id(ViewByKeyID).TieRadioButton(_("&Key"), wxT("key"));
               mViewByKey->SetName(_("View by key"));
            }
            S.EndRadioButtonGroup();
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL, 0);
         {
            // just a spacer
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 0);
         {
            mFilterLabel = S.AddVariableText(_("Searc&h:"));

            if (!mFilter) {
               mFilter = safenew wxTextCtrl(this,
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
               mFilter->Connect(wxEVT_KEY_DOWN,
                                wxKeyEventHandler(KeyConfigPrefs::OnFilterKeyDown),
                                NULL,
                                this);
               mFilter->Connect(wxEVT_CHAR,
                                wxKeyEventHandler(KeyConfigPrefs::OnFilterChar),
                                NULL,
                                this);
            }
            S.AddWindow(mFilter, wxALIGN_NOT | wxALIGN_LEFT);
         }
         S.EndHorizontalLay();
      }
      S.EndThreeColumn();
      S.AddSpace(-1, 2);

      S.StartHorizontalLay(wxEXPAND, 1);
      {
         if (!mView) {
            mView = safenew KeyView(this, CommandsListID);
            mView->SetName(_("Bindings"));
         }
         S.Prop(true);
         S.AddWindow(mView, wxEXPAND);
      }
      S.EndHorizontalLay();

      S.StartThreeColumn();
      {
         if (!mKey) {
            mKey = safenew wxTextCtrl(this,
                                  CurrentComboID,
                                  wxT(""),
                                  wxDefaultPosition,
#if defined(__WXMAC__)
                                  wxSize(300, -1),
#else
                                  wxSize(210, -1),
#endif
                                  wxTE_PROCESS_ENTER);

            mKey->SetName(_("Short cut"));
            mKey->Connect(wxEVT_KEY_DOWN,
                          wxKeyEventHandler(KeyConfigPrefs::OnHotkeyKeyDown),
                          NULL,
                          this);
            mKey->Connect(wxEVT_CHAR,
                          wxKeyEventHandler(KeyConfigPrefs::OnHotkeyChar),
                          NULL,
                          this);
            mKey->Connect(wxEVT_KILL_FOCUS,
                          wxFocusEventHandler(KeyConfigPrefs::OnHotkeyKillFocus),
                          NULL,
                          this);
         }
         S.AddWindow(mKey);

         /* i18n-hint: (verb)*/
         mSet = S.Id(SetButtonID).AddButton(_("&Set"));
         mClear = S.Id(ClearButtonID).AddButton(_("Cl&ear"));
      }
      S.EndThreeColumn();

#if defined(__WXMAC__)
      S.AddFixedText(_("Note: Pressing Cmd+Q will quit. All other keys are valid."));
#endif

      S.StartThreeColumn();
      {
         S.Id(ImportButtonID).AddButton(_("&Import..."));
         S.Id(ExportButtonID).AddButton(_("&Export..."));
         S.Id(AssignDefaultsButtonID).AddButton(_("&Defaults"));
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
   wxArrayString Labels;
   wxArrayString Categories;
   wxArrayString Prefixes;

   mNames.Clear();
   mKeys.Clear();
   mDefaultKeys.Clear();
   mStandardDefaultKeys.Clear();
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

void KeyConfigPrefs::OnImport(wxCommandEvent & WXUNUSED(event))
{
   wxString file = wxT("Audacity-keys.xml");

   file = FileNames::SelectFile(FileNames::Operation::Open,
                        _("Select an XML file containing Audacity keyboard shortcuts..."),
                       wxEmptyString,
                       file,
                       wxT(""),
                       _("XML files (*.xml)|*.xml|All files|*"),
                       wxRESIZE_BORDER,
                       this);

   if (!file) {
      return;
   }

   XMLFileReader reader;
   if (!reader.Parse(mManager, file)) {
      wxMessageBox(reader.GetErrorStr(),
                   _("Error Importing Keyboard Shortcuts"),
                   wxOK | wxCENTRE, this);
   }

   RefreshBindings(true);
}

void KeyConfigPrefs::OnExport(wxCommandEvent & WXUNUSED(event))
{
   wxString file = wxT("Audacity-keys.xml");

   file = FileNames::SelectFile(FileNames::Operation::Export,
                       _("Export Keyboard Shortcuts As:"),
                       wxEmptyString,
                       file,
                       wxT("xml"),
                       _("XML files (*.xml)|*.xml|All files|*"),
                       wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                       this);

   if (!file) {
      return;
   }

   GuardedCall< void >( [&] {
      XMLFileWriter prefFile{ file, _("Error Exporting Keyboard Shortcuts") };
      mManager->WriteXML(prefFile);
      prefFile.Commit();
   } );
}



// There currently is only one clickable AButton
// so we just do what it needs.
void KeyConfigPrefs::OnDefaults(wxCommandEvent & event)
{
   wxMenu Menu;
   Menu.Append( 1, _("Standard") );
   Menu.Append( 2, _("Full") );
   Menu.Bind( wxEVT_COMMAND_MENU_SELECTED, &KeyConfigPrefs::OnImportDefaults, this );
   // Pop it up where the mouse is.
   PopupMenu(&Menu);//, wxPoint(0, 0));
}

void KeyConfigPrefs::FilterKeys( wxArrayString & arr )
{
   wxSortedArrayString MaxListOnly;

   // These short cuts are for the max list only....
   //MaxListOnly.Add( "Ctrl+I" );
   MaxListOnly.Add( "Ctrl+Alt+I" );
   MaxListOnly.Add( "Ctrl+J" );
   MaxListOnly.Add( "Ctrl+Alt+J" );
   MaxListOnly.Add( "Ctrl+Alt+V" );
   MaxListOnly.Add( "Alt+X" );
   MaxListOnly.Add( "Alt+K" );
   MaxListOnly.Add( "Shift+Alt+X" );
   MaxListOnly.Add( "Shift+Alt+K" );
   MaxListOnly.Add( "Alt+L" );
   MaxListOnly.Add( "Shift+Alt+C" );
   MaxListOnly.Add( "Alt+I" );
   MaxListOnly.Add( "Alt+J" );
   MaxListOnly.Add( "Shift+Alt+J" );
   MaxListOnly.Add( "Ctrl+Shift+A" );
   MaxListOnly.Add( "Q" );
   //MaxListOnly.Add( "Shift+J" );
   //MaxListOnly.Add( "Shift+K" );
   //MaxListOnly.Add( "Shift+Home" );
   //MaxListOnly.Add( "Shift+End" );
   MaxListOnly.Add( "Ctrl+[" );
   MaxListOnly.Add( "Ctrl+]" );
   MaxListOnly.Add( "1" );
   MaxListOnly.Add( "Shift+F5" );
   MaxListOnly.Add( "Shift+F6" );
   MaxListOnly.Add( "Shift+F7" );
   MaxListOnly.Add( "Shift+F8" );
   MaxListOnly.Add( "Ctrl+Shift+F5" );
   MaxListOnly.Add( "Ctrl+Shift+F7" );
   MaxListOnly.Add( "Ctrl+Shift+N" );
   MaxListOnly.Add( "Ctrl+Shift+M" );
   MaxListOnly.Add( "Ctrl+Home" );
   MaxListOnly.Add( "Ctrl+End" );
   MaxListOnly.Add( "Shift+C" );
   MaxListOnly.Add( "Alt+Shift+Up" );
   MaxListOnly.Add( "Alt+Shift+Down" );
   MaxListOnly.Add( "Shift+P" );
   MaxListOnly.Add( "Alt+Shift+Left" );
   MaxListOnly.Add( "Alt+Shift+Right" );
   MaxListOnly.Add( "Ctrl+Shift+T" );
   //MaxListOnly.Add( "Command+M" );
   //MaxListOnly.Add( "Option+Command+M" );
   MaxListOnly.Add( "Shift+H" );
   MaxListOnly.Add( "Shift+O" );
   MaxListOnly.Add( "Shift+I" );
   MaxListOnly.Add( "Shift+N" );
   MaxListOnly.Add( "D" );
   MaxListOnly.Add( "A" );
   MaxListOnly.Add( "Alt+Shift+F6" );
   MaxListOnly.Add( "Alt+F6" );

   MaxListOnly.Sort();
   // Remove items that are in MaxList.
   for (size_t i = 0; i < arr.GetCount(); i++) {
      if( MaxListOnly.Index( arr[i] ) != wxNOT_FOUND )
         arr[i]= wxT("");
   }
}

void KeyConfigPrefs::OnImportDefaults(wxCommandEvent & event)
{
   gPrefs->DeleteEntry(wxT("/GUI/Shortcuts/FullDefaults"));
   gPrefs->Flush();

   mNewKeys = mDefaultKeys;
   if( event.GetId() == 1 )
      FilterKeys( mNewKeys );

   for (size_t i = 0; i < mNewKeys.GetCount(); i++) {
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
      NavigateIn(e.ShiftDown()
                 ? wxNavigationKeyEvent::IsBackward
                 : wxNavigationKeyEvent::IsForward);
      return;
   }

   t->SetValue(KeyStringDisplay(KeyEventToKeyString(e)));
}

void KeyConfigPrefs::OnHotkeyChar(wxKeyEvent & WXUNUSED(e))
{
   // event.Skip() not performed, so event will not be processed further.
}

void KeyConfigPrefs::OnHotkeyKillFocus(wxFocusEvent & e)
{
   if (mKey->GetValue().IsEmpty() && mCommandSelected != wxNOT_FOUND) {
      mKey->AppendText(mView->GetKey(mCommandSelected));
   }

   e.Skip();
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
      wxString key = KeyStringDisplay(KeyEventToKeyString(e));
      t->SetValue(key);

      if (key != wxEmptyString) {
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

void KeyConfigPrefs::OnFilterChar(wxKeyEvent & e)
{
   if (mViewType != ViewByKey)
   {
      e.Skip();
   }
}

// Given a hotkey combination, returns the name (description) of the
// corresponding command, or the empty string if none is found.
wxString KeyConfigPrefs::NameFromKey(const wxString & key)
{
   return mView->GetNameByKey(key);
}

// Sets the selected command to have this key
// This is not yet a committed change, which will happen on a save.
void KeyConfigPrefs::SetKeyForSelected(const wxString & key)
{
   wxString name = mView->GetName(mCommandSelected);

   if (!mView->CanSetKey(mCommandSelected))
   {
      wxMessageBox(_("You may not assign a key to this entry"),
         _("Error"), wxICON_ERROR | wxCENTRE, this);
      return;
   }

   mView->SetKey(mCommandSelected, key);
   mManager->SetKeyFromName(name, key);
   mNewKeys[mNames.Index(name)] = key;
}


void KeyConfigPrefs::OnSet(wxCommandEvent & WXUNUSED(event))
{
   if (mCommandSelected == wxNOT_FOUND) {
      wxMessageBox(_("You must select a binding before assigning a shortcut"),
         _("Error"), wxICON_WARNING | wxCENTRE, this);
      return;
   }

   wxString key = mKey->GetValue();
   wxString oldname = mView->GetNameByKey(key);
   wxString newname = mView->GetName(mCommandSelected);

   // Just ignore it if they are the same
   if (oldname == newname) {
      return;
   }

   // Prevent same hotkey combination being used twice.
   if (!oldname.IsEmpty()) {
      wxString oldlabel = mManager->GetCategoryFromName(oldname) + wxT(" - ") +
                          mManager->GetPrefixedLabelFromName(oldname);
      wxString newlabel = mManager->GetCategoryFromName(newname) + wxT(" - ") +
                          mManager->GetPrefixedLabelFromName(newname);
      if (wxMessageBox(
            wxString::Format(
            _("The keyboard shortcut '%s' is already assigned to:\n\n\t'%s'\n\nClick OK to assign the shortcut to\n\n\t'%s'\n\ninstead.  Otherwise, click Cancel."),
            key.c_str(),
            oldlabel.c_str(),
            newlabel.c_str()),
            _("Error"), wxOK | wxCANCEL | wxICON_STOP | wxCENTRE, this) == wxCANCEL)
      {
         return;
      }

      mView->SetKeyByName(oldname, wxEmptyString);
      mManager->SetKeyFromName(oldname, wxEmptyString);
      mNewKeys[mNames.Index(oldname)].Empty();

   }

   SetKeyForSelected(key);
}

void KeyConfigPrefs::OnClear(wxCommandEvent& WXUNUSED(event))
{
   mKey->Clear();

   if (mCommandSelected != wxNOT_FOUND) {
      SetKeyForSelected(wxEmptyString);
   }
}

void KeyConfigPrefs::OnSelected(wxCommandEvent & WXUNUSED(e))
{
   mCommandSelected = mView->GetSelected();
   mKey->Clear();

   if (mCommandSelected != wxNOT_FOUND) {
      bool canset = mView->CanSetKey(mCommandSelected);
      if (canset) {
         mKey->AppendText(mView->GetKey(mCommandSelected));
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
   // projects.  This means that the CommandManager isn't availabe
   // either.  So we can't attempt to save preferences, otherwise
   // NULL ptr dereferences will happen in ShuttleGui because the
   // radio buttons are never created.  (See Populate() above.)
   if (!GetActiveProject()) {
      return true;
   }

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   for (size_t i = 0; i < mNames.GetCount(); i++) {
      wxString dkey = KeyStringNormalize(mStandardDefaultKeys[i]);
      wxString name = wxT("/NewKeys/") + mNames[i];
      wxString key = KeyStringNormalize(mNewKeys[i]);

      if (gPrefs->HasEntry(name)) {
         if (key != KeyStringNormalize(gPrefs->Read(name, key))) {
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
   for (size_t i = 0; i < mNames.GetCount(); i++) {
      mManager->SetKeyFromIndex(i, mKeys[i]);
   }

   return;
}

wxString KeyConfigPrefs::HelpPageName()
{
   return "Keyboard_Preferences";
}

PrefsPanel *KeyConfigPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew KeyConfigPrefs(parent);
}
