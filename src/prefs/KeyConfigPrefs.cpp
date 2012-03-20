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
It would be nice to create a new 'Bindings' class which both 
KeyConfigPrefs and MousePrefs use.

*//*********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>
#include <wx/filedlg.h>

#include "../Prefs.h"
#include "../Project.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"
#include "../xml/XMLFileReader.h"

#include "../Internat.h"
#include "../ShuttleGui.h"
#include "KeyConfigPrefs.h"

#include "FileDialog.h"

//
// KeyConfigPrefs
//
#define AssignDefaultsButtonID  17001
#define CurrentComboID          17002
#define SetButtonID             17003
#define ClearButtonID           17004
#define CommandsListID          17005
#define SaveButtonID            17006
#define LoadButtonID            17007
#define CategoryID              17008

// The numbers of the columns of the mList.
enum
{
   CommandColumn,
   KeyComboColumn
};

BEGIN_EVENT_TABLE(KeyConfigPrefs, PrefsPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::OnDefaults)
   EVT_BUTTON(SetButtonID, KeyConfigPrefs::OnSet)
   EVT_BUTTON(ClearButtonID, KeyConfigPrefs::OnClear)
   EVT_BUTTON(SaveButtonID, KeyConfigPrefs::OnSave)
   EVT_BUTTON(LoadButtonID, KeyConfigPrefs::OnLoad)
   EVT_CHOICE(CategoryID, KeyConfigPrefs::OnCategory)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
   EVT_LIST_KEY_DOWN(CommandsListID, KeyConfigPrefs::OnKeyDown)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Keyboard")),
   mKey(NULL)
{
   Populate();
}

KeyConfigPrefs::~KeyConfigPrefs()
{
   if (mKey)
   {
      mKey->Disconnect(wxEVT_KEY_DOWN,
            wxKeyEventHandler(KeyConfigPrefs::OnCaptureKeyDown));
      mKey->Disconnect(wxEVT_CHAR,
            wxKeyEventHandler(KeyConfigPrefs::OnCaptureChar));
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

   mManager = project->GetCommandManager();
   mManager->GetCategories(mCats);
   mCats.Insert(_("All"), 0);

   PopulateOrExchange(S);

   CreateList();
   mCommandSelected = -1;
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
      S.StartHorizontalLay(wxALIGN_CENTRE, false);
      {
         S.Id(CategoryID);
         mCat = S.AddChoice(_("C&ategory:"),
                            mCats[0],
                            &mCats);
      }
      S.EndHorizontalLay();
                                    
      mList = S.Id(CommandsListID).AddListControlReportMode();
      mList->SetName(_("Key Bindings"));

      S.StartThreeColumn();
      {
         if (!mKey) {
            mKey = new wxTextCtrl(this,
                                  CurrentComboID,
                                  wxT(""),
                                  wxDefaultPosition,
#if defined(__WXMAC__)                                  
                                  wxSize(300, -1));
#else
                                  wxSize(210, -1));
#endif
            mKey->Connect(wxEVT_KEY_DOWN,
                          wxKeyEventHandler(KeyConfigPrefs::OnCaptureKeyDown));
            mKey->Connect(wxEVT_CHAR,
                          wxKeyEventHandler(KeyConfigPrefs::OnCaptureChar));
         }
         S.AddWindow(mKey);

         /* i18n-hint: (verb)*/
         S.Id(SetButtonID).AddButton(_("Set"));
         S.Id(ClearButtonID).AddButton(_("Cl&ear"));
      }
      S.EndThreeColumn();

#if defined(__WXMAC__)
      S.AddFixedText(_("Note: Pressing Cmd+Q will quit. All other keys are valid."));
#endif

      S.StartThreeColumn();
      {
         S.Id(LoadButtonID).AddButton(_("&Load..."));
         S.Id(SaveButtonID).AddButton(_("&Save..."));
         S.Id(AssignDefaultsButtonID).AddButton(_("&Defaults"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();
}

/// Sets up mList with the right number of columns, titles,
/// fills the contents and sets column widths.
void KeyConfigPrefs::CreateList()
{
   mList->InsertColumn(CommandColumn, _("Command"), wxLIST_FORMAT_LEFT);
   mList->InsertColumn(KeyComboColumn, _("Key Combination"), wxLIST_FORMAT_LEFT);

   RepopulateBindingsList();

   mList->SetColumnWidth(CommandColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(KeyComboColumn, 250);
}

int wxCALLBACK SortCallback(long item1, long item2, long sortData)
{
   wxArrayString *names = (wxArrayString *) sortData;

   if (names->Item(item1) < names->Item(item2)) {
      return -1;
   }

   if (names->Item(item1) > names->Item(item2)) {
      return 1;
   }

   return 0;
}

void KeyConfigPrefs::RepopulateBindingsList()
{
   wxString cat = mCat->GetStringSelection();

   mList->DeleteAllItems(); // Delete contents, but not the column headers.
   mNames.Clear();
   mManager->GetAllCommandNames(mNames, false);
   bool save = (mKeys.GetCount() == 0);

   size_t ndx = 0;
   int color = 0;
   for (size_t i = 0; i < mNames.GetCount(); i++) {
      wxString name = mNames[i];
      wxString key = KeyStringDisplay(mManager->GetKeyFromName(name));

      // Save the original key value to support canceling
      if (save) {
         mKeys.Add(key);
         // mNewKeys is what mKeys will change to
         mNewKeys.Add(key);
      }

      if (cat != _("All") && mManager->GetCategoryFromName(name) != cat) {
         continue;
      }

      wxString label;

      // Labels for undo and redo change according to the last command
      // which can be undone/redone, so give them a special check in order
      // not to confuse users
      if (name == wxT("Undo")) {
         label = _("Undo");
      }
      else if (name == wxT("Redo")) {
         label = _("Redo");
      }
      else {
         label = mManager->GetPrefixedLabelFromName(name);
      }

      label = wxMenuItem::GetLabelFromText(label.BeforeFirst(wxT('\t')));

      mList->InsertItem(ndx, label);
      mList->SetItem(ndx, KeyComboColumn, key);
      mList->SetItemData(ndx, i);
      mList->SetItemBackgroundColour(ndx, color ? wxColour(240, 240, 240) : *wxWHITE);
      color = 1 - color;

      ndx++;
   }

//   mList->SortItems(SortCallback, (long) &mNames);
}

void KeyConfigPrefs::OnLoad(wxCommandEvent & e)
{
   wxString file = wxT("Audacity-keys.xml");
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),
                                ::wxGetCwd());

   file = FileSelector(_("Select an XML file containing Audacity keyboard shortcuts..."),
                       path,
                       file,
                       wxT(""),
                       _("XML files (*.xml)|*.xml|All files (*.*)|*.*"),
                       wxRESIZE_BORDER,
                       this);

   if (!file) {
      return;
   }

   path = wxPathOnly(file);
   gPrefs->Write(wxT("/DefaultOpenPath"), path);

   XMLFileReader reader;
   if (!reader.Parse(mManager, file)) {
      wxMessageBox(reader.GetErrorStr(),
                   _("Error Loading Keyboard Shortcuts"),
                   wxOK | wxCENTRE, this);
   }

   RepopulateBindingsList();
}

void KeyConfigPrefs::OnSave(wxCommandEvent & e)
{
   wxString file = wxT("Audacity-keys.xml");
   wxString path = gPrefs->Read(wxT("/DefaultExportPath"),
                                ::wxGetCwd());

   file = FileSelector(_("Export Keyboard Shortcuts As:"),
                       path,
                       file,
                       wxT("xml"),
                       _("XML files (*.xml)|*.xml|All files (*.*)|*.*"),
                       wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                       this);

   if (!file) {
      return;
   }

   path = wxPathOnly(file);
   gPrefs->Write(wxT("/DefaultExportPath"), path);

   XMLFileWriter prefFile;
   
   try
   {
      prefFile.Open(file, wxT("wb"));
      mManager->WriteXML(prefFile);
      prefFile.Close();
   }
   catch (XMLFileWriterException* pException)
   {
      wxMessageBox(_("Couldn't write to file: ") + file,
                   _("Error Saving Keyboard Shortcuts"),
                   wxOK | wxCENTRE, this);

      delete pException;
   }
}

void KeyConfigPrefs::OnDefaults(wxCommandEvent & e)
{
   for (size_t i = 0; i < mNames.GetCount(); i++) {
      mManager->SetKeyFromName(mNames[i],
                               mManager->GetDefaultKeyFromName(mNames[i]));
   }
   RepopulateBindingsList();
}

void KeyConfigPrefs::OnCaptureKeyDown(wxKeyEvent & e)
{
   wxTextCtrl *t = (wxTextCtrl *)e.GetEventObject();

#if defined(__WXMAC__)
   if (e.GetKeyCode() == WXK_TAB) {
      wxNavigationKeyEvent nevent;
      nevent.SetWindowChange(e.ControlDown());
      nevent.SetDirection(!e.ShiftDown());
      nevent.SetEventObject(t);
      nevent.SetCurrentFocus(t);
      t->GetParent()->ProcessEvent(nevent);

      return;
   }
#endif

   t->SetValue(KeyStringDisplay(KeyEventToKeyString(e)));
}

void KeyConfigPrefs::OnCaptureChar(wxKeyEvent & e)
{
}

// Given a hotkey combination, returns the name (description) of the 
// corresponding command, or the empty string if none is found.
wxString KeyConfigPrefs::NameFromKey( const wxString & key )
{
   int i;
   i=mNewKeys.Index( key );
   if( i== wxNOT_FOUND )
      return wxT("");
   return mNames[i];
}

// Sets the selected command to have this key
// This is not yet a committed change, which will happen on a save.
void KeyConfigPrefs::SetKeyForSelected( const wxString & key )
{
   wxString name = mNames[mList->GetItemData(mCommandSelected)];

   mList->SetItem(mCommandSelected, KeyComboColumn, key);
   mManager->SetKeyFromName(name, key);

   int i=mNames.Index( name );
   if( i!=wxNOT_FOUND ) 
      mNewKeys[i]=key;
}


void KeyConfigPrefs::OnSet(wxCommandEvent & e)
{
   if ( mCommandSelected >= (int)mNames.GetCount())
      return;

   wxString newKey = mKey->GetValue();
   wxString alreadyAssignedName = NameFromKey( newKey );

   // Prevent same hotkey combination being used twice.
   if( !alreadyAssignedName.IsEmpty() ) {
      wxMessageBox(
         wxString::Format(
            _("The keyboard shortcut '%s' is already assigned to:\n\n'%s'"),
            newKey.c_str(),
            alreadyAssignedName.c_str()),
         _("Error"), wxICON_STOP | wxCENTRE, this);
      return;
   }

   SetKeyForSelected( newKey );
}

void KeyConfigPrefs::OnClear(wxCommandEvent& event)
{
   mKey->Clear();

   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount()) {
      return;
   }
   SetKeyForSelected( wxT("") );
}

void KeyConfigPrefs::OnKeyDown(wxListEvent & e)
{
// the code in this function allows the user to seek to the next
// command which begins with the letter that is pressed
#ifdef __WXMAC__
   // I (Ed) have no way of telling what code will work on
   // the Mac but the following code does not
   return;
#endif

#ifdef __WXMSW__
   // Windows seems to have this built-in
   // and does not need the following code
   return;
#endif

   // The following code seems to work well on at least some versions of Linux
   int keycode = e.GetKeyCode();
   int selected = mList->GetNextItem(-1, wxLIST_NEXT_ALL,  wxLIST_STATE_SELECTED);
   int cnt = mList->GetItemCount();
   wxListItem item;
   bool found = false;

   item.SetColumn(CommandColumn);
   item.SetMask(wxLIST_MASK_TEXT);

   for (int i = selected + 1; i < cnt; i++)
   {
      item.SetId(i);

      mList->GetItem(item);

      if (item.GetText().Left(1).IsSameAs(keycode, false)) {
         mList->SetItemState(e.GetIndex(),
                             0,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

         mList->SetItemState(i,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

         mList->EnsureVisible(i);

         found = true;

         break;
      }
   }

   if (!found) {
      for (int i = 0; i < selected; i++)
      {
         item.SetId(i);

         mList->GetItem(item);

         if (item.GetText().Left(1).IsSameAs(keycode, false)) {
            mList->SetItemState(e.GetIndex(),
                                0,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

            mList->SetItemState(i,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

            mList->EnsureVisible(i);
            break;
         }
      }
   }
}

void KeyConfigPrefs::OnCategory(wxCommandEvent & e)
{
   RepopulateBindingsList();
}

void KeyConfigPrefs::OnItemSelected(wxListEvent & e)
{
   mCommandSelected = e.GetIndex();

   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount()) {
      mKey->SetLabel(wxT(""));
      return;
   }

   wxListItem item;
   item.SetColumn(KeyComboColumn);
   item.SetMask(wxLIST_MASK_TEXT);
   item.SetId(mCommandSelected);
   mList->GetItem(item);

   mKey->Clear();
   mKey->AppendText(item.GetText());
}

bool KeyConfigPrefs::Apply()
{
   for (size_t i = 0; i < mNames.GetCount(); i++) {
      wxString dkey = KeyStringNormalize(mManager->GetDefaultKeyFromName(mNames[i]));
      wxString name = wxT("/NewKeys/") + mNames[i];
      wxString key = KeyStringNormalize(mManager->GetKeyFromName(mNames[i]));

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

   return true;
}

void KeyConfigPrefs::Cancel()
{
   // Restore original key values
   for (size_t i = 0; i < mNames.GetCount(); i++) {
      mManager->SetKeyFromName(mNames[i], mKeys[i]);
   }

   return;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: f09afeeb-9805-463a-b3ca-e3e3bfe05549

