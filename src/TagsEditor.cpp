/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TagsEditor.cpp
 
 Paul Licameli split from Tags.cpp
 
 **********************************************************************/

#include "TagsEditor.h"

#include "SelectFile.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/Grid.h"
#include "widgets/HelpSystem.h"
#include "XMLFileReader.h"
#include <wx/combobox.h>
#include <wx/display.h>
#include <wx/scrolbar.h>

bool TagsEditorDialog::ShowEditDialog(Tags &tags, wxWindow *parent, const TranslatableString &title, bool force)
{
   if (force) {
      TagsEditorDialog dlg(parent, title, &tags, true, true);

      return dlg.ShowModal() == wxID_OK;
   }

   return true;
}

//
// ComboEditor - Wrapper to prevent unwanted background erasure
//

class ComboEditor final : public wxGridCellChoiceEditor
{
public:
   ComboEditor(const wxArrayString& choices, bool allowOthers = false)
   :  wxGridCellChoiceEditor(choices, allowOthers)
   ,  m_choices{ choices }
   ,  m_allowOthers{ allowOthers }
   {
   }

   void PaintBackground(wxDC&, const wxRect& WXUNUSED(rectCell), const wxGridCellAttr & WXUNUSED(attr)) override
   {
      // Ignore it (a must on the Mac as the erasure causes problems.)
   }

   void SetParameters(const wxString& params) override
   {
      wxGridCellChoiceEditor::SetParameters(params);

      // Refresh the wxComboBox with NEW values
      if (Combo()) {
         Combo()->Clear();
         Combo()->Append(m_choices);
      }
   }

   void SetSize(const wxRect& rectOrig) override
   {
      wxRect rect(rectOrig);
      wxRect r = Combo()->GetRect();

      // Center the combo box in or over the cell
      rect.y -= (r.GetHeight() - rect.GetHeight()) / 2;
      rect.height = r.GetHeight();

      wxGridCellChoiceEditor::SetSize(rect);
   }

   // Fix for Bug 1389
   // July 2016: ANSWER-ME: Does this need reporting upstream to wxWidgets?
   virtual void StartingKey(wxKeyEvent& event) override
   {
       // Lifted from wxGridCellTextEditor and adapted to combo.

       // [Below is comment from wxWidgets code]
       // Since this is now happening in the EVT_CHAR event EmulateKeyPress is no
       // longer an appropriate way to get the character into the text control.
       // Do it ourselves instead.  We know that if we get this far that we have
       // a valid character, so not a whole lot of testing needs to be done.

       //The only difference to wxGridCellTextEditor.
       //wxTextCtrl* tc = (wxTextCtrl *)m_control;
       wxComboBox * tc = Combo();
       int ch;

       bool isPrintable;

   #if wxUSE_UNICODE
       ch = event.GetUnicodeKey();
       if ( ch != WXK_NONE )
           isPrintable = true;
       else
   #endif // wxUSE_UNICODE
       {
           ch = event.GetKeyCode();
           isPrintable = ch >= WXK_SPACE && ch < WXK_START;
       }

       switch (ch)
       {
           case WXK_DELETE:
               // Delete the initial character when starting to edit with DELETE.
               tc->Remove(0, 1);
               break;

           case WXK_BACK:
               // Delete the last character when starting to edit with BACKSPACE.
               {
                   const long pos = tc->GetLastPosition();
                   tc->Remove(pos - 1, pos);
               }
               break;

           default:
               if ( isPrintable )
                   tc->WriteText(static_cast<wxChar>(ch));
               break;
       }
   }

   // Clone is required by wxwidgets; implemented via copy constructor
   wxGridCellEditor *Clone() const override
   {
      return safenew ComboEditor{ m_choices, m_allowOthers };
   }

private:
   wxArrayString   m_choices;
   bool            m_allowOthers;
};

//
// Editor
//

#define LABEL_ARTIST    XO("Artist Name")
#define LABEL_TITLE     XO("Track Title")
#define LABEL_ALBUM     XO("Album Title")
#define LABEL_TRACK     XO("Track Number")
#define LABEL_YEAR      XO("Year")
#define LABEL_GENRE     XO("Genre")
#define LABEL_COMMENTS  XO("Comments")

static TranslatableStrings names{
   LABEL_ARTIST,
   LABEL_TITLE,
   LABEL_ALBUM,
   LABEL_TRACK,
   LABEL_YEAR,
   LABEL_GENRE,
   LABEL_COMMENTS
};

static const struct
{
   TranslatableString label;
   wxString name;
}
labelmap[] =
{
   {  LABEL_ARTIST,     TAG_ARTIST     },
   {  LABEL_TITLE,      TAG_TITLE      },
   {  LABEL_ALBUM,      TAG_ALBUM      },
   {  LABEL_TRACK,      TAG_TRACK      },
   {  LABEL_YEAR,       TAG_YEAR       },
   {  LABEL_GENRE,      TAG_GENRE      },
   {  LABEL_COMMENTS,   TAG_COMMENTS   }
};

#define STATICCNT WXSIZEOF(labelmap)

enum {
   ClearID = 10000,
   EditID,
   ResetID,
   LoadID,
   SaveID,
   SaveDefaultsID,
   AddID,
   RemoveID,
   DontShowID
};

BEGIN_EVENT_TABLE(TagsEditorDialog, wxDialogWrapper)
   EVT_GRID_CELL_CHANGED(TagsEditorDialog::OnChange)
   EVT_BUTTON(EditID, TagsEditorDialog::OnEdit)
   EVT_BUTTON(ResetID, TagsEditorDialog::OnReset)
   EVT_BUTTON(ClearID, TagsEditorDialog::OnClear)
   EVT_BUTTON(LoadID, TagsEditorDialog::OnLoad)
   EVT_BUTTON(SaveID, TagsEditorDialog::OnSave)
   EVT_BUTTON(SaveDefaultsID, TagsEditorDialog::OnSaveDefaults)
   EVT_BUTTON(AddID, TagsEditorDialog::OnAdd)
   EVT_BUTTON(RemoveID, TagsEditorDialog::OnRemove)
   EVT_BUTTON(wxID_HELP, TagsEditorDialog::OnHelp)
   EVT_BUTTON(wxID_CANCEL, TagsEditorDialog::OnCancel)
   EVT_BUTTON(wxID_OK, TagsEditorDialog::OnOk)
   EVT_CHECKBOX( DontShowID, TagsEditorDialog::OnDontShow )
   EVT_KEY_DOWN(TagsEditorDialog::OnKeyDown)
END_EVENT_TABLE()

TagsEditorDialog::TagsEditorDialog(wxWindow * parent,
                       const TranslatableString &title,
                       Tags * tags,
                       bool editTitle,
                       bool editTrack)
:  wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mTags(tags),
   mEditTitle(editTitle),
   mEditTrack(editTrack)
{
   SetName();

   mGrid = NULL;

   // Make a local copy of the passed in tags
   mLocal = *mTags;

   // Build, size, and position the dialog
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   TransferDataToWindow();

   Layout();
   Fit();
   Center();
   wxSize sz = GetSize();
   SetSizeHints(sz.x, std::min(sz.y, 600));

   // Restore the original tags because TransferDataToWindow() will be called again
   mLocal.Clear();
   mLocal = *mTags;

   // Override size and position with last saved
   wxRect r = GetRect();
   gPrefs->Read(wxT("/TagsEditorDialog/x"), &r.x, r.x);
   gPrefs->Read(wxT("/TagsEditorDialog/y"), &r.y, r.y);
   gPrefs->Read(wxT("/TagsEditorDialog/width"), &r.width, r.width);
   gPrefs->Read(wxT("/TagsEditorDialog/height"), &r.height, r.height);
   //On multi-monitor systems, there's a chance the last saved window position is
   //on a monitor that has been removed or is unavailable.
   if (IsWindowRectValid(&r))
      Move(r.GetPosition());

   SetSize(r.GetSize());
   Layout();

   // Resize value column width based on width of columns and the vertical scrollbar
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
   r = mGrid->GetClientRect();
   r.width -= mGrid->GetColSize(0);
   r.width -= sb.GetSize().GetWidth();
   r.width -= 10;
   r.width -= r.x;
   mGrid->SetColSize(1, r.width);
   //Bug 2038
   mGrid->SetFocus();

   // Load the genres
   PopulateGenres();
}

TagsEditorDialog::~TagsEditorDialog()
{
   // This DELETE is not needed because wxWidgets owns the grid.
// DELETE mGrid;

// TODO:  Need to figure out if these should be deleted.  Looks like the wxGrid
//        code takes ownership and uses reference counting, but there's been
//        cases where they show up as memory leaks.
//  PRL: Fixed the leaks, see commit c87eb0804bc5f40659b133cab6e2ade061959645
//   DELETE mStringRenderer;
//   DELETE mComboEditor;
}

void TagsEditorDialog::PopulateOrExchange(ShuttleGui & S)
{
   bool bShow;
   gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &bShow, true );

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxALIGN_LEFT, 0);
      {
         S.AddUnits(XO("Use arrow keys (or ENTER key after editing) to navigate fields."));
      }
      S.EndHorizontalLay();

      if (mGrid == NULL) {
         mGrid = safenew Grid(S.GetParent(),
                          wxID_ANY,
                          wxDefaultPosition,
                          wxDefaultSize,
                          wxSUNKEN_BORDER);

         mGrid->RegisterDataType(wxT("Combo"),
            (mStringRenderer = safenew wxGridCellStringRenderer),
            (mComboEditor = safenew ComboEditor(wxArrayString(), true)));

         mGrid->SetColLabelSize(mGrid->GetDefaultRowSize());

         auto  cs = transform_container<wxArrayStringEx>(
            names, std::mem_fn( &TranslatableString::Translation ) );

         // Build the initial (empty) grid
         mGrid->CreateGrid(0, 2, wxGrid::wxGridSelectRows);
         mGrid->SetRowLabelSize(0);
         mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
         mGrid->SetColLabelValue(0, _("Tag"));
         mGrid->SetColLabelValue(1, _("Value"));

         // Resize the name column and set default row height.
         wxComboBox tc(this, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, cs);
         mGrid->SetColSize(0, tc.GetSize().x);
         mGrid->SetColMinimalWidth(0, tc.GetSize().x);
      }
      S.Prop(1)
         .Position(wxEXPAND | wxALL)
         .AddWindow(mGrid);

      S.StartMultiColumn(4, wxALIGN_CENTER);
      {
         S.Id(AddID).AddButton(XXO("&Add"));
         S.Id(RemoveID).AddButton(XXO("&Remove"));
         S.AddTitle( {} );
         S.Id(ClearID).AddButton(XXO("Cl&ear"));
      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxALIGN_CENTRE, 0);
      {
         S.StartStatic(XO("Genres"));
         {
            S.StartMultiColumn(4, wxALIGN_CENTER);
            {
               S.Id(EditID).AddButton(XXO("E&dit..."));
               S.Id(ResetID).AddButton(XXO("Rese&t..."));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
         S.StartStatic(XO("Template"));
         {
            S.StartMultiColumn(4, wxALIGN_CENTER);
            {
               S.Id(LoadID).AddButton(XXO("&Load..."));
               S.Id(SaveID).AddButton(XXO("&Save..."));
               S.AddTitle( {} );
               S.Id(SaveDefaultsID).AddButton(XXO("Set De&fault"));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndHorizontalLay();
      S.StartHorizontalLay(wxALIGN_LEFT, 0);
      {
         S.Id( DontShowID ).AddCheckBox( XXO("Don't show this when exporting audio"), !bShow );
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton | eHelpButton);
}

void TagsEditorDialog::OnDontShow( wxCommandEvent & Evt )
{
   bool bShow = !Evt.IsChecked();
   gPrefs->Write(wxT("/AudioFiles/ShowId3Dialog"), bShow );
   gPrefs->Flush();
}

void TagsEditorDialog::OnHelp(wxCommandEvent& WXUNUSED(event))
{
   HelpSystem::ShowHelp(this, L"Metadata_Editor", true);
}

bool TagsEditorDialog::TransferDataFromWindow()
{
   int i, cnt = mGrid->GetNumberRows();

   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
   }

   mLocal.Clear();
   for (i = 0; i < cnt; i++) {
      // Get tag name from the grid

      auto n = mGrid->GetCellValue(i, 0);
      wxString v = mGrid->GetCellValue(i, 1);

      if (n.empty()) {
         continue;
      }

      bool bSpecialTag = true;

      // Map special tag names back to internal keys
      if (n.CmpNoCase(LABEL_ARTIST.Translation()) == 0) {
         n = TAG_ARTIST;
      }
      else if (n.CmpNoCase(LABEL_TITLE.Translation()) == 0) {
         n = TAG_TITLE;
      }
      else if (n.CmpNoCase(LABEL_ALBUM.Translation()) == 0) {
         n = TAG_ALBUM;
      }
      else if (n.CmpNoCase(LABEL_TRACK.Translation()) == 0) {
         n = TAG_TRACK;
      }
      else if (n.CmpNoCase(LABEL_YEAR.Translation()) == 0) {
         n = TAG_YEAR;
      }
      else if (n.CmpNoCase(LABEL_GENRE.Translation()) == 0) {
         n = TAG_GENRE;
      }
      else if (n.CmpNoCase(LABEL_COMMENTS.Translation()) == 0) {
         n = TAG_COMMENTS;
      }
      else {
         bSpecialTag = false;
      }

      mLocal.SetTag(n, v, bSpecialTag);
   }

   return true;
}

bool TagsEditorDialog::TransferDataToWindow()
{
   size_t i;
   TagMap popTagMap;

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows
   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   // Populate the static rows
   for (i = 0; i < STATICCNT; i++) {
      mGrid->AppendRows();

      mGrid->SetReadOnly(i, 0);
      // The special tag name that's displayed and translated may not match
      // the key string used for internal lookup.
      mGrid->SetCellValue(i, 0, labelmap[i].label.Translation() );
      mGrid->SetCellValue(i, 1, mLocal.GetTag(labelmap[i].name));

      if (!mEditTitle &&
          mGrid->GetCellValue(i, 0).CmpNoCase(LABEL_TITLE.Translation()) == 0) {
         mGrid->SetReadOnly(i, 1);
      }

      if (!mEditTrack &&
          mGrid->GetCellValue(i, 0).CmpNoCase(LABEL_TRACK.Translation()) == 0) {
         mGrid->SetReadOnly(i, 1);
      }

      popTagMap[ labelmap[i].name ] = mGrid->GetCellValue(i, 1);
   }

   // Populate the rest
   for (const auto &pair : mLocal.GetRange()) {
      const auto &n = pair.first;
      const auto &v = pair.second;
      if (popTagMap.find(n) == popTagMap.end()) {
         mGrid->AppendRows();
         mGrid->SetCellValue(i, 0, n);
         mGrid->SetCellValue(i, 1, v);
         i++;
      }
   }

   // Add an extra one to help with initial sizing and to show it can be done
   mGrid->AppendRows(1);

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   // Set the editors
   SetEditors();
   Layout();
   Fit();

   return true;
}

void TagsEditorDialog::OnChange(wxGridEvent & event)
{
   static bool ischanging = false;

   // Prevent recursion
   if (ischanging) {
      return;
   }

   event.Skip();

   if (event.GetCol() != 0) {
      return;
   }

   // Do not permit duplication of any of the tags.
   // Tags differing only in case are nondistinct.
   auto row = event.GetRow();
   const wxString key0 = mGrid->GetCellValue(row, 0).Upper();
   auto nn = mGrid->GetNumberRows();
   for (decltype(nn) ii = 0; ii < nn; ++ii) {
      if (ii == row)
         continue;
      auto key = mGrid->GetCellValue(ii, 0).Upper();
      if (key0.CmpNoCase(key) == 0) {
         ischanging = true;
         wxBell();
         mGrid->SetGridCursor(ii, 0);
         event.Veto();
         ischanging = false;
         break;
      }
   }

   return;
}

void TagsEditorDialog::OnEdit(wxCommandEvent & WXUNUSED(event))
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
   }

   wxDialogWrapper dlg(this, wxID_ANY, XO("Edit Genres"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
   dlg.SetName();
   wxTextCtrl *tc;

   ShuttleGui S(&dlg, eIsCreating);

   S.StartVerticalLay(true);
   {
      tc = S.AddTextWindow(wxT(""));
   }
   S.EndVerticalLay();

   S.AddStandardButtons();

   wxArrayString g;
   int cnt = mLocal.GetNumUserGenres();
   for (int i = 0; i < cnt; i++) {
      g.push_back(mLocal.GetUserGenre(i));
   }
   std::sort( g.begin(), g.end() );

   for (int i = 0; i < cnt; i++) {
      tc->AppendText(g[i] + wxT("\n"));
   }

   dlg.Center();
   if (dlg.ShowModal() == wxID_CANCEL) {
      return;
   }

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxFile f(fn.GetFullPath(), wxFile::write);
   if (!f.IsOpened() || !f.Write(tc->GetValue())) {
      AudacityMessageBox(
         XO("Unable to save genre file."),
         XO("Reset Genres") );
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditorDialog::OnReset(wxCommandEvent & WXUNUSED(event))
{
   int id = AudacityMessageBox(
      XO("Are you sure you want to reset the genre list to defaults?"),
      XO("Reset Genres"),
      wxYES_NO);

   if (id == wxNO) {
      return;
   }
   mLocal.LoadDefaultGenres();

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxTextFile tf(fn.GetFullPath());

   bool open = (tf.Exists() && tf.Open()) ||
               (!tf.Exists() && tf.Create());

   if (!open) {
      AudacityMessageBox(
         XO("Unable to open genre file."),
         XO("Reset Genres") );
      mLocal.LoadGenres();
      return;
   }

   tf.Clear();
   int cnt = mLocal.GetNumUserGenres();
   for (int i = 0; i < cnt; i++) {
      tf.AddLine(mLocal.GetUserGenre(i));
   }

   if (!tf.Write()) {
      AudacityMessageBox(
         XO("Unable to save genre file."),
         XO("Reset Genres") );
      mLocal.LoadGenres();
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditorDialog::OnClear(wxCommandEvent & WXUNUSED(event))
{
   mLocal.Clear();

   TransferDataToWindow();
}

void TagsEditorDialog::OnLoad(wxCommandEvent & WXUNUSED(event))
{
   wxString fn;

   // Ask the user for the real name
   fn = SelectFile(FileNames::Operation::_None,
      XO("Load Metadata As:"),
      FileNames::DataDir(),
      wxT("Tags.xml"),
      wxT("xml"),
      { FileNames::XMLFiles },
      wxFD_OPEN | wxRESIZE_BORDER,
      this);

   // User canceled...
   if (fn.empty()) {
      return;
   }

   // Load the metadata
   decltype(mLocal) temp;
   XMLFileReader reader;
   if (!reader.Parse(&temp, fn)) {
      // Inform user of load failure
      AudacityMessageBox(
         reader.GetErrorStr(),
         XO("Error Loading Metadata"),
         wxOK | wxCENTRE,
         this);
      return;
   }

   // Remember title and track in case they're read only
   wxString title = mLocal.GetTag(TAG_TITLE);
   wxString track = mLocal.GetTag(TAG_TRACK);

   // Replace existing tags with loaded ones
   mLocal = temp;

   // Restore title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, title);
   }

   // Restore track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, track);
   }

   // Go fill up the window
   TransferDataToWindow();

   return;
}

void TagsEditorDialog::OnSave(wxCommandEvent & WXUNUSED(event))
{
   wxString fn;

   // Refresh tags
   TransferDataFromWindow();

   // Ask the user for the real name
   fn = SelectFile(FileNames::Operation::_None,
      XO("Save Metadata As:"),
      FileNames::DataDir(),
      wxT("Tags.xml"),
      wxT("xml"),
      { FileNames::XMLFiles },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      this);

   // User canceled...
   if (fn.empty()) {
      return;
   }

   GuardedCall( [&] {
      // Create/Open the file
      XMLFileWriter writer{ fn, XO("Error Saving Tags File") };

      // Remember title and track in case they're read only
      wxString title = mLocal.GetTag(TAG_TITLE);
      wxString track = mLocal.GetTag(TAG_TRACK);

      // Clear title
      if (!mEditTitle) {
         mLocal.SetTag(TAG_TITLE, wxEmptyString);
      }

      // Clear track
      if (!mEditTrack) {
         mLocal.SetTag(TAG_TRACK, wxEmptyString);
      }

      auto cleanup = finally( [&] {
         // Restore title
         if (!mEditTitle) {
            mLocal.SetTag(TAG_TITLE, title);
         }

         // Restore track
         if (!mEditTrack) {
            mLocal.SetTag(TAG_TRACK, track);
         }
      } );

      // Write the metadata
      mLocal.WriteXML(writer);

      writer.Commit();
   } );
}

void TagsEditorDialog::OnSaveDefaults(wxCommandEvent & WXUNUSED(event))
{
   // Refresh tags
   TransferDataFromWindow();

   // Remember title and track in case they're read only
   wxString title = mLocal.GetTag(TAG_TITLE);
   wxString track = mLocal.GetTag(TAG_TRACK);

   // Clear title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, wxEmptyString);
   }

   // Clear track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, wxEmptyString);
   }

   // Remove any previous defaults
   gPrefs->DeleteGroup(wxT("/Tags"));

   // Write out each tag
   for (const auto &pair : mLocal.GetRange()) {
      const auto &n = pair.first;
      const auto &v = pair.second;
      gPrefs->Write(wxT("/Tags/") + n, v);
   }
   gPrefs->Flush();

   // Restore title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, title);
   }

   // Restore track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, track);
   }
}

void TagsEditorDialog::OnAdd(wxCommandEvent & WXUNUSED(event))
{
   mGrid->AppendRows();
}

void TagsEditorDialog::OnRemove(wxCommandEvent & WXUNUSED(event))
{
   size_t row = mGrid->GetGridCursorRow();

   if (!mEditTitle &&
       mGrid->GetCellValue(row, 0).CmpNoCase(LABEL_TITLE.Translation()) == 0) {
      return;
   }
   else if (!mEditTrack &&
            mGrid->GetCellValue(row, 0)
               .CmpNoCase(LABEL_TRACK.Translation()) == 0) {
      return;
   }
   else if (row < STATICCNT) {
      mGrid->SetCellValue(row, 1, wxEmptyString);
   }
   else if (row >= STATICCNT) {
      mGrid->DeleteRows(row, 1);
   }
}

void TagsEditorDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
#if defined(__WXMAC__)
      // The cell editors do not capture the ENTER key, so it invokes
      // the default button ("Ok") when it should just close the
      // editor. So, cancel the "Ok" action.
      return;
#endif
   }

   if (!Validate() || !TransferDataFromWindow()) {
      return;
   }

   *mTags = mLocal;

   wxRect r = GetRect();
   gPrefs->Write(wxT("/TagsEditorDialog/x"), r.x);
   gPrefs->Write(wxT("/TagsEditorDialog/y"), r.y);
   gPrefs->Write(wxT("/TagsEditorDialog/width"), r.width);
   gPrefs->Write(wxT("/TagsEditorDialog/height"), r.height);
   gPrefs->Flush();

   EndModal(wxID_OK);
}

void TagsEditorDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   DoCancel(false);
}

void TagsEditorDialog::DoCancel(bool escKey)
{
   if (mGrid->IsCellEditControlShown()) {
      auto editor = mGrid->GetCellEditor(mGrid->GetGridCursorRow(),
         mGrid->GetGridCursorCol());
      editor->Reset();
      // To avoid memory leak, don't forget DecRef()!
      editor->DecRef();
      mGrid->HideCellEditControl();
#if defined(__WXMSW__)
      return;
#endif
   }

   auto focus = wxWindow::FindFocus();
   if (escKey && focus == mGrid)
      return;

   EndModal(wxID_CANCEL);
}

void TagsEditorDialog::OnKeyDown(wxKeyEvent &event)
{
   if (event.GetKeyCode() == WXK_ESCAPE)
      DoCancel(true);
   else
      event.Skip();
}

void TagsEditorDialog::SetEditors()
{
   int cnt = mGrid->GetNumberRows();

   for (int i = 0; i < cnt; i++) {
      wxString label = mGrid->GetCellValue(i, 0);
      if (label.CmpNoCase(LABEL_GENRE.Translation()) == 0) {
         // This use of GetDefaultEditorForType does not require DecRef.
         mGrid->SetCellEditor(i, 1, mGrid->GetDefaultEditorForType(wxT("Combo")));
      }
      else {
         mGrid->SetCellEditor(i, 1, NULL); //mGrid->GetDefaultEditor());
      }
   }
}

void TagsEditorDialog::PopulateGenres()
{
   int cnt = mLocal.GetNumUserGenres();
   int i;
   wxString parm;
   wxArrayString g;

   for (i = 0; i < cnt; i++) {
      g.push_back(mLocal.GetUserGenre(i));
   }
   std::sort( g.begin(), g.end() );

   for (i = 0; i < cnt; i++) {
      parm = parm + (i == 0 ? wxT("") : wxT(",")) + g[i];
   }

   // Here was a memory leak!  wxWidgets docs for wxGrid::GetDefaultEditorForType() say:
   // "The caller must call DecRef() on the returned pointer."
   auto editor = mGrid->GetDefaultEditorForType(wxT("Combo"));
   editor->SetParameters(parm);
   editor->DecRef();
}

bool TagsEditorDialog::IsWindowRectValid(const wxRect *windowRect) const
{
   wxDisplay display;
   wxPoint topLeft(windowRect->GetTopLeft().x, windowRect->GetTopLeft().y);
   wxPoint topRight(windowRect->GetTopRight().x, windowRect->GetTopRight().y);
   wxPoint bottomLeft(windowRect->GetBottomLeft().x, windowRect->GetBottomLeft().y);
   wxPoint bottomRight(windowRect->GetBottomRight().x, windowRect->GetBottomRight().y);
   display.GetFromPoint(topLeft);
   if (display.GetFromPoint(topLeft) == -1 &&
       display.GetFromPoint(topRight) == -1 &&
       display.GetFromPoint(bottomLeft) == -1 &&
       display.GetFromPoint(bottomRight) == -1) {
      return false;
   }

   return true;
}
