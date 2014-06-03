/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class LabelDialog
\brief Dialog for editing labels.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/button.h>
#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dc.h>
#include <wx/dialog.h>
#include <wx/filedlg.h>
#include <wx/grid.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textdlg.h>

#include "Internat.h"
#include "LabelDialog.h"
#include "LabelTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"
#include "ViewInfo.h"
#include "widgets/TimeTextCtrl.h"

#include "FileDialog.h"

enum Column
{
   Col_Track,
   Col_Label,
   Col_Stime,
   Col_Etime,
   Col_Max
};



class RowData
{
 public:
   RowData() {};

   int index;

   wxString title;
   double stime;
   double etime;
};

enum {
   ID_INSERTA = 11000,
   ID_INSERTB,
   ID_REMOVE,
   ID_IMPORT,
   ID_EXPORT
};

BEGIN_EVENT_TABLE(LabelDialog, wxDialog)
   EVT_GRID_SELECT_CELL(LabelDialog::OnSelectCell)
   EVT_GRID_CELL_CHANGE(LabelDialog::OnCellChange)
   EVT_BUTTON(ID_INSERTA, LabelDialog::OnInsert)
   EVT_BUTTON(ID_INSERTB, LabelDialog::OnInsert)
   EVT_BUTTON(ID_REMOVE,  LabelDialog::OnRemove)
   EVT_BUTTON(ID_IMPORT,  LabelDialog::OnImport)
   EVT_BUTTON(ID_EXPORT,  LabelDialog::OnExport)
   EVT_BUTTON(wxID_OK,      LabelDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL,  LabelDialog::OnCancel)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, LabelDialog::OnUpdate)
END_EVENT_TABLE()

LabelDialog::LabelDialog(wxWindow *parent,
                         DirManager *dirmanager,
                         TrackList *tracks,
                         ViewInfo &viewinfo,
                         double rate,
                         const wxString & format)
: wxDialog(parent,
           wxID_ANY,
           _("Edit Labels"),
           wxDefaultPosition,
           wxSize(800, 600),
           wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
  mDirManager(dirmanager),
  mTracks(tracks),
  mViewInfo(&viewinfo),
  mRate(rate),
  mFormat(format)
{
   // Create the main sizer
   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);

   // A little instruction
   wxStaticText *instruct =
      new wxStaticText(this,
                       wxID_ANY,
                       _("Press F2 or double click to edit cell contents."));
   instruct->SetName(instruct->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   vs->Add(instruct,
           0,
           wxALIGN_LEFT | wxALL,
           5);

   // Create the main sizer
   mGrid = new Grid(this, wxID_ANY);
   vs->Add(mGrid, 1, wxEXPAND | wxALL, 5);

   // Create the action buttons
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);
   hs->Add(new wxButton(this, ID_INSERTA, _("Insert &After")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_INSERTB, _("Insert &Before")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_REMOVE,  _("&Remove")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_IMPORT,  _("&Import...")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_EXPORT,  _("&Export...")), 1, wxCENTER | wxALL, 5);
   vs->Add(hs, 0, wxEXPAND | wxCENTER | wxALL, 5);

   // Create the exit buttons
   vs->Add(CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND);

   // Make it so
   SetSizer(vs);

   // Build the initial (empty) grid
   mGrid->CreateGrid(0, Col_Max);
   mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

   /* i18n-hint: (noun).  A track contains waves, audio etc.*/
   mGrid->SetColLabelValue(0,_("Track"));
   /* i18n-hint: (noun)*/
   mGrid->SetColLabelValue(1,_("Label"));
   /* i18n-hint: (noun) of a label*/
   mGrid->SetColLabelValue(2,_("Start Time"));
   /* i18n-hint: (noun) of a label*/
   mGrid->SetColLabelValue(3,_("End Time"));

   // Create and remember editors.  No need to delete these as the wxGrid will
   // do it for us.
   mChoiceEditor = (ChoiceEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_CHOICE);
   mTimeEditor = (TimeEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_TIME);

   // Initialize and set the track name column attributes
   wxGridCellAttr *attr = new wxGridCellAttr();
   attr->SetEditor(mChoiceEditor);
   mGrid->SetColAttr(Col_Track, attr);
   mTrackNames.Add(_("New..."));

   // Initialize and set the time column attributes
   attr = new wxGridCellAttr();
   attr->SetRenderer(mGrid->GetDefaultRendererForType(GRID_VALUE_TIME));
   attr->SetEditor(mTimeEditor);
   attr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);
   mGrid->SetColAttr(Col_Stime, attr);
   mGrid->SetColAttr(Col_Etime, attr->Clone());

   // Seems there's a bug in wxGrid.  Adding only 1 row does not
   // allow SetCellSize() to work properly and you will not get
   // the expected 1 row by 4 column cell.
   //
   // So, we set the minimum row height to 0 and basically hide
   // the extra row by setting its height to 0.  And not allowing the
   // rows to be manually resized prevents the user from ever seeing
   // the extra row.
   mGrid->SetRowMinimalAcceptableHeight(0);
   mGrid->EnableDragRowSize(false);

   // Locate all labels in current track list
   FindAllLabels();

   // Populate the grid
   TransferDataToWindow();

   // Resize the label name column and ensure it doesn't go below an
   // arbitrary width.
   //
   // This should not be in TransferDataToWindow() since a user might
   // resize the column and we'd resize it back to the minimum.
   mGrid->AutoSizeColumn(Col_Label, false );
   mGrid->SetColSize(Col_Label, wxMax(150, mGrid->GetColSize(Col_Label)));
   mGrid->SetColMinimalWidth(Col_Label, mGrid->GetColSize(Col_Label));

   // Layout the works
   Layout();

   // Resize width based on width of columns and the vertical scrollbar
   wxRect r = mGrid->GetGridColLabelWindow()->GetRect();
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
   r.width += sb.GetSize().GetWidth() + 6;
   SetClientSize(r.width, 300);

   // Make sure it doesn't go below this size
   r = GetRect();
   SetSizeHints(r.GetWidth(), r.GetHeight());

   // Center on display
   Center();
}

LabelDialog::~LabelDialog()
{
   int cnt = mData.GetCount();

   // Delete any RowData we've allocated
   while (cnt) {
      RowData *rd = mData[--cnt];
      delete rd;
   }
}

bool LabelDialog::TransferDataToWindow()
{
   int cnt = mData.GetCount();
   int i;

   // Set the editor parameters.  Do this each time since they may change
   // due to new tracks and change in TimeTextCtrl format.  Rate won't
   // change but might as well leave it here.
   mChoiceEditor->SetChoices(mTrackNames);
   mTimeEditor->SetFormat(mFormat);
   mTimeEditor->SetRate(mRate);

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows
   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   // Add the exact number that we'll need
   mGrid->InsertRows(0, cnt);

   // Populate the rows
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      // Set the cell contents
      mGrid->SetCellValue(i, Col_Track, TrackName(rd->index));
      mGrid->SetCellValue(i, Col_Label, rd->title);
      mGrid->SetCellValue(i, Col_Stime, wxString::Format(wxT("%g"), rd->stime));
      mGrid->SetCellValue(i, Col_Etime, wxString::Format(wxT("%g"), rd->etime));
   }

   // Autosize all the rows
   mGrid->AutoSizeRows(true);

   // Resize the track name column.  Use a wxChoice to determine the maximum
   // width needed.
   wxChoice tc(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, mTrackNames);
   mGrid->SetColSize(Col_Track, tc.GetSize().x);
   mGrid->SetColMinimalWidth(Col_Track, tc.GetSize().x);

   // Autosize the time columns and set their minimal widths
   mGrid->AutoSizeColumn(Col_Stime);
   mGrid->AutoSizeColumn(Col_Etime);

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   return true;
}

bool LabelDialog::Show(bool show)
{
   bool ret = wxDialog::Show(show);

   // Set initial row
   // (This will not work until the grid is actually displayed)
   if (mInitialRow != -1) {
      mGrid->SetGridCursor(mInitialRow, Col_Label);
   }

   return ret;
}

bool LabelDialog::TransferDataFromWindow()
{
   int cnt = mData.GetCount();
   int i;
   TrackListIterator iter(mTracks);
   Track *t;
   int tndx = 0;

   // Clear all label tracks of labels
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         LabelTrack *lt = (LabelTrack *)t;
         tndx++;

         for (i = lt->GetNumLabels() - 1; i >= 0 ; i--) {
            lt->DeleteLabel(i);
         }
      }
   }

   // Create any added tracks
   while (tndx < (int)mTrackNames.GetCount() - 1) {

      // Extract the name
      wxString name = mTrackNames[tndx + 1].AfterFirst(wxT('-')).Mid(1);

      // Create the new track and add to track list
      LabelTrack *newTrack = new LabelTrack(mDirManager);
      newTrack->SetName(name);
      mTracks->Add(newTrack);
      tndx++;
   }

   // Repopulate with updated labels
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      // Look for track with matching index
      tndx = 1;
      for (t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label && rd->index == tndx++) {
            break;
         }
      }
      wxASSERT(t);
      if (!t)
         return false;

      // Add the label to it
      ((LabelTrack *) t)->AddLabel(rd->stime, rd->etime, rd->title);
      ((LabelTrack *) t)->Unselect();
   }

   return true;
}

bool LabelDialog::Validate()
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
      mGrid->SaveEditControlValue();
   }

   return true;
}

wxString LabelDialog::TrackName(int & index, wxString dflt)
{
   // Generate a new track name if the passed index is out of range
   if (index < 1 || index >= (int)mTrackNames.GetCount()) {
      index = mTrackNames.GetCount();
      mTrackNames.Add(wxString::Format(wxT("%d - %s"), index, dflt.c_str()));
   }

   // Return the track name
   return mTrackNames[index];
}

void LabelDialog::FindAllLabels()
{
   TrackListIterator iter(mTracks);
   Track *t;

   mInitialRow = -1;

   // Add labels from all label tracks
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         AddLabels((LabelTrack *) t);
      }
   }

   if (mData.GetCount() == 0) {
      wxCommandEvent e;
      OnInsert(e);
   }
}

void LabelDialog::AddLabels(LabelTrack *t)
{
   wxString lab;
   int tndx = 0;
   int i;

   // Add a new track name
   TrackName(tndx, t->GetName());

   // Add each label in the track
   for (i = 0; i < t->GetNumLabels(); i++) {
      const LabelStruct *ls = t->GetLabel(i);
      RowData *rd = new RowData();

      rd->index = tndx;
      rd->stime = ls->t;
      rd->etime = ls->t1;
      rd->title = ls->title;

      mData.Add(rd);

      if (i == t->getSelectedIndex()) {
         mInitialRow = mData.GetCount() - 1;
      }
   }
}

void LabelDialog::OnUpdate(wxCommandEvent &event)
{
   // Remember the new format and repopulate grid
   mFormat = event.GetString();
   TransferDataToWindow();

   event.Skip(false);
}

void LabelDialog::OnInsert(wxCommandEvent &event)
{
   RowData *rd = new RowData();
   int cnt = mData.GetCount();
   int row = 0;
   int index = 0;

   // Make sure the edit control isn't active before inserting any rows
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
   }

   // Attempt to guess which track the label should reside on
   if (cnt > 0) {
      row = mGrid->GetCursorRow();
      if (row > 0 && row >= cnt) {
         index = mTrackNames.Index(mGrid->GetCellValue(row - 1, Col_Track));
      }
      else {
         index = mTrackNames.Index(mGrid->GetCellValue(row, Col_Track));
      }
   }

   // Initialize the new label
   rd->index = index;
   rd->stime = 0.0;
   rd->etime = 0.0;
   rd->title = wxT("");

   // Insert it before or after the current row
   if (event.GetId() == ID_INSERTA && row < cnt) {
      row++;
   }
   mData.Insert(rd, row);

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition cursor to new row/col and put user into edit mode to
   // set the label name
   mGrid->SetGridCursor(row, Col_Label);
   mGrid->EnableCellEditControl(true);
   mGrid->ShowCellEditControl();
}

void LabelDialog::OnRemove(wxCommandEvent & WXUNUSED(event))
{
   int row = mGrid->GetCursorRow();
   int col = mGrid->GetCursorColumn();
   int cnt = mData.GetCount();

   // Don't try to remove if no labels exist
   if (cnt == 0) {
      return;
   }

   // Make sure the edit control isn't active before removing rows
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
   }

   // Remove the row
   RowData *rd = mData[row];
   mTrackNames.RemoveAt(rd->index);
   mData.RemoveAt(row);
   delete rd;

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition the cursor
   if (row > 0 && row >= --cnt) {
      row--;
   }
   mGrid->SetGridCursor(row, col);

   // Make sure focus isn't lost
   if (mData.GetCount() == 0 && wxWindow::FindFocus() == mGrid->GetGridWindow()) {
      wxWindow *ok = wxWindow::FindWindowById( wxID_OK, this);
      if (ok) {
         ok->SetFocus();
      }
   }
}

void LabelDialog::OnImport(wxCommandEvent & WXUNUSED(event))
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());

   // Ask user for a filename
   wxString fileName =
       FileSelector(_("Select a text file containing labels..."),
                    path,     // Path
                    wxT(""),       // Name
                    wxT(".txt"),   // Extension
                    _("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
                    wxRESIZE_BORDER, // Flags
                    this);    // Parent

   // They gave us one...
   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);
      gPrefs->Flush();

      wxTextFile f;

      // Get at the data
      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
      }
      else {
         // Create a temporary label track and load the labels
         // into it
         LabelTrack *lt = new LabelTrack(mDirManager);
         lt->Import(f);

         // Add the labesls to our collection
         AddLabels(lt);

         // Done with the temporary track
         delete lt;
     }

      // Repopulate the grid
      TransferDataToWindow();
   }
}

void LabelDialog::OnExport(wxCommandEvent & WXUNUSED(event))
{
   int cnt = mData.GetCount();

   // Silly user (could just disable the button, but that's a hassle ;-))
   if (cnt == 0) {
      wxMessageBox(_("No labels to export."));
      return;
   }

   // Extract the actual name.
   wxString fName = mTrackNames[mTrackNames.GetCount() - 1].AfterFirst(wxT('-')).Mid(1);

   fName = FileSelector(_("Export Labels As:"),
                        NULL,
                        fName.c_str(),
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                        this);

   if (fName == wxT(""))
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(fName)) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + wxT("~");
#else
      wxString safetyFileName = fName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(fName);
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   // Transfer our collection to a temporary label track
   LabelTrack *lt = new LabelTrack(mDirManager);
   int i;

   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      lt->AddLabel(rd->stime, rd->etime, rd->title);
   }

   // Export them and clean
   lt->Export(f);
   delete lt;

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void LabelDialog::OnSelectCell(wxGridEvent &event)
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while( t )
   {
      t->SetSelected( true );
      t = iter.Next();
   }

   if (!mData.empty())
   {
      RowData *rd;
      rd = mData[event.GetRow()];
      mViewInfo->sel0 = rd->stime;
      mViewInfo->sel1 = rd->etime;

      GetActiveProject()->RedrawProject();
   }

   event.Skip();
}

void LabelDialog::OnCellChange(wxGridEvent &event)
{
   static bool guard = false;
   int row = event.GetRow();
   RowData *rd;

   // Guard against recursion which can happen when a change to the "new label" row
   // is made.  When InsertRow() is done in TransferDataToWindow(), checks are made
   // within wxGrid to see if the edit control is active and since it hasn't yet
   // been marked inactive on the first time through here, we get entered again.
   // Sort of a double change.  I think this is probably a bug in wxGrid.
   if (guard) {
      return;
   }
   guard = true;

   // The change was to an existing label, so go process it based
   // on which column was changed.
   rd = mData[row];
   switch (event.GetCol())
   {
      case Col_Track:
         OnChangeTrack(event, row, rd);
      break;

      case Col_Label:
         OnChangeLabel(event, row, rd);
      break;

      case Col_Stime:
         OnChangeStime(event, row, rd);
      break;

      case Col_Etime:
         OnChangeEtime(event, row, rd);
      break;
   }

   // Done...no need for protection anymore
   guard = false;

   return;
}

void LabelDialog::OnChangeTrack(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   wxString val = mGrid->GetCellValue(row, Col_Track);

   // User selected the "New..." choice so ask for a new name
   if (mTrackNames.Index(val) == 0) {
      wxTextEntryDialog d(this,
                          _("New Label Track"),
                          _("Enter track name"),
                          /* i18n-hint: (noun) it's the name of a kind of track.*/
                          _("Label Track"));

      // User canceled so repopulating the grid will set the track
      // name to the orignal value
      if (d.ShowModal() == wxID_CANCEL) {
         TransferDataToWindow();
         return;
      }

      // Force generation of a new track name
      rd->index = 0;
      TrackName(rd->index, d.GetValue());
   }
   else {
      // Remember the tracks index
      rd->index = mTrackNames.Index(val);
   }

   // Repopulate the grid
   TransferDataToWindow();

   return;
}

void LabelDialog::OnChangeLabel(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   rd->title = mGrid->GetCellValue(row, Col_Label);

   return;
}

void LabelDialog::OnChangeStime(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   mGrid->GetCellValue(row, Col_Stime).ToDouble(&rd->stime);
   if (rd->etime < rd->stime) {
      rd->etime = rd->stime;
      mGrid->SetCellValue(row, Col_Etime, wxString::Format(wxT("%g"), rd->etime));
   }

   return;
}

void LabelDialog::OnChangeEtime(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   mGrid->GetCellValue(row, Col_Etime).ToDouble(&rd->etime);
   if (rd->etime < rd->stime) {
      rd->stime = rd->etime;
      mGrid->SetCellValue(row, Col_Stime, wxString::Format(wxT("%g"), rd->stime));
   }

   return;
}

void LabelDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
      return;
   }

   // Standard handling
   if (Validate() && TransferDataFromWindow()) {
      EndModal(wxID_OK);
   }

   return;
}

void LabelDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->GetCellEditor(mGrid->GetGridCursorRow(),
                           mGrid->GetGridCursorCol())
                           ->Reset();
      mGrid->HideCellEditControl();
      return;
   }

   // Standard handling
   EndModal(wxID_CANCEL);

   return;
}
