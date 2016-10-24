/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class LabelDialog
\brief Dialog for editing labels.

*//*******************************************************************/

#include "Audacity.h"
#include "LabelDialog.h"

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

#include "ShuttleGui.h"
#include "Internat.h"
#include "LabelTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "ViewInfo.h"
#include "widgets/NumericTextCtrl.h"

#include "FileDialog.h"
#include <limits>

enum Column
{
   Col_Track,
   Col_Label,
   Col_Stime,
   Col_Etime,
   Col_Lfreq,
   Col_Hfreq,
   Col_Max
};



class RowData
{
 public:
   RowData(int index_, const wxString &title_, SelectedRegion selectedRegion_)
      : index(index_), title(title_), selectedRegion(selectedRegion_)
   {}

   int index;
   wxString title;
   SelectedRegion selectedRegion;
};

enum {
   ID_INSERTA = 11000,
   ID_INSERTB,
   ID_REMOVE,
   ID_IMPORT,
   ID_EXPORT
};

BEGIN_EVENT_TABLE(LabelDialog, wxDialogWrapper)
   EVT_GRID_SELECT_CELL(LabelDialog::OnSelectCell)
   EVT_GRID_CELL_CHANGED(LabelDialog::OnCellChange)
   EVT_BUTTON(ID_INSERTA, LabelDialog::OnInsert)
   EVT_BUTTON(ID_INSERTB, LabelDialog::OnInsert)
   EVT_BUTTON(ID_REMOVE,  LabelDialog::OnRemove)
   EVT_BUTTON(ID_IMPORT,  LabelDialog::OnImport)
   EVT_BUTTON(ID_EXPORT,  LabelDialog::OnExport)
   EVT_BUTTON(wxID_OK,      LabelDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL,  LabelDialog::OnCancel)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, LabelDialog::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_FREQUENCYTEXTCTRL_UPDATED,
               LabelDialog::OnFreqUpdate)
END_EVENT_TABLE()

LabelDialog::LabelDialog(wxWindow *parent,
                         TrackFactory &factory,
                         TrackList *tracks,
                         LabelTrack *selectedTrack,
                         int index,
                         ViewInfo &viewinfo,
                         double rate,
                         const wxString & format, const wxString &freqFormat)
: wxDialogWrapper(parent,
           wxID_ANY,
           _("Edit Labels"),
           wxDefaultPosition,
           wxSize(800, 600),
           wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
  mFactory(factory),
  mTracks(tracks)
  , mSelectedTrack(selectedTrack)
  , mIndex(index)
  , mViewInfo(&viewinfo),
  mRate(rate),
  mFormat(format)
  , mFreqFormat(freqFormat)
{
   SetName(GetTitle());

   {
      // Create the main sizer
      auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);

      // A little instruction
      wxStaticText *instruct =
         safenew wxStaticText(this,
         wxID_ANY,
         _("Press F2 or double click to edit cell contents."));
      instruct->SetName(instruct->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      vs->Add(instruct,
         0,
         wxALIGN_LEFT | wxALL,
         5);

      // Create the main sizer
      mGrid = safenew Grid(this, wxID_ANY);
      vs->Add(mGrid, 1, wxEXPAND | wxALL, 5);

      // Create the action buttons
      {
         auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         hs->Add(safenew wxButton(this, ID_INSERTA, _("Insert &After")), 1, wxCENTER | wxALL, 5);
         hs->Add(safenew wxButton(this, ID_INSERTB, _("Insert &Before")), 1, wxCENTER | wxALL, 5);
         hs->Add(safenew wxButton(this, ID_REMOVE, _("&Remove")), 1, wxCENTER | wxALL, 5);
         hs->Add(safenew wxButton(this, ID_IMPORT, _("&Import...")), 1, wxCENTER | wxALL, 5);
         hs->Add(safenew wxButton(this, ID_EXPORT, _("&Export...")), 1, wxCENTER | wxALL, 5);
         vs->Add(hs.release(), 0, wxEXPAND | wxCENTER | wxALL, 5);
      }

      // Create the exit buttons
      vs->Add(CreateStdButtonSizer(this, eCancelButton | eOkButton).release(), 0, wxEXPAND);

      // Make it so
      SetSizer(vs.release());
   }

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
   /* i18n-hint: (noun) of a label*/
   mGrid->SetColLabelValue(4,_("Low Frequency"));
   /* i18n-hint: (noun) of a label*/
   mGrid->SetColLabelValue(5,_("High Frequency"));

   // Create and remember editors.  No need to DELETE these as the wxGrid will
   // do it for us.  (The DecRef() that is needed after GetDefaultEditorForType
   // becomes the duty of the wxGridCellAttr objects after we set them in the grid.)
   mChoiceEditor = (ChoiceEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_CHOICE);
   mTimeEditor = static_cast<NumericEditor*>
      (mGrid->GetDefaultEditorForType(GRID_VALUE_TIME));
   mFrequencyEditor = static_cast<NumericEditor *>
      (mGrid->GetDefaultEditorForType(GRID_VALUE_FREQUENCY));

   // Initialize and set the track name column attributes
   wxGridCellAttr *attr;
   mGrid->SetColAttr(Col_Track, (attr = safenew wxGridCellAttr));
   attr->SetEditor(mChoiceEditor);
   mTrackNames.Add(_("New..."));

   // Initialize and set the time column attributes
   mGrid->SetColAttr(Col_Stime, (attr = safenew wxGridCellAttr));
   // Don't need DecRef() after this GetDefaultRendererForType.
   attr->SetRenderer(mGrid->GetDefaultRendererForType(GRID_VALUE_TIME));
   attr->SetEditor(mTimeEditor);
   attr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);

   mGrid->SetColAttr(Col_Etime, attr->Clone());

   // Initialize and set the frequency column attributes
   mGrid->SetColAttr(Col_Lfreq, (attr = safenew wxGridCellAttr));
   // Don't need DecRef() after this GetDefaultRendererForType.
   attr->SetRenderer(mGrid->GetDefaultRendererForType(GRID_VALUE_FREQUENCY));
   attr->SetEditor(mFrequencyEditor);
   attr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);

   mGrid->SetColAttr(Col_Hfreq, attr->Clone());
   
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
}

bool LabelDialog::TransferDataToWindow()
{
   int cnt = mData.size();
   int i;

   // Set the editor parameters.  Do this each time since they may change
   // due to NEW tracks and change in NumericTextCtrl format.  Rate won't
   // change but might as well leave it here.
   mChoiceEditor->SetChoices(mTrackNames);
   mTimeEditor->SetFormat(mFormat);
   mTimeEditor->SetRate(mRate);
   mFrequencyEditor->SetFormat(mFreqFormat);
   mFrequencyEditor->SetRate(mRate);

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
      RowData &rd = mData[i];

      // Set the cell contents
      mGrid->SetCellValue(i, Col_Track, TrackName(rd.index));
      mGrid->SetCellValue(i, Col_Label, rd.title);
      mGrid->SetCellValue(i, Col_Stime,
         wxString::Format(wxT("%g"), rd.selectedRegion.t0()));
      mGrid->SetCellValue(i, Col_Etime,
         wxString::Format(wxT("%g"), rd.selectedRegion.t1()));
      mGrid->SetCellValue(i, Col_Lfreq,
         wxString::Format(wxT("%g"), rd.selectedRegion.f0()));
      mGrid->SetCellValue(i, Col_Hfreq,
         wxString::Format(wxT("%g"), rd.selectedRegion.f1()));
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
   mGrid->AutoSizeColumn(Col_Lfreq);
   mGrid->AutoSizeColumn(Col_Hfreq);

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   return true;
}

bool LabelDialog::Show(bool show)
{
   bool ret = wxDialogWrapper::Show(show);

   // Set initial row
   // (This will not work until the grid is actually displayed)
   if (show && mInitialRow != -1) {
      mGrid->SetGridCursor(mInitialRow, Col_Label);
   }

   return ret;
}

bool LabelDialog::TransferDataFromWindow()
{
   int cnt = mData.size();
   int i;
   TrackListIterator iter(mTracks);
   Track *t;
   int tndx = 0;

   // Clear label tracks of labels
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         ++tndx;
         LabelTrack *lt = static_cast<LabelTrack*>(t);
         if (!mSelectedTrack) {
            for (i = lt->GetNumLabels() - 1; i >= 0 ; i--) {
               lt->DeleteLabel(i);
            }
         }
         else if (mSelectedTrack == lt && mIndex > -1) {
            lt->DeleteLabel(mIndex);
         }
         else
            // Do nothing to the nonselected tracks
            ;
      }
   }

   // Create any added tracks
   while (tndx < (int)mTrackNames.GetCount() - 1) {

      // Extract the name
      wxString name = mTrackNames[tndx + 1].AfterFirst(wxT('-')).Mid(1);

      // Create the NEW track and add to track list
      auto newTrack = mFactory.NewLabelTrack();
      newTrack->SetName(name);
      mTracks->Add(std::move(newTrack));
      tndx++;
   }

   // Repopulate with updated labels
   for (i = 0; i < cnt; i++) {
      RowData &rd = mData[i];

      // Look for track with matching index
      tndx = 1;
      for (t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label && rd.index == tndx++) {
            break;
         }
      }
      wxASSERT(t);
      if (!t)
         return false;

      // Add the label to it
      static_cast<LabelTrack *>(t)->AddLabel(rd.selectedRegion, rd.title);
      static_cast<LabelTrack *>(t)->Unselect();
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

wxString LabelDialog::TrackName(int & index, const wxString &dflt)
{
   // Generate a NEW track name if the passed index is out of range
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


   // Add labels from all label tracks
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label)
         AddLabels(static_cast<const LabelTrack *>(t));
   }

   FindInitialRow();

   if (mData.size() == 0) {
      wxCommandEvent e;
      OnInsert(e);
   }
}

void LabelDialog::AddLabels(const LabelTrack *t)
{
   wxString lab;
   int tndx = 0;
   int i;

   // Add a NEW track name
   TrackName(tndx, t->GetName());

   // If editor was invoked for one label, add that one only, else add all.
   if (!mSelectedTrack || mSelectedTrack == t) {
      for (i = 0; i < t->GetNumLabels(); i++) {
         const LabelStruct *ls = t->GetLabel(i);

         if (mIndex < 0 || mIndex == i)
            mData.push_back(RowData(tndx, ls->title, ls->selectedRegion));
      }
   }
}

void LabelDialog::FindInitialRow()
{
   int cnt = mData.size();
   mInitialRow = -1;

   if (cnt == 0)
      return;

   // find closest previous label

   double distMin = std::numeric_limits<double>::max();
   double dist;
   double t0 = mViewInfo->selectedRegion.t0();
   int i;
   for (i = 0; i < cnt; i++)
   {
      dist = t0 - mData[i].selectedRegion.t0();
      if (dist >= 0.0 && dist < distMin)
      {
         mInitialRow = i;
         distMin = dist;
      }
   }

   // if no previous label was found, find first label

   if (mInitialRow == -1)
   {
      double t0Min = std::numeric_limits<double>::max();
      for (i = 0; i < cnt; i++)
      {
         if (mData[i].selectedRegion.t0() < t0Min)
         {
            mInitialRow  = i;
            t0Min = mData[i].selectedRegion.t0();
         }
      }
   }
}

void LabelDialog::OnUpdate(wxCommandEvent &event)
{
   // Remember the NEW format and repopulate grid
   mFormat = event.GetString();
   TransferDataToWindow();

   event.Skip(false);
}

void LabelDialog::OnFreqUpdate(wxCommandEvent &event)
{
   // Remember the NEW format and repopulate grid
   mFreqFormat = event.GetString();
   TransferDataToWindow();

   event.Skip(false);
}

void LabelDialog::OnInsert(wxCommandEvent &event)
{
   int cnt = mData.size();
   int row = 0;
   int index = 0;

   // Make sure the edit control isn't active before inserting any rows
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
   }

   // Attempt to guess which track the label should reside on
   if (cnt > 0) {
      row = mGrid->GetGridCursorRow();
      if (row > 0 && row >= cnt) {
         index = mTrackNames.Index(mGrid->GetCellValue(row - 1, Col_Track));
      }
      else {
         index = mTrackNames.Index(mGrid->GetCellValue(row, Col_Track));
      }
   }

   // Insert NEW label before or after the current row
   if (event.GetId() == ID_INSERTA && row < cnt) {
      row++;
   }
   mData.insert(mData.begin() + row, RowData(index, wxT(""), SelectedRegion()));

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition cursor to NEW row/col and put user into edit mode to
   // set the label name
   mGrid->SetGridCursor(row, Col_Label);
   mGrid->EnableCellEditControl(true);
   mGrid->ShowCellEditControl();
}

void LabelDialog::OnRemove(wxCommandEvent & WXUNUSED(event))
{
   int row = mGrid->GetGridCursorRow();
   int col = mGrid->GetGridCursorCol();
   int cnt = mData.size();

   // Don't try to remove if no labels exist
   if (cnt == 0) {
      return;
   }

   // Make sure the edit control isn't active before removing rows
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
   }

   // Remove the row
   //RowData &rd = mData[row];
   mData.erase(mData.begin() + row);

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition the cursor
   if (row > 0 && row >= --cnt) {
      row--;
   }
   mGrid->SetGridCursor(row, col);

   // Make sure focus isn't lost
   if (mData.size() == 0 && wxWindow::FindFocus() == mGrid->GetGridWindow()) {
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
                    _("Text files (*.txt)|*.txt|All files|*"),
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
         auto lt = mFactory.NewLabelTrack();
         lt->Import(f);

         // Add the labels to our collection
         AddLabels(lt.get());

         // Done with the temporary track
     }

      // Repopulate the grid
      TransferDataToWindow();
   }
}

void LabelDialog::OnExport(wxCommandEvent & WXUNUSED(event))
{
   int cnt = mData.size();

   // Silly user (could just disable the button, but that's a hassle ;-))
   if (cnt == 0) {
      wxMessageBox(_("No labels to export."));
      return;
   }

   // Extract the actual name.
   wxString fName = mTrackNames[mTrackNames.GetCount() - 1].AfterFirst(wxT('-')).Mid(1);

   fName = FileSelector(_("Export Labels As:"),
      wxEmptyString,
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
   wxFile{}.Create(fName);
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   // Transfer our collection to a temporary label track
   auto lt = mFactory.NewLabelTrack();
   int i;

   for (i = 0; i < cnt; i++) {
      RowData &rd = mData[i];

      lt->AddLabel(rd.selectedRegion, rd.title);
   }

   // Export them and clean
   lt->Export(f);

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
      RowData &rd = mData[event.GetRow()];
      mViewInfo->selectedRegion = rd.selectedRegion;

      GetActiveProject()->RedrawProject();
   }

   event.Skip();
}

void LabelDialog::OnCellChange(wxGridEvent &event)
{
   static bool guard = false;
   int row = event.GetRow();

   // Guard against recursion which can happen when a change to the "NEW label" row
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
   RowData *rd = &mData[row];
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

      case Col_Lfreq:
         OnChangeLfreq(event, row, rd);
      break;

      case Col_Hfreq:
         OnChangeHfreq(event, row, rd);
      break;
   }

   // Done...no need for protection anymore
   guard = false;

   return;
}

void LabelDialog::OnChangeTrack(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   wxString val = mGrid->GetCellValue(row, Col_Track);

   // User selected the "New..." choice so ask for a NEW name
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

      // Force generation of a NEW track name
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
   double t {};
   mGrid->GetCellValue(row, Col_Stime).ToDouble(&t);
   rd->selectedRegion.setT0(t, false);
   mGrid->SetCellValue(row, Col_Etime, wxString::Format(wxT("%g"),
                       rd->selectedRegion.t1()));

   return;
}

void LabelDialog::OnChangeEtime(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   double t {};
   mGrid->GetCellValue(row, Col_Etime).ToDouble(&t);
   rd->selectedRegion.setT1(t, false);
   mGrid->SetCellValue(row, Col_Stime, wxString::Format(wxT("%g"),
                       rd->selectedRegion.t0()));

   return;
}

void LabelDialog::OnChangeLfreq(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   double f;
   mGrid->GetCellValue(row, Col_Lfreq).ToDouble(&f);
   rd->selectedRegion.setF0(f, false);
   mGrid->SetCellValue(row, Col_Hfreq, wxString::Format(wxT("%g"),
                                                        rd->selectedRegion.f1()));

   return;
}

void LabelDialog::OnChangeHfreq(wxGridEvent & WXUNUSED(event), int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   double f;
   mGrid->GetCellValue(row, Col_Hfreq).ToDouble(&f);
   rd->selectedRegion.setF1(f, false);
   mGrid->SetCellValue(row, Col_Lfreq, wxString::Format(wxT("%g"),
                                                        rd->selectedRegion.f0()));
   
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
      auto editor = mGrid->GetCellEditor(mGrid->GetGridCursorRow(),
         mGrid->GetGridCursorCol());
      editor->Reset();
      // To avoid memory leak, don't forget DecRef()!
      editor->DecRef();
      mGrid->HideCellEditControl();
      return;
   }

   // Standard handling
   EndModal(wxID_CANCEL);

   return;
}
