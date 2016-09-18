/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LABELDIALOG__
#define __AUDACITY_LABELDIALOG__

#include <vector>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/grid.h>
#include <wx/string.h>

#include "Internat.h"
#include "widgets/Grid.h"
#include "widgets/wxPanelWrapper.h"

class TrackFactory;
class TrackList;
class RowData;
class EmptyLabelRenderer;
class LabelTrack;
class ViewInfo;

typedef std::vector<RowData> RowDataArray;

class LabelDialog final : public wxDialogWrapper
{
 public:

   LabelDialog(wxWindow *parent,
               TrackFactory &factory,
               TrackList *tracks,

               // if NULL edit all tracks, else this one only:
               LabelTrack *selectedTrack,

               // This is nonnegative only if selectedTrack is not NULL
               // and is the unique label to edit
               int index,

               ViewInfo &viewinfo,
               double rate,
               const wxString & format, const wxString &freqFormat);
   ~LabelDialog();

    bool Show(bool show = true) override;

 private:

   bool TransferDataToWindow();
   bool TransferDataFromWindow();
   bool Validate();
   void FindAllLabels();
   void AddLabels(const LabelTrack *t);
   void FindInitialRow();
   wxString TrackName(int & index, const wxString &dflt = _("Label Track"));

   void OnUpdate(wxCommandEvent &event);
   void OnFreqUpdate(wxCommandEvent &event);
   void OnInsert(wxCommandEvent &event);
   void OnRemove(wxCommandEvent &event);
   void OnImport(wxCommandEvent &event);
   void OnExport(wxCommandEvent &event);
   void OnSelectCell(wxGridEvent &event);
   void OnCellChange(wxGridEvent &event);
   void OnChangeTrack(wxGridEvent &event, int row, RowData *rd);
   void OnChangeLabel(wxGridEvent &event, int row, RowData *rd);
   void OnChangeStime(wxGridEvent &event, int row, RowData *rd);
   void OnChangeEtime(wxGridEvent &event, int row, RowData *rd);
   void OnChangeLfreq(wxGridEvent &event, int row, RowData *rd);
   void OnChangeHfreq(wxGridEvent &event, int row, RowData *rd);
   void OnOK(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);

 private:

   Grid *mGrid;
   ChoiceEditor *mChoiceEditor;
   NumericEditor *mTimeEditor;
   NumericEditor *mFrequencyEditor;

   RowDataArray mData;

   TrackFactory &mFactory;
   TrackList *mTracks;
   LabelTrack *mSelectedTrack {};
   int mIndex { -1 };
   ViewInfo *mViewInfo;
   wxArrayString mTrackNames;
   double mRate;
   wxString mFormat;
   wxString mFreqFormat;

   int mInitialRow;

   DECLARE_EVENT_TABLE()
};

#endif
