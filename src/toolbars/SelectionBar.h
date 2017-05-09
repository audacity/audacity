/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR__
#define __AUDACITY_SELECTION_BAR__

#include <wx/defs.h>

#include "ToolBar.h"

class wxBitmap;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxCommandEvent;
class wxDC;
class wxRadioButton;
class wxSizeEvent;

class SelectionBarListener;
class NumericTextCtrl;

class SelectionBar final : public ToolBar {

 public:

   SelectionBar();
   virtual ~SelectionBar();

   void Create(wxWindow *parent);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;

   void SetTimes(double start, double end, double audio);
   double GetLeftTime();
   double GetRightTime();
   void SetField(const wxChar *msg, int fieldNum);
   void SetSnapTo(int);
   void SetSelectionFormat(const wxString & format);
   void SetRate(double rate);
   void SetListener(SelectionBarListener *l);
   void RegenerateTooltips() override;

 private:
   wxRadioButton * AddRadioButton( const wxString & Name, int id, 
      std::unique_ptr<wxBoxSizer>& pSizer, long style);

   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnLeftTime(wxCommandEvent &evt);
   void OnRightTime(wxCommandEvent &evt);

   void OnStartRadio(wxCommandEvent &evt);
   void OnCenterRadio(wxCommandEvent &evt);
   void OnEndRadio(wxCommandEvent &evt);
   void OnLengthRadio(wxCommandEvent &evt);

   void OnRate(wxCommandEvent & event);

   void OnSnapTo(wxCommandEvent & event);

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnSize(wxSizeEvent &evt);

   void ModifySelection(bool done = false);

   void UpdateRates();

   SelectionBarListener * mListener;
   double mRate;
   double mStart, mEnd, mAudio;
   wxString mField[10];

   bool mbUseNativeRadioButton;

   NumericTextCtrl   *mLeftTime;
   NumericTextCtrl   *mRightTime;
   wxRadioButton  *mSelStartButton;   // for start/center
   wxRadioButton  *mSelEndButton;     // for end/length
   NumericTextCtrl   *mAudioTime;

   wxComboBox     *mRateBox;
   wxChoice       *mSnapTo;

   wxWindow       *mRateText;

 public:

   DECLARE_CLASS(SelectionBar)
   DECLARE_EVENT_TABLE()
};

#endif

