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

class TimeTextCtrl;

class AUDACITY_DLL_API SelectionBarListener {

 public:

   SelectionBarListener(){};
   virtual ~SelectionBarListener(){};

   virtual double AS_GetRate() = 0;
   virtual void AS_SetRate(double rate) = 0;
   virtual int AS_GetSnapTo() = 0;
   virtual void AS_SetSnapTo(int snap) = 0;
   virtual const wxString & AS_GetSelectionFormat() = 0;
   virtual void AS_SetSelectionFormat(const wxString & format) = 0;
   virtual void AS_ModifySelection(double &start, double &end, bool done) = 0;
};

class SelectionBar:public ToolBar {

 public:

   SelectionBar();
   virtual ~SelectionBar();

   void Create(wxWindow *parent);

   virtual void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};
   virtual void UpdatePrefs();

   void SetTimes(double start, double end, double audio);
   double GetLeftTime();
   double GetRightTime();
   void SetField(const wxChar *msg, int fieldNum);
   void SetSnapTo(int);
   void SetSelectionFormat(const wxString & format);
   void SetRate(double rate);
   void SetListener(SelectionBarListener *l);

 private:

   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnLeftTime(wxCommandEvent &evt);
   void OnRightTime(wxCommandEvent &evt);

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

   TimeTextCtrl   *mLeftTime;
   TimeTextCtrl   *mRightTime;
   wxRadioButton  *mRightEndButton;
   wxRadioButton  *mRightLengthButton;
   TimeTextCtrl   *mAudioTime;

   wxComboBox     *mRateBox;
   wxChoice       *mSnapTo;

   wxWindow       *mRateText;

 public:

   DECLARE_CLASS(SelectionBar);
   DECLARE_EVENT_TABLE();
};

#endif

