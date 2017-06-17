/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TIME_TOOLBAR__
#define __AUDACITY_TIME_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"

// PLAIN_TITLES give Start Length Center End above each field.
// RADIO_TITLES give ()SE (*)Start-Length ()LE ()LC style.
// BUTTON_TITLES give    < Start - Length >  style.
// CHOICE gives a choice control
//#define SEL_RADIO_TITLES
//#define SEL_BUTTON_TITLES
#define SEL_CHOICE

// OPTIONS_BUTTON gives a button with three dots to select the option.

// Column for 
//   Project rate
//   Snap To
//   Option Button
//   Vertical Line
//   Selection fields
//   Vertical Line
//   Cursor position

class wxBitmap;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxCommandEvent;
class wxDC;
class wxRadioButton;
class wxSizeEvent;
class wxStaticText;

class TimeToolbarListener;
class NumericTextCtrl;

enum
{
   numTimeToolbarButtons = 1,
};

class TimeToolbar final : public ToolBar {

 public:
   TimeToolbar();
   virtual ~TimeToolbar();

   void Create(wxWindow *parent);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;

   void SetTimes(double audio);
   void SetField(const wxChar *msg, int fieldNum);
   void SetSelectionFormat(const wxString & format);
   void SetRate(double rate);
   void SetListener(TimeToolbarListener *l);
   void RegenerateTooltips() override;

 private:
   NumericTextCtrl * AddTime( const wxString Name, int id, wxSizer * pSizer );

   void SetSelectionMode(int mode);
   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnChangedTime(wxCommandEvent &evt);

   void OnRate(wxCommandEvent & event);
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnFieldChoice(wxCommandEvent &event);

   void ModifySelection(int newDriver, bool done = false);
   void UpdateRates();
   void SelectionModeUpdated();

   TimeToolbarListener * mListener;
   double mRate;
   double mAudio;
   wxString mField[10];

   NumericTextCtrl   *mAudioTime;

   wxStaticText * mProxy;

   AButton * mButtons[numTimeToolbarButtons];

 public:

   DECLARE_CLASS(TimeToolbar)
   DECLARE_EVENT_TABLE()
};

#endif

