/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR__
#define __AUDACITY_SELECTION_BAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "auStaticText.h"

// Column for 
//   Project rate
//   Snap To
//   Option Button
//   Vertical Line
//   Selection fields
//   Vertical Line
//   Cursor position

#ifdef TIME_IN_SELECT_TOOLBAR
#define SIZER_COLS 7
#else
#define SIZER_COLS 5
#endif


class wxChoice;
class wxComboBox;
class wxCommandEvent;
class wxDC;
class wxSizeEvent;
class wxStaticText;

class AudacityProject;
class SelectionBarListener;
class NumericTextCtrl;

class AUDACITY_DLL_API SelectionBar final : public ToolBar {

 public:
   static Identifier ID();

   SelectionBar( AudacityProject &project );
   virtual ~SelectionBar();

   bool ShownByDefault() const override;
   DockID DefaultDockID() const override;

   static SelectionBar &Get( AudacityProject &project );
   static const SelectionBar &Get( const AudacityProject &project );

   void Create(wxWindow *parent) override;

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;

   void SetTimes(double start, double end, double audio);
   void SetSnapTo(int);
   void SetSelectionFormat(const NumericFormatSymbol & format);
   void SetRate(double rate);
   void SetListener(SelectionBarListener *l);
   void RegenerateTooltips() override;

 private:
   auStaticText * AddTitle( const TranslatableString & Title,
      wxSizer * pSizer );
   NumericTextCtrl * AddTime( const TranslatableString &Name, int id, wxSizer * pSizer );
   void AddVLine(  wxSizer * pSizer );

   void SetSelectionMode(int mode);
   void ShowHideControls(int mode);
   void SetDrivers( int driver1, int driver2 );
   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnChangedTime(wxCommandEvent &evt);

   void OnRate(wxCommandEvent & event);
   void OnSnapTo(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnIdle( wxIdleEvent &evt );

   void ModifySelection(int newDriver, bool done = false);
   void UpdateRates();
   void SelectionModeUpdated();

   SelectionBarListener * mListener;
   double mRate;
   double mStart, mEnd, mLength, mCenter,  mAudio;

   // These two numbers say which two controls 
   // drive the other two.
   int mDrive1;
   int mDrive2;

   int mSelectionMode{ 0 };
   int mLastSelectionMode{ 0 };

   NumericTextCtrl   *mStartTime;
   NumericTextCtrl   *mCenterTime;
   NumericTextCtrl   *mLengthTime;
   NumericTextCtrl   *mEndTime;
   NumericTextCtrl   *mAudioTime;
   wxChoice          *mChoice;
   wxStaticText      *mProxy;
   wxComboBox        *mRateBox;
   wxChoice          *mSnapTo;
   wxWindow          *mRateText;

   wxString mLastValidText;

 public:

   DECLARE_CLASS(SelectionBar)
   DECLARE_EVENT_TABLE()
};

#endif

