/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.h

  Dominic Mazzoni
  Dmitry Vedenko

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR__
#define __AUDACITY_SELECTION_BAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "widgets/auStaticText.h"

#include "Observer.h"


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
   
   void OnSnapTo(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnIdle( wxIdleEvent &evt );

   void ModifySelection(int newDriver, bool done = false);
   void SelectionModeUpdated();

   void UpdateRate(double rate);

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
   wxChoice          *mSnapTo;

   wxString mLastValidText;

   Observer::Subscription mRateChangedSubscription;

 public:

   DECLARE_CLASS(SelectionBar)
   DECLARE_EVENT_TABLE()
};

#endif

