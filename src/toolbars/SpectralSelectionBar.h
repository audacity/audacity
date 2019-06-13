/**********************************************************************

Audacity: A Digital Audio Editor

SpectralSelectionBar.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRAL_SELECTION_BAR__
#define __AUDACITY_SPECTRAL_SELECTION_BAR__

#include <wx/defs.h>

#include "ToolBar.h"

class wxChoice;
class wxComboBox;
class wxCommandEvent;
class wxDC;
class wxSizeEvent;

class AudacityProject;
class SpectralSelectionBarListener;
class NumericTextCtrl;

class SpectralSelectionBar final : public ToolBar {

public:

   SpectralSelectionBar( AudacityProject &project );
   virtual ~SpectralSelectionBar();

   static SpectralSelectionBar &Get( AudacityProject &project );
   static const SpectralSelectionBar &Get( const AudacityProject &project );

   void Create(wxWindow *parent) override;

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;

   void SetFrequencies(double bottom, double top);
   void SetFrequencySelectionFormatName(const NumericFormatSymbol & formatName);
   void SetBandwidthSelectionFormatName(const NumericFormatSymbol & formatName);
   void SetListener(SpectralSelectionBarListener *l);

   void RegenerateTooltips() override {};

private:

   void ValuesToControls();
   void SetBounds();
   void OnUpdate(wxCommandEvent &evt);
   void OnCtrl(wxCommandEvent &evt);
   void OnChoice(wxCommandEvent &evt);
   void OnIdle( wxIdleEvent &evt );

   void OnSize(wxSizeEvent &evt);

   void ModifySpectralSelection(bool done = false);

   SpectralSelectionBarListener * mListener;

   bool mbCenterAndWidth;

   double mCenter; // hertz
   double mWidth; // logarithm of ratio of hertz
   double mLow; // hertz
   double mHigh; // hertz

   NumericTextCtrl *mCenterCtrl, *mWidthCtrl, *mLowCtrl, *mHighCtrl;
   wxChoice *mChoice;

   int mHeight;   // height of main sizer after creation - used by OnChoice()

public:

   DECLARE_CLASS(SpectralSelectionBar)
   DECLARE_EVENT_TABLE()
};

#endif

