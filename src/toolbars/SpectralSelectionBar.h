/**********************************************************************

Audacity: A Digital Audio Editor

SpectralSelectionBar.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRAL_SELECTION_BAR__
#define __AUDACITY_SPECTRAL_SELECTION_BAR__

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

class SpectralSelectionBarListener;
class NumericTextCtrl;

class SpectralSelectionBar :public ToolBar {

public:

   SpectralSelectionBar();
   virtual ~SpectralSelectionBar();

   void Create(wxWindow *parent);

   virtual void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};
   virtual void UpdatePrefs();

   void SetFrequencies(double bottom, double top);
   void SetFrequencySelectionFormatName(const wxString & formatName);
   void SetLogFrequencySelectionFormatName(const wxString & formatName);
   void SetListener(SpectralSelectionBarListener *l);

private:

   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnCenter(wxCommandEvent &evt);
   void OnWidth(wxCommandEvent &evt);

   void OnSize(wxSizeEvent &evt);

   void ModifySpectralSelection(bool done = false);

   SpectralSelectionBarListener * mListener;
   double mCenter, mWidth;

   NumericTextCtrl   *mCenterCtrl;
   NumericTextCtrl   *mWidthCtrl;

public:

   DECLARE_CLASS(SpectralSelectionBar);
   DECLARE_EVENT_TABLE();
};

#endif

