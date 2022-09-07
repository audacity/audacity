/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistUIControls.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_UI_CONTROLS__
#define __AUDACITY_EFFECT_NYQUIST_UI_CONTROLS__

#include "NyquistControls.h"

class Effect;

struct NyquistUIControls : NyquistControls, wxEvtHandler {
   explicit NyquistUIControls(Effect &effect, Bindings &bindings)
      : mEffect{ effect }
      , mBindings{ bindings }
   {}
   virtual ~NyquistUIControls();
   bool UpdateUI();
   bool ValidateUI();
   void Populate(ShuttleGui &S,
      const NumericFormatSymbol &selectionFormat, double projectRate);

   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnTime(wxCommandEvent & evt);
   void OnFileButton(wxCommandEvent & evt);

   bool validatePath(wxString path);
   wxString ToTimeFormat(double t);

   Effect &mEffect;
   Bindings &mBindings;
   bool mEnablePreview { true };
};
#endif
