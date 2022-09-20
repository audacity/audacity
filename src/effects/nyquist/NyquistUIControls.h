/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistUIControls.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_UI_CONTROLS__
#define __AUDACITY_EFFECT_NYQUIST_UI_CONTROLS__

#include "NyquistControls.h"
#include "../Effect.h"

class AUDACITY_DLL_API NyquistEffectBase
   : public EffectWithSettings<NyquistSettings, StatefulEffect>
{
public:
   ~NyquistEffectBase() override;
   virtual bool RecoverParseTypeFailed() = 0;
};

struct NyquistUIControls : NyquistControls, wxEvtHandler {
   explicit NyquistUIControls(NyquistEffectBase &effect, Bindings &bindings)
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

   NyquistEffectBase &mEffect;
   Bindings &mBindings;
   bool mEnablePreview { true };
};
#endif
