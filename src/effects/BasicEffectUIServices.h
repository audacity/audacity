/**********************************************************************

  Audacity: A Digital Audio Editor

  @file BasicEffectUIServices.h
  @brief decorator of a Sink with a non-time-warping effect

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/
#ifndef __AUDACITY_BASIC_EFFECT_UI_SERVICES__
#define __AUDACITY_BASIC_EFFECT_UI_SERVICES__

#include "EffectUIServices.h"

//! Supplies implementations of all pure virtual functions of the base class
//! except PopulateUI
class BasicEffectUIServices
   : public EffectUIServices
{
public:
   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;
   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;
   void ShowOptions(const EffectPlugin &plugin) const override;
   bool ValidateUI(const EffectPlugin &context, EffectSettings &)
      const override;
   bool CloseUI() const override;
};
#endif
