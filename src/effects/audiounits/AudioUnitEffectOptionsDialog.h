/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectOptionsDialog.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#ifndef __AUDACITY_AUDIOUNIT_EFFECT_OPTIONS_DIALOG__
#define __AUDACITY_AUDIOUNIT_EFFECT_OPTIONS_DIALOG__

#include "wxPanelWrapper.h"

class EffectDefinitionInterface;
class ShuttleGui;

constexpr auto UITypeKey = L"UIType";

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectOptionsDialog
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectOptionsDialog final : public wxDialogWrapper
{
public:
   explicit AudioUnitEffectOptionsDialog(
      const EffectDefinitionInterface &effect);
   virtual ~AudioUnitEffectOptionsDialog();
   void PopulateOrExchange(ShuttleGui & S);
   void OnOk(wxCommandEvent & evt);
private:
   const EffectDefinitionInterface &mEffect;
   bool mUseLatency;
   TranslatableString mUITypeString;
   DECLARE_EVENT_TABLE()
};

static const auto FullValue = XO("Full");
static const auto GenericValue = XO("Generic");
static const auto BasicValue = XO("Basic");

#endif
