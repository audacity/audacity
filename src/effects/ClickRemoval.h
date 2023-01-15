/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.h

  Craig DeForest

  (Structure largely stolen from NoiseRemoval.h by Dominic Mazzoni)

  This file is intended to become part of Audacity.  You may modify and/or
  distribute it under the same terms as Audacity itself.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CLICK_REMOVAL__
#define __AUDACITY_EFFECT_CLICK_REMOVAL__

#include "StatefulEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxTextCtrl;
class Envelope;
class ShuttleGui;

class EffectClickRemoval final : public StatefulEffect
{
public:
   static inline EffectClickRemoval *
   FetchParameters(EffectClickRemoval &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   bool ProcessOne(EffectContext &context,
      int count, WaveTrack * track, sampleCount start, sampleCount len);

   bool RemoveClicks(size_t len, float *buffer);

   void OnWidthText(wxCommandEvent & evt);
   void OnThreshText(wxCommandEvent & evt);
   void OnWidthSlider(wxCommandEvent & evt);
   void OnThreshSlider(wxCommandEvent & evt);

private:
   wxWeakRef<wxWindow> mUIParent{};

   Envelope *mEnvelope;

   bool mbDidSomething; // This effect usually does nothing on real-world data.
   size_t windowSize;
   int mThresholdLevel;
   int mClickWidth;
   int sep;

   wxSlider *mWidthS;
   wxSlider *mThreshS;
   wxTextCtrl *mWidthT;
   wxTextCtrl *mThreshT;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Threshold{ &EffectClickRemoval::mThresholdLevel,
   L"Threshold",  200,     0,       900,     1  };
static constexpr EffectParameter Width{ &EffectClickRemoval::mClickWidth,
   L"Width",      20,      0,       40,      1  };
};

#endif
