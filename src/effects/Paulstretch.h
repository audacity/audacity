/**********************************************************************

   Audacity: A Digital Audio Editor
   Paulstretch.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/

#ifndef __AUDACITY_EFFECT_PAULSTRETCH__
#define __AUDACITY_EFFECT_PAULSTRETCH__

#include "Effect.h"
#include "ShuttleAutomation.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

class EffectPaulstretch final : public StatefulEffect
{
public:
   static inline EffectPaulstretch *
   FetchParameters(EffectPaulstretch &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectPaulstretch();
   virtual ~EffectPaulstretch();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   double CalcPreviewInputLength(
      const EffectSettings &settings, double previewLength) const override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectPaulstretch implementation
   
   void OnText(wxCommandEvent & evt);
   size_t GetBufferSize(double rate) const;

   bool ProcessOne(WaveTrack *track, double t0, double t1, int count);

   wxWeakRef<wxWindow> mUIParent;

   float mAmount;
   float mTime_resolution;  //seconds
   double m_t1;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Amount{ &EffectPaulstretch::mAmount,
   L"Stretch Factor",   10.0f,    1.0,     FLT_MAX, 1   };
static constexpr EffectParameter Time{ &EffectPaulstretch::mTime_resolution,
   L"Time Resolution",  0.25f,   0.00099f,  FLT_MAX, 1   };
};

#endif

