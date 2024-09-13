/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

class LabelTrack;
class WaveChannel;

#include "ShuttleAutomation.h"
#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class FindClippingBase : public StatefulEffect
{
public:
   static inline FindClippingBase *
   FetchParameters(FindClippingBase &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   FindClippingBase();
   virtual ~FindClippingBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool Process(EffectInstance &instance, EffectSettings &settings) override;

private:
   // EffectFindCliping implementation

   bool ProcessOne(LabelTrack &lt, int count, const WaveChannel &wt,
      sampleCount start, sampleCount len);

protected:
   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
   // To do: eliminate this

   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Start{ &FindClippingBase::mStart,
   L"Duty Cycle Start",  3,    1,    INT_MAX, 1   };
static constexpr EffectParameter Stop{ &FindClippingBase::mStop,
   L"Duty Cycle End",    3,    1,    INT_MAX, 1   };
};

class EffectFindClipping :
    public FindClippingBase,
    public StatefulEffectUIServices
{
public:
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
      const EffectOutputs* pOutputs) override;
   void DoPopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access);
   bool TransferDataToWindow(const EffectSettings& settings) override;
   bool TransferDataFromWindow(EffectSettings& settings) override;

private:
   wxWeakRef<wxWindow> mUIParent;
   EffectSettingsAccessPtr mpAccess;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
