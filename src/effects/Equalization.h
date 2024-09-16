/**********************************************************************

  Audacity: A Digital Audio Editor

  Equalization.h

  Mitch Golden
  Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/setup.h> // for wxUSE_* macros

#include "EqualizationUI.h"
#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"


class WaveChannel;

class EqualizationBase : public StatefulEffect
{
public:
   static inline EqualizationParameters *
   FetchParameters(EqualizationBase &e, EffectSettings &)
   { return &e.mParameters; }
   static const ComponentInterfaceSymbol Symbol;

   EqualizationBase(int Options = kEqLegacy);

   virtual ~EqualizationBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   bool VisitSettings(SettingsVisitor &visitor, EffectSettings &settings)
      override;
   bool VisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   OptionalMessage LoadFactoryDefaults(EffectSettings &settings)
      const override;
   OptionalMessage DoLoadFactoryDefaults(EffectSettings &settings);

   RegistryPaths GetFactoryPresets() const override;
   OptionalMessage LoadFactoryPreset(int id, EffectSettings &settings)
      const override;

   // Effect implementation

   bool Init() override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;

protected:
   // EqualizationBase implementation

   struct Task;
   bool ProcessOne(Task &task, int count, const WaveChannel &t,
      sampleCount start, sampleCount len);

   EqualizationFilter mParameters;
   EqualizationCurvesList mCurvesList{ mParameters };
   const int mOptions;

   const EffectParameterMethods& Parameters() const override;
};

class EffectEqualization :
    public EqualizationBase,
    public StatefulEffectUIServices
{
   using EqualizationBase::EqualizationBase;

   bool ValidateUI(const EffectPlugin& plugin, EffectSettings&) const override;

   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
      const EffectOutputs* pOutputs) override;
   bool TransferDataToWindow(const EffectSettings& settings) override;

private:
   wxWeakRef<wxWindow> mUIParent {};
   EqualizationUI mUI { *this, mUIParent, GetName(), mCurvesList, mOptions };
};

class EffectEqualizationCurve final : public EffectEqualization
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualizationCurve() : EffectEqualization( kEqOptionCurve ) {}
};

class EffectEqualizationGraphic final : public EffectEqualization
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualizationGraphic() : EffectEqualization( kEqOptionGraphic ) {}
};

#endif
