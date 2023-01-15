/**********************************************************************

  Audacity: A Digital Audio Editor

  Equalization.h

  Mitch Golden
  Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/setup.h> // for wxUSE_* macros

#include "StatefulEffect.h"
#include "EqualizationUI.h"

class EffectEqualization : public StatefulEffect
{
public:
   static inline EqualizationParameters *
   FetchParameters(EffectEqualization &e, EffectSettings &)
   { return &e.mParameters; }
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualization(int Options = kEqLegacy);
   
   virtual ~EffectEqualization();

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

   bool ValidateUI(const EffectPlugin &plugin, EffectSettings &) const override;

   // Effect implementation

   bool Init() override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;

private:
   // EffectEqualization implementation

   bool ProcessOne(EffectContext &context, int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
   
   wxWeakRef<wxWindow> mUIParent{};
   EqualizationFilter mParameters;
   EqualizationCurvesList mCurvesList{ mParameters };
   const int mOptions;
   EqualizationUI mUI{ *this, mUIParent, GetName(), mCurvesList, mOptions };

   const EffectParameterMethods& Parameters() const override;
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
