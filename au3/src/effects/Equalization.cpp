/**********************************************************************

   Audacity: A Digital Audio Editor

   Equalization.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

*******************************************************************//**

   \file Equalization.cpp
   \brief Implements EffectEqualization.

*//****************************************************************//**


   \class EffectEqualization
   \brief An Effect that modifies volume in different frequency bands.

   Performs filtering, using an FFT to do a FIR filter.
   It lets the user draw an arbitrary envelope (using the same
   envelope editing code that is used to edit the track's
   amplitude envelope).

   Also allows the curve to be specified with a series of 'graphic EQ'
   sliders.

   The filter is applied using overlap/add of Hann windows.

   Clone of the FFT Filter effect, no longer part of Audacity.

*//*******************************************************************/
#include "Equalization.h"
#include "EqualizationUI.h"
#include "EffectEditor.h"
#include "LoadEffects.h"
#include "ShuttleGui.h"

#ifdef LEGACY_EQ
namespace {
BuiltinEffectsModule::Registration< EffectEqualization > reg;
}
#endif

// "Filter Curve EQ" in the user-facing string, but preserve the old
// internal string
const ComponentInterfaceSymbol EffectEqualizationCurve::Symbol
{ wxT("Filter Curve"), XO("Filter Curve EQ") };

namespace {
BuiltinEffectsModule::Registration< EffectEqualizationCurve > reg2;
}

const ComponentInterfaceSymbol EffectEqualizationGraphic::Symbol
{ wxT("Graphic EQ"), XO("Graphic EQ") };

namespace {
BuiltinEffectsModule::Registration< EffectEqualizationGraphic > reg3;
}

ComponentInterfaceSymbol EffectEqualization::GetSymbol() const
{
    if (mOptions == kEqOptionGraphic) {
        return EffectEqualizationGraphic::Symbol;
    }
    if (mOptions == kEqOptionCurve) {
        return EffectEqualizationCurve::Symbol;
    }
    return EqualizationBase::Symbol;
}

bool EffectEqualization::ValidateUI(
    const EffectPlugin&, EffectSettings& settings) const
{
    // Stateful effect still cheats const_cast!
    return const_cast<EffectEqualization&>(*this).mUI.ValidateUI(settings);
}

std::unique_ptr<EffectEditor> EffectEqualization::PopulateOrExchange(
    ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
    const EffectOutputs* pOutputs)
{
    mUIParent = S.GetParent();
    return mUI.PopulateOrExchange(S, instance, access, pOutputs);
}

//
// Populate the window with relevant variables
//
bool EffectEqualization::TransferDataToWindow(const EffectSettings& settings)
{
    return mUI.TransferDataToWindow(settings);
}
