/**********************************************************************

 Audacity: A Digital Audio Editor

 @file StatelessPerTrackEffect.cpp

 Paul Licameli

 **********************************************************************/
#include "StatelessPerTrackEffect.h"
#include "EffectEditor.h"
#include "ShuttleGui.h"
#include <wx/sizer.h>

std::unique_ptr<EffectEditor> StatelessEffectUIServices::PopulateUI(
    const EffectPlugin&, ShuttleGui& S, EffectInstance& instance,
    EffectSettingsAccess& access, const EffectOutputs* pOutputs) const
{
    auto parent = S.GetParent();

    // Subclass must provide something
    auto result = MakeEditor(S, instance, access, pOutputs);
    assert(result);

    parent->SetMinSize(parent->GetSizer()->GetMinSize());

    return result;
}

StatelessPerTrackEffect::~StatelessPerTrackEffect() = default;
