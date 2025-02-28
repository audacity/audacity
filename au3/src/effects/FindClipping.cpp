/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.cpp

  Leland Lucius

*//****************************************************************//**

\class FindClippingDialog
\brief FindClippingDialog used with EffectFindClipping

*//*******************************************************************/
#include "FindClipping.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "AudacityMessageBox.h"

namespace {
BuiltinEffectsModule::Registration< EffectFindClipping > reg;
}

std::unique_ptr<EffectEditor> EffectFindClipping::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    DoPopulateOrExchange(S, access);
    return nullptr;
}

void EffectFindClipping::DoPopulateOrExchange(
    ShuttleGui& S, EffectSettingsAccess& access)
{
    mpAccess = access.shared_from_this();
    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S
        .Validator<IntegerValidator<int> >(
            &mStart, NumValidatorStyle::DEFAULT, Start.min)
        .TieTextBox(XXO("&Start threshold (samples):"), mStart, 10);

        S
        .Validator<IntegerValidator<int> >(
            &mStop, NumValidatorStyle::DEFAULT, Stop.min)
        .TieTextBox(XXO("St&op threshold (samples):"), mStop, 10);
    }
    S.EndMultiColumn();
}

bool EffectFindClipping::TransferDataToWindow(const EffectSettings&)
{
    ShuttleGui S(mUIParent, eIsSettingToDialog);
    // To do: eliminate this and just use validators for controls
    DoPopulateOrExchange(S, *mpAccess);

    return true;
}

bool EffectFindClipping::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate()) {
        return false;
    }

    ShuttleGui S(mUIParent, eIsGettingFromDialog);
    // To do: eliminate this and just use validators for controls
    DoPopulateOrExchange(S, *mpAccess);

    return true;
}
