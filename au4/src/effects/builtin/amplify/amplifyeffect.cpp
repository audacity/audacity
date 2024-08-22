/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "global/translation.h"

using namespace au::effects;

AmplifyEffect::AmplifyEffect()
{
}

ComponentInterfaceSymbol AmplifyEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString AmplifyEffect::GetDescription() const
{
    return XO("Increases or decreases the volume of the audio you have selected");
}

ManualPageID AmplifyEffect::ManualPage() const
{
    return L"Amplify";
}

au::effects::EffectMeta AmplifyEffect::meta()
{
    EffectMeta meta;
    meta.title = muse::mtrc("effects", "Amplify");
    meta.categoryId = BUILTIN_CATEGORY_ID;
    meta.qmlUrl = "qrc:/builtin/amplify/AmplifyView.qml";

    return meta;
}
