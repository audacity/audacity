/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "global/translation.h"

#include "log.h"

using namespace muse;
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

double AmplifyEffect::ratio() const
{
    return mRatio;
}

void AmplifyEffect::setRatio(double r)
{
    mRatio = r;
    LOGDA() << "mRatio: " << r;
}

au::effects::EffectMeta AmplifyEffect::meta()
{
    EffectMeta meta;
    meta.title = muse::mtrc("effects", "Amplify");
    meta.categoryId = BUILTIN_CATEGORY_ID;

    return meta;
}
