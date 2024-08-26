/**********************************************************************

  Audacity: A Digital Audio Editor

  RegisterBuiltinEffects.h

  Matthieu Hodgkinson

**********************************************************************/
#include "RegisterBuiltinEffects.h"
#include "Fade.h"
#include "LoadEffects.h"

void RegisterBuiltinEffects()
{
}

namespace
{
BuiltinEffectsModule::Registration<FadeIn> fadeIn;
BuiltinEffectsModule::Registration<FadeOut> fadeOut;
}
