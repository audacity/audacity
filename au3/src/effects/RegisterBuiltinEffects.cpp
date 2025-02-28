/**********************************************************************

  Audacity: A Digital Audio Editor

  RegisterBuiltinEffects.h

  Matthieu Hodgkinson

**********************************************************************/
#include "RegisterBuiltinEffects.h"
#include "Fade.h"
#include "Invert.h"
#include "LoadEffects.h"
#include "Repair.h"
#include "Reverse.h"
#include "StereoToMono.h"

void RegisterBuiltinEffects()
{
}

namespace {
BuiltinEffectsModule::Registration<FadeIn> fadeIn;
BuiltinEffectsModule::Registration<FadeOut> fadeOut;
BuiltinEffectsModule::Registration<Reverse> reverse;
BuiltinEffectsModule::Registration<StereoToMono> stereoToMono;
BuiltinEffectsModule::Registration<Repair> repair;
BuiltinEffectsModule::Registration<Invert> invert;
} // namespace
