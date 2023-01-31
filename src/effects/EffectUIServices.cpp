/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectUIServices.cpp

  Paul Licameli split from EffectPlugin.cpp

**********************************************************************/
#include "EffectUIServices.h"
#include "EffectPlugin.h"
#include "widgets/AudacityMessageBox.h"

EffectUIServices::~EffectUIServices() = default;

int EffectUIServices::MessageBox(const EffectPlugin &plugin,
   const TranslatableString& message,
   long style, const TranslatableString &titleStr)
{
   auto title = titleStr.empty()
      ? plugin.GetName()
      : XO("%s: %s").Format(plugin.GetName(), titleStr);
   return AudacityMessageBox(message, title, style);
}

