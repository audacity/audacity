/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectPreview.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from EffectBase.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_PREVIEW__
#define __AUDACITY_EFFECT_PREVIEW__

#include <functional>

class EffectBase;
class EffectSettingsAccess;

//! Calculate temporary tracks of limited length with effect applied and play
/*!
 @param updateUI called after adjusting temporary settings and before play
 */
void EffectPreview(EffectBase& effect, EffectSettingsAccess& access, std::function<void()> updateUI, bool dryOnly);

#endif
