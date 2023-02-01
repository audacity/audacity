/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulEffectUIServices.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/
#ifndef __AUDACITY_STATEFUL_EFFECT_UI_SERVICES__
#define __AUDACITY_STATEFUL_EFFECT_UI_SERVICES__

#include "Effect.h"
#include <wx/event.h>

class StatefulEffectUIServices
   : public wxEvtHandler
   , public BasicEffectUIServices
{
public:
   ~StatefulEffectUIServices() override;

   //! Allows PopulateOrExchange to return null
   std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin &plugin,
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const override;

   //! Add controls to effect panel; always succeeds
   /*!
    @return if not null, then return it from PopulateUI instead of a
    DefaultEffectEditor; default implementation returns null
    */
   virtual std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs);
};

#endif
