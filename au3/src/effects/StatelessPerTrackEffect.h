/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file StatelessPerTrackEffect.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_STATELESS_PER_TRACK_EFFECT__
#define __AUDACITY_STATELESS_PER_TRACK_EFFECT__

#include "PerTrackEffect.h"
#include "BasicEffectUIServices.h"

class StatelessEffectUIServices
   : public BasicEffectUIServices
{
public:
   std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin &plugin,
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const override;

   //! Called only from PopulateUI, to add controls to effect panel
   /*!
    @return also returned from PopulateUI
    @post `result: result != nullptr`
    */
   virtual std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const = 0;
};

//! Simply the union of PerTrackEffect and StatelessEffectUIServices
class StatelessPerTrackEffect
   : public PerTrackEffect
   , public StatelessEffectUIServices
{
public:
   ~StatelessPerTrackEffect() override;
};

#endif
