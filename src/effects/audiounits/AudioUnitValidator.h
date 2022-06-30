/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitValidator.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_VALIDATOR__
#define __AUDACITY_AUDIO_UNIT_VALIDATOR__

#include <AudioToolbox/AudioUnitUtilities.h>
#include "AudioUnitUtils.h"
#include "EffectInterface.h"

class AUControl;

class AudioUnitInstance;

class AudioUnitValidator : public EffectUIValidator {
   struct CreateToken{};
public:
   static std::unique_ptr<EffectUIValidator> Create(
      EffectUIClientInterface &effect, ShuttleGui &S,
      const wxString &uiType,
      EffectInstance &instance, EffectSettingsAccess &access);

   AudioUnitValidator(CreateToken,
      EffectUIClientInterface &effect, EffectSettingsAccess &access,
      AudioUnitInstance &instance, AUControl *pControl);

   ~AudioUnitValidator() override;

   bool UpdateUI() override;
   bool ValidateUI() override;

private:
   static void EventListenerCallback(void *inCallbackRefCon,
      void *inObject, const AudioUnitEvent *inEvent,
      UInt64 inEventHostTime, AudioUnitParameterValue inParameterValue);
   void EventListener(const AudioUnitEvent *inEvent,
      AudioUnitParameterValue inParameterValue);
   bool FetchSettingsFromInstance(EffectSettings &settings);
   bool StoreSettingsToInstance(const EffectSettings &settings);

   void Notify();

   using EventListenerPtr =
      AudioUnitCleanup<AUEventListenerRef, AUListenerDispose>;

   EventListenerPtr MakeListener();

   // The lifetime guarantee is assumed to be provided by the instance.
   // See contract of PopulateUI
   AudioUnitInstance &mInstance;
   const EventListenerPtr mEventListenerRef;
   AUControl *const mpControl{};
};
#endif
