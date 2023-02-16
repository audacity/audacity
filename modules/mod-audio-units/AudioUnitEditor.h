/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEditor.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_EDITOR__
#define __AUDACITY_AUDIO_UNIT_EDITOR__

#include <AudioToolbox/AudioUnitUtilities.h>
#include <unordered_map>
#include "AudioUnitUtils.h"
#include "EffectEditor.h"

class AUControl;

class AudioUnitInstance;
class EffectSettings;
class EffectInstance;
class ShuttleGui;

class AudioUnitEditor : public wxEvtHandler, public EffectEditor {
   struct CreateToken{};
public:
   static std::unique_ptr<EffectEditor> Create(
      const EffectUIServices &effect, ShuttleGui &S,
      const wxString &uiType,
      EffectInstance &instance, EffectSettingsAccess &access);

   AudioUnitEditor(CreateToken,
      const EffectUIServices &effect, EffectSettingsAccess &access,
      AudioUnitInstance &instance, AUControl *pControl, bool isGraphical);

   ~AudioUnitEditor() override;

   bool UpdateUI() override;
   bool ValidateUI() override;
   bool IsGraphicalUI() override;

private:
   static void EventListenerCallback(void *inCallbackRefCon,
      void *inObject, const AudioUnitEvent *inEvent,
      UInt64 inEventHostTime, AudioUnitParameterValue inParameterValue);
   void EventListener(const AudioUnitEvent *inEvent,
      AudioUnitParameterValue inParameterValue);
   void OnIdle(wxIdleEvent &evt);
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
   std::vector<
      std::pair<AudioUnitParameterID, AudioUnitParameterValue>> mToUpdate;
   const bool mIsGraphical;

   // The map of parameter IDs to their current values
   std::unordered_map<AudioUnitParameterID, AudioUnitParameterValue>
      mParameterValues;
};
#endif
