/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitValidator.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitValidator.h"
#include "AudioUnitEffectOptionsDialog.h"
#include "AudioUnitInstance.h"
#include "AUControl.h"
#include <wx/sizer.h>
#include "../../ShuttleGui.h"
#include "../../widgets/wxPanelWrapper.h"

AudioUnitValidator::AudioUnitValidator(CreateToken,
   EffectUIClientInterface &effect,
   EffectSettingsAccess &access, AudioUnitInstance &instance,
   AUControl *pControl
)  : EffectUIValidator{ effect, access }
   , mInstance{ instance }
   , mEventListenerRef{ MakeListener() }
   , mpControl{ pControl }
{
   // Make the settings of the instance up to date before using it to
   // build a UI
   StoreSettingsToInstance(mAccess.Get());
}

AudioUnitValidator::~AudioUnitValidator()
{
   if (mpControl)
      mpControl->Close();
}

auto AudioUnitValidator::MakeListener()
   -> EventListenerPtr
{
   const auto unit = mInstance.GetAudioUnit();
   EventListenerPtr result;

   // Register a callback with the audio unit
   AUEventListenerRef eventListenerRef{};
   if (AUEventListenerCreate(AudioUnitValidator::EventListenerCallback,
      this,
      static_cast<CFRunLoopRef>( const_cast<void*>(
         GetCFRunLoopFromEventLoop(GetCurrentEventLoop()))),
      kCFRunLoopDefaultMode, 0.0, 0.0, &eventListenerRef))
      return nullptr;
   result.reset(eventListenerRef);

   // AudioUnitEvent is a struct with a discriminator field and a union
   AudioUnitEvent event{ kAudioUnitEvent_ParameterValueChange };
   // Initialize union member -- the ID (second field) reassigned later
   auto &parameter = event.mArgument.mParameter;
   parameter = AudioUnitUtils::Parameter{ unit, kAudioUnitScope_Global };

   // Register each parameter as something we're interested in
   if (auto &parameters = mInstance.GetParameters())
      for (const auto &ID : parameters) {
         parameter.mParameterID = ID;
         if (AUEventListenerAddEventType(result.get(), this, &event))
            return nullptr;
      }

   // Now set up the other union member
   event = { kAudioUnitEvent_PropertyChange };
   event.mArgument.mProperty = AudioUnitUtils::Property{
      unit, kAudioUnitProperty_Latency, kAudioUnitScope_Global };
   if (AUEventListenerAddEventType(result.get(), this, &event))
      return nullptr;

   return result;
}

bool AudioUnitValidator::UpdateUI()
{
   // Update parameter values in AudioUnit, and propagate to any listeners
   if (StoreSettingsToInstance(mAccess.Get())) {
      // See AUView::viewWillDraw
      if (mpControl)
         mpControl->ForceRedraw();
   
      // This will be the AudioUnit of a stateful instance, not of the effect
      Notify();

      return true;
   }
   return false;
}

bool AudioUnitValidator::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings &settings){
#if 0
      // This analogy with other generators doesn't seem to fit AudioUnits
      // How can we define the control mDuration?
      if (GetType() == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());
#endif
      FetchSettingsFromInstance(settings);
   });
   return true;
}

bool AudioUnitValidator::FetchSettingsFromInstance(EffectSettings &settings)
{
   return mInstance.FetchSettings(AudioUnitInstance::GetSettings(settings));
}

bool AudioUnitValidator::StoreSettingsToInstance(const EffectSettings &settings)
{
   return mInstance.StoreSettings(AudioUnitInstance::GetSettings(settings));
}

std::unique_ptr<EffectUIValidator> AudioUnitValidator::Create(
   EffectUIClientInterface &effect, ShuttleGui &S,
   const wxString &uiType,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   const auto parent = S.GetParent();
   // Cast is assumed to succeed because only this effect's own instances
   // are passed back by the framework
   auto &myInstance = dynamic_cast<AudioUnitInstance&>(instance);

   AUControl *pControl{};
   wxPanel *container{};
   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      wxASSERT(parent); // To justify safenew
      container = safenew wxPanelWrapper(parent, wxID_ANY);
      mainSizer->Add(container, 1, wxEXPAND);
      parent->SetSizer(mainSizer.release());
   }

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   if (uiType == BasicValue.MSGID().GET()) {
      if (!CreatePlain(mParent))
         return nullptr;
   }
   else
#endif
   {
      auto uControl = Destroy_ptr<AUControl>(safenew AUControl);
      if (!uControl)
         return nullptr;
      pControl = uControl.get();

      if (!pControl->Create(container, myInstance.GetComponent(),
         myInstance.GetAudioUnit(),
         uiType == FullValue.MSGID().GET()))
         return nullptr;

      {
         auto innerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

         innerSizer->Add(uControl.release(), 1, wxEXPAND);
         container->SetSizer(innerSizer.release());
      }

      parent->SetMinSize(wxDefaultSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
      wxEventLoop::SetBusyWaiting(true);
#endif
#endif
   }

   return std::make_unique<AudioUnitValidator>(
      CreateToken{}, effect, access, myInstance, pControl);
}

void AudioUnitValidator::Notify()
{
   AudioUnitParameter aup = {};
   aup.mAudioUnit = mInstance.GetAudioUnit();
   aup.mParameterID = kAUParameterListener_AnyParameter;
   aup.mScope = kAudioUnitScope_Global;
   aup.mElement = 0;
   AUParameterListenerNotify(NULL, NULL, &aup);
}

void AudioUnitValidator::EventListener(const AudioUnitEvent *inEvent,
   AudioUnitParameterValue inParameterValue)
{
   // Modify the instance and its workers
   mInstance.EventListener(inEvent, inParameterValue);
   // Fetch changed settings and send them to the framework
   // (Maybe we need a way to fetch just one changed setting, but this is
   // the easy way to write it)
   ValidateUI();
}

// static
void AudioUnitValidator::EventListenerCallback(void *inCallbackRefCon,
   void *inObject, const AudioUnitEvent *inEvent, UInt64 inEventHostTime,
   AudioUnitParameterValue inParameterValue)
{
   static_cast<AudioUnitValidator *>(inCallbackRefCon)
      ->EventListener(inEvent, inParameterValue);
}
#endif
