/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEditor.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEditor.h"
#include "AudioUnitEffectOptionsDialog.h"
#include "AudioUnitInstance.h"
#include "AUControl.h"
#include <wx/app.h>
#include <wx/sizer.h>
#include "ShuttleGui.h"
#include "wxPanelWrapper.h"

AudioUnitEditor::AudioUnitEditor(CreateToken,
   const EffectUIServices &effect,
   EffectSettingsAccess &access, AudioUnitInstance &instance,
   AUControl *pControl, bool isGraphical
)  : EffectEditor{ effect, access }
   , mInstance{ instance }
   , mEventListenerRef{ MakeListener() }
   , mpControl{ pControl }
   , mIsGraphical{ isGraphical }
{
   // Make the settings of the instance up to date before using it to
   // build a UI
   StoreSettingsToInstance(mAccess.Get());

   wxTheApp->Bind(wxEVT_IDLE, &AudioUnitEditor::OnIdle, this);
}

AudioUnitEditor::~AudioUnitEditor()
{
   if (mpControl)
      mpControl->Close();
}

auto AudioUnitEditor::MakeListener()
   -> EventListenerPtr
{
   const auto unit = mInstance.GetAudioUnit();
   EventListenerPtr result;

   // Register a callback with the audio unit
   AUEventListenerRef eventListenerRef{};
   if (AUEventListenerCreate(AudioUnitEditor::EventListenerCallback,
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
   // And bind the listener function to certain property changes
   for (auto type : {
      kAudioUnitProperty_Latency,
      kAudioUnitProperty_PresentPreset,
   }) {
      event.mArgument.mProperty = AudioUnitUtils::Property{
         unit, type, kAudioUnitScope_Global };
      if (AUEventListenerAddEventType(result.get(), this, &event))
         return nullptr;
   }

   return result;
}

bool AudioUnitEditor::UpdateUI()
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

bool AudioUnitEditor::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings &settings){
#if 0
      // This analogy with other generators doesn't seem to fit AudioUnits
      // How can we define the control mDuration?
      if (GetType() == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());
#endif
      FetchSettingsFromInstance(settings);
      return nullptr;
   });
   return true;
}

bool AudioUnitEditor::IsGraphicalUI()
{
   return mIsGraphical;
}

bool AudioUnitEditor::FetchSettingsFromInstance(EffectSettings &settings)
{
   return mInstance
      .FetchSettings(AudioUnitInstance::GetSettings(settings), true, true);
}

bool AudioUnitEditor::StoreSettingsToInstance(const EffectSettings &settings)
{
   return mInstance.StoreSettings(mInstance.mProcessor,
      AudioUnitInstance::GetSettings(settings));
}

std::unique_ptr<EffectEditor> AudioUnitEditor::Create(
   const EffectUIServices &effect, ShuttleGui &S,
   const wxString &uiType,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   const auto parent = S.GetParent();
   bool isGraphical = (uiType == FullValue.MSGID().GET());
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
         myInstance.GetAudioUnit(), isGraphical))
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

   return std::make_unique<AudioUnitEditor>(
      CreateToken{}, effect, access, myInstance, pControl, isGraphical);
}

void AudioUnitEditor::Notify()
{
   AudioUnitParameter aup = {};
   aup.mAudioUnit = mInstance.GetAudioUnit();
   aup.mParameterID = kAUParameterListener_AnyParameter;
   aup.mScope = kAudioUnitScope_Global;
   aup.mElement = 0;
   AUParameterListenerNotify(NULL, NULL, &aup);
}

void AudioUnitEditor::EventListener(const AudioUnitEvent *inEvent,
   AudioUnitParameterValue inParameterValue)
{
   // Modify the instance and its workers
   mInstance.EventListener(inEvent, inParameterValue);

   if (inEvent->mEventType == kAudioUnitEvent_ParameterValueChange) {
      constexpr AudioUnitParameterValue epsilon = 1e-6;

      auto it = mParameterValues.find(inEvent->mArgument.mParameter.mParameterID);

      // When the UI is opened - EventListener is called for each parameter
      // with the current value.
      if (it == mParameterValues.end())
         mParameterValues.insert(std::make_pair(inEvent->mArgument.mParameter.mParameterID, inParameterValue));
      else if (std::abs(it->second - inParameterValue) > epsilon)
      {
        it->second = inParameterValue;
        Publish({inEvent->mArgument.mParameter.mParameterID, inParameterValue});
      }

      const auto ID = inEvent->mArgument.mParameter.mParameterID;
      mToUpdate.emplace_back(ID, inParameterValue);
      mAccess.Set(mInstance.MakeMessage(ID, inParameterValue));
   }
   else if (inEvent->mEventType == kAudioUnitEvent_PropertyChange &&
      inEvent->mArgument.mProperty.mPropertyID ==
         kAudioUnitProperty_PresentPreset
   ) {
      ValidateUI();
   }
}

// static
void AudioUnitEditor::EventListenerCallback(void *inCallbackRefCon,
   void *inObject, const AudioUnitEvent *inEvent, UInt64 inEventHostTime,
   AudioUnitParameterValue inParameterValue)
{
   static_cast<AudioUnitEditor *>(inCallbackRefCon)
      ->EventListener(inEvent, inParameterValue);
}

void AudioUnitEditor::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   if (mToUpdate.size()) {
      mAccess.ModifySettings([&](EffectSettings &settings){
         // Reassign settings, so that there is "stickiness" when dialog is
         // closed and opened again
         auto &mySettings = AudioUnitInstance::GetSettings(settings);
         for (auto [ID, value] : mToUpdate)
            if (auto &pair = mySettings.values[ID]; pair.has_value())
               pair->second = value;
         return nullptr;
      });
      mToUpdate.clear();
   }
}
#endif
