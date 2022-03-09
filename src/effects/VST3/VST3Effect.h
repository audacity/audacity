/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#pragma once

#include <wx/wx.h>

#include <pluginterfaces/gui/iplugview.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <public.sdk/source/vst/hosting/module.h>

#include "EffectInterface.h"
#include "internal/ComponentHandler.h"

#include "SampleCount.h"

class NumericTextCtrl;

namespace Steinberg
{
   namespace Vst
   {
      class IComponent;
      class IEditController;
      class IConnectionPoint;
   }
}

class ParameterChangesProvider;

/**
 * \brief Objects of this class connect Audacity with VST3 effects
 */
class VST3Effect final : public EffectUIClientInterface
{
   //Keep strong reference to a module while effect is alive
   std::shared_ptr<VST3::Hosting::Module> mModule;

   //Following fields are unique to each effect instance

   Steinberg::IPtr<Steinberg::Vst::IComponent> mEffectComponent;
   Steinberg::IPtr<Steinberg::Vst::IAudioProcessor> mAudioProcessor;
   Steinberg::Vst::ProcessSetup mSetup;
   const VST3::Hosting::ClassInfo mEffectClassInfo;

   //Since all of the realtime processors share same presets, following
   //fields are only initialized and assigned in the global effect instance

   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mComponentConnectionProxy;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mControllerConnectionProxy;
   Steinberg::IPtr<Steinberg::IPlugView> mPlugView;
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<internal::ComponentHandler> mComponentHandler;
   wxWindow* mParent { nullptr };
   EffectHostInterface *mEffectHost{};
   NumericTextCtrl* mDuration { nullptr };

   //Holds pending parameter changes to be applied to multiple realtime effects.
   //Not used in the "offline" mode
   internal::ComponentHandler::PendingChangesPtr mPendingChanges;

   std::vector<std::shared_ptr<VST3Effect>> mRealtimeGroupProcessors;

   // Mutable cache fields computed once on demand
   mutable bool mRescanFactoryPresets { true };
   mutable RegistryPaths mFactoryPresets;

   size_t mUserBlockSize { 8192 };
   bool mUseLatency { true };
   sampleCount mInitialDelay { 0 };


   void Initialize();
   
public:

   static EffectFamilySymbol GetFamilySymbol();

   VST3Effect(
      std::shared_ptr<VST3::Hosting::Module> module,
      VST3::Hosting::ClassInfo effectClassInfo);

   VST3Effect(const VST3Effect& other);

   ~VST3Effect() override;

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

   EffectType GetType() const override;
   EffectFamilySymbol GetFamily() const override;
   bool IsInteractive() const override;
   bool IsDefault() const override;
   bool SupportsRealtime() const override;
   bool SupportsAutomation() const override;
   bool GetAutomationParameters(CommandParameters& parms) const override;
   bool SetAutomationParameters(const CommandParameters& parms) override;
   bool LoadUserPreset(const RegistryPath& name) override;
   bool SaveUserPreset(
      const RegistryPath & name, const Settings &settings) const override;
   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   int GetMidiInCount() override;
   int GetMidiOutCount() override;
   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;
   sampleCount GetLatency() override;
   size_t GetTailSize() override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings) override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() noexcept override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(int group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

   int ShowClientInterface(wxWindow& parent, wxDialog& dialog, bool forceModal) override;
   bool InitializePlugin();
   bool InitializeInstance(EffectHostInterface* host) override;
   bool IsGraphicalUI() override;
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectSettingsAccess &access) override;
   bool ValidateUI(EffectSettings &) override;
   bool CloseUI() override;
   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;
   bool HasOptions() override;
   void ShowOptions() override;

private:
   void OnEffectWindowResize(wxSizeEvent & evt);

   bool LoadVSTUI(wxWindow* parent);

   void SyncParameters();

   bool LoadPreset(const wxString& path);

   void ReloadUserOptions();
};
