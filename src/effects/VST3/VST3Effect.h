/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#pragma once

#include <wx/wx.h>

#include <pluginterfaces/gui/iplugview.h>
#include <public.sdk/source/vst/hosting/module.h>

#include "../StatefulPerTrackEffect.h"
#include "internal/ComponentHandler.h"

#include "SampleCount.h"

class NumericTextCtrl;

class ParameterChangesProvider;
class VST3ParametersWindow;
class VST3Wrapper;

/**
 * \brief Objects of this class connect Audacity with VST3 effects
 */
class VST3Effect final : public StatefulPerTrackEffect
{
   // Keep strong reference to a module; this because it has to be destroyed in the destructor of this class,
   // otherwise the destruction of mEditController and mEffectComponent would trigger a memory fault.
   std::shared_ptr<VST3::Hosting::Module> mModule;
   const VST3::Hosting::ClassInfo mEffectClassInfo;
   std::unique_ptr<VST3Wrapper> mWrapper;
   
   bool mActive{false};
   //Used if provided by the plugin and enabled in the settings
   Steinberg::IPtr<Steinberg::IPlugView> mPlugView;
   Steinberg::IPtr<Steinberg::IPlugFrame> mPlugFrame;
   wxWindow* mParent { nullptr };
   NumericTextCtrl* mDuration { nullptr };
   //Used if graphical plugin interface is disabled in the settings, or not provided by the plugin
   VST3ParametersWindow* mPlainUI { nullptr };

   //Holds pending parameter changes to be applied to multiple realtime effects.
   //Not used in the "offline" mode
   internal::ComponentHandler::PendingChangesPtr mPendingChanges;

   std::vector<std::unique_ptr<VST3Effect>> mRealtimeGroupProcessors;

   // Mutable cache fields computed once on demand
   mutable bool mRescanFactoryPresets { true };
   mutable RegistryPaths mFactoryPresets;

   size_t mUserBlockSize { 8192 };
   bool mUseLatency { true };
   sampleCount mInitialDelay { 0 };

   mutable bool mInitialFetchDone{ false };

public:

   static EffectFamilySymbol GetFamilySymbol();

   VST3Effect(
      std::shared_ptr<VST3::Hosting::Module> module,
      VST3::Hosting::ClassInfo effectClassInfo);

   VST3Effect(const VST3Effect&) = delete;
   VST3Effect(VST3Effect&&) = delete;
   VST3Effect& operator=(const VST3Effect&) = delete;
   VST3Effect& operator=(VST3Effect&) = delete;
   
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
   RealtimeSince RealtimeSupport() const override;
   bool SupportsAutomation() const override;
   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;
   bool LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;
   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;
   sampleCount GetLatency() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   bool ProcessFinalize() noexcept override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

   int ShowClientInterface(wxWindow &parent, wxDialog &dialog,
      EffectUIValidator *pValidator, bool forceModal) override;
   bool InitializePlugin();
   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::shared_ptr<EffectInstance> DoMakeInstance();
   bool IsGraphicalUI() override;
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool ValidateUI(EffectSettings &) override;
   bool CloseUI() override;
   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;
   bool HasOptions() override;
   void ShowOptions() override;

   EffectSettings MakeSettings() const override;

   bool TransferDataToWindow(const EffectSettings& settings) override;

private:
   //Used to flush all pending changes to the IAudioProcessor, while
   //plugin is inactive(!)
   void FlushPendingChanges() const;

   void OnEffectWindowResize(wxSizeEvent & evt);

   bool LoadVSTUI(wxWindow* parent);

   bool LoadPreset(const wxString& path, EffectSettings& settings);

   void ReloadUserOptions();
};
