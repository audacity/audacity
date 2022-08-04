#pragma once

#include <public.sdk/source/vst/utility/uid.h>

#include "effects/StatefulPerTrackEffect.h"

namespace Steinberg
{
   class IPlugView;
   class IPlugFrame;
}

class NumericTextCtrl;
class VST3ParametersWindow;

namespace VST3
{
   namespace Hosting
   {
      class Module;
   }
}

class VST3Wrapper;
class VST3Effect;

class VST3Instance
   : public PerTrackEffect::Instance
{
   VST3::UID mEffectUID;

   std::unique_ptr<VST3Wrapper> mWrapper;

   //Used if provided by the plugin and enabled in the settings
   Steinberg::IPtr<Steinberg::IPlugView> mPlugView;
   Steinberg::IPtr<Steinberg::IPlugFrame> mPlugFrame;
   wxWindow* mParent { nullptr };
   NumericTextCtrl* mDuration { nullptr };
   //Used if graphical plugin interface is disabled in the settings, or not provided by the plugin
   VST3ParametersWindow* mPlainUI { nullptr };

   size_t mUserBlockSize { 8192 };
   size_t mProcessingBlockSize { 8192 };
   bool mUseLatency { true };
   sampleCount mInitialDelay { 0 };

public:
   VST3Instance(const PerTrackEffect& effect, VST3::Hosting::Module& module, VST3::UID effectUID);
   ~VST3Instance() override;

   VST3Instance(const VST3Instance&) = delete;
   VST3Instance& operator=(const VST3Instance&) = delete;

   size_t GetTailSize() const override;
   bool Init() override;
   bool RealtimeAddProcessor(EffectSettings& settings, unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings& settings) noexcept override;
   bool RealtimeInitialize(EffectSettings& settings, double sampleRate) override;
   size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
      size_t numSamples) override;
   bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;
   bool RealtimeProcessStart(EffectSettings& settings) override;
   bool RealtimeResume() override;
   bool RealtimeSuspend() override;
   sampleCount GetLatency(const EffectSettings& settings, double sampleRate) const override;
   bool ProcessFinalize() noexcept override;
   bool ProcessInitialize(EffectSettings &settings,
         double sampleRate, ChannelNames chanMap) override;
   size_t GetBlockSize() const override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
      size_t blockLen) override;

   void PopulateUI(ShuttleGui &S, EffectSettingsAccess& access);
   void CloseUI();
   bool IsGraphicalUI() const;

   bool ValidateUI(EffectSettings&);

   bool SaveSettings(CommandParameters & parms) const;
   bool LoadSettings(const CommandParameters & parms);
   bool SaveUserPreset(const RegistryPath & name) const;
   bool LoadUserPreset(const RegistryPath & name);
   void ExportPresets() const;

   bool LoadPreset(const wxString& path);
   void ShowOptions();

   bool TransferDataToWindow(const EffectSettings& settings);

   unsigned GetAudioOutCount() const override;
   unsigned GetAudioInCount() const override;

private:

   void ReloadUserOptions();

   bool LoadVSTUI(wxWindow* parent);
   void OnEffectWindowResize(wxSizeEvent & evt);
};
