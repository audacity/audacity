#pragma once

#include <vector>

#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <pluginterfaces/vst/ivstprocesscontext.h>
#include <public.sdk/source/vst/hosting/module.h>

#include "au3-components/EffectInterface.h"

class VST3Wrapper;

namespace Steinberg {
class IPlugFrame;
class IPlugView;
}

namespace Steinberg {
namespace Vst {
class IComponentHandler;
class IConnectionPoint;
class IEditController;
class IParameterChanges;
}
}

class SingleInputParameterValue final : public Steinberg::Vst::IParamValueQueue
{
    Steinberg::Vst::ParamID mParameterId{};
    Steinberg::Vst::ParamValue mValue;
public:

    SingleInputParameterValue() { FUNKNOWN_CTOR }
    ~SingleInputParameterValue() { FUNKNOWN_DTOR }

    void Set(Steinberg::Vst::ParamID id, const Steinberg::Vst::ParamValue value);

    Steinberg::tresult PLUGIN_API addPoint(Steinberg::int32 sampleOffset, Steinberg::Vst::ParamValue value,
                                           Steinberg::int32& index) override;

    Steinberg::Vst::ParamID PLUGIN_API getParameterId() override;

    Steinberg::tresult PLUGIN_API getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
                                           Steinberg::Vst::ParamValue& value) override;

    Steinberg::int32 PLUGIN_API getPointCount() override;

    DECLARE_FUNKNOWN_METHODS
};

class VST3_API VST3Wrapper
{
    EffectSettings mDefaultSettings;
    VST3::Hosting::Module& mModule;
    const VST3::Hosting::ClassInfo& mEffectClassInfo;
public:

    struct FactoryPresetDesc
    {
        wxString id;
        wxString displayName;
    };

    Steinberg::IPtr<Steinberg::Vst::IAudioProcessor> mAudioProcessor;
    Steinberg::Vst::ProcessSetup mSetup;
    Steinberg::IPtr<Steinberg::Vst::IComponent> mEffectComponent;
    Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mComponentConnectionProxy;
    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mControllerConnectionProxy;
    Steinberg::IPtr<Steinberg::Vst::IComponentHandler> mComponentHandler;

    //! Counter to track when FetchSettings was last called, used to skip redundant calls
    uint64_t lastFetchedSettingsCounter{ 0 };

    VST3Wrapper(VST3::Hosting::Module& module, const VST3::Hosting::ClassInfo& effectClassInfo);
    ~VST3Wrapper();

    VST3Wrapper(const VST3Wrapper&) = delete;
    VST3Wrapper(VST3Wrapper&&) = delete;
    VST3Wrapper& operator=(const VST3Wrapper&) = delete;
    VST3Wrapper& operator=(VST3Wrapper&&) = delete;

    //! Should be called once before almost any other method call
    void InitializeComponents();

    VST3::Hosting::Module& GetModule() const { return mModule; }
    const VST3::Hosting::ClassInfo& GetEffectClassInfo() const;

    bool IsActive() const noexcept;

    //!Fetch state from settings object, may change internal runtime data
    void FetchSettings(EffectSettings&, bool resetState);
    //!Saves current state inside settings object, clears all runtime data
    void StoreSettings(EffectSettings&) const;

    void LoadPreset(const wxString& presetId);
    void SavePresetToFile(const wxString& filepath) const;

    //!Initializes effect for processing using settings.
    bool Initialize(EffectSettings& settings, Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode,
                    Steinberg::int32 maxSamplesPerBlock);
    //!Frees up resources allocated for processing, should be called
    //!after processing is complete. Optionally settings object may
    //!be passed to update runtime data with current internal state.
    void Finalize(EffectSettings* settings);

    //!Prepares effect to process next block with changes written to the settings object
    void ProcessBlockStart(const EffectSettings& settings);

    //Used to send EffectSettings changes to the IAudioProcessor, while effect is inactive(!)

    //! \param hasChanges optional output variable, set to true if flushing has
    //! changed the DSP model state
    void FlushParameters(EffectSettings& settings, bool* hasChanges = nullptr);

    //Intialize first, before calling to Process. It's safe to it use from another thread
    size_t Process(const float* const* inBlock, float* const* outBlock, size_t blockLen);

    void SuspendProcessing();
    void ResumeProcessing();

    void BeginParameterEdit(EffectSettingsAccess& access);
    void EndParameterEdit();

    //! Returns an array of factory preset ids.
    //! Safe to call before `InitializeComponents`
    std::vector<FactoryPresetDesc> FindFactoryPresets() const;

    Steinberg::int32 GetLatencySamples() const;

    static EffectSettings MakeSettings();

    static void LoadSettings(const CommandParameters& parms, EffectSettings& settings);
    static void SaveSettings(const EffectSettings& settings, CommandParameters& parms);
    static OptionalMessage LoadUserPreset(
        const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings);
    static void SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings);

    static void CopySettingsContents(const EffectSettings& src, EffectSettings& dst);

    std::function<void(Steinberg::Vst::ParamID)> ParamChangedHandler;

private:

    bool LoadPresetFromStream(Steinberg::IBStream* fileStream);
    bool SavePresetToStream(Steinberg::IBStream* fileStream) const;

    //Reads runtime data changes to apply them during next processing pass
    void ConsumeChanges(const EffectSettings& settings);

    //! Move the transport on by a block that has just been processed
    void AdvanceTransport(Steinberg::int32 numSamples);

    //! Report the transport as stopped and push one silent block through
    /*!
     A plug-in that accumulates what it is fed while the host plays needs to be
     told that no more is coming, and a zero-sample call does not say it: that
     is the parameter-flush idiom, which plug-ins are free to skip. Does
     nothing unless the transport was playing.
     */
    void NotifyTransportStopped();

    //! Channels on the main buses, which is what Process expects to be given
    unsigned CountMainChannels(Steinberg::Vst::BusDirection direction) const;

    //! Audacity has no tempo map, so a steady default is reported to plug-ins
    //! that insist on a musical timeline
    static constexpr double sDefaultTempo = 120.0;
    static constexpr Steinberg::int32 sDefaultTimeSigNumerator = 4;
    static constexpr Steinberg::int32 sDefaultTimeSigDenominator = 4;

    bool mActive { false };

    //! Whether the stored state has been pushed into this plug-in already
    bool mStateRestored { false };

    std::vector<std::pair<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> > mParameters;
    //A preallocated array of Steinberg::Vst::IParameterValueQueue
    //used as a view to an actual parameter changes that reside
    //in VST3EffectSettings structure, dynamically assigned during
    //processing
    std::unique_ptr<SingleInputParameterValue[]> mParameterQueues;

    Steinberg::Vst::ProcessContext mProcessContext { };

    //! @name The final block that NotifyTransportStopped renders
    //! Silence in, and somewhere for the plug-in to write; sized once, during
    //! Initialize, so that finalizing does no allocation
    //! @{
    std::vector<float> mStopBlock;
    //! Into mStopBlock: the input channels, then the output channels
    std::vector<float*> mStopBlockChannels;
    unsigned mMainInputChannels { 0 };
    unsigned mMainOutputChannels { 0 };
    //! @}
};
