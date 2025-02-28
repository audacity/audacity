/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitWrapper.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.h

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_WRAPPER_H
#define AUDACITY_AUDIOUNIT_WRAPPER_H

#if USE_AUDIO_UNITS

#include <optional>
#include <map>
#include <set>
#include <unordered_map>
#include <wx/string.h>

#include "AudioUnitUtils.h"
#include "Identifier.h"

class wxCFStringRef;
class wxMemoryBuffer;
class EffectDefinitionInterface;
class EffectSettings;
class TranslatableString;
class AudioUnitWrapper;

/**
 * @struct AudioUnitEffectSettings
 * @brief Represents a cached copy of the state stored in an AudioUnit, but can outlive the original AudioUnit.
 *
 * This structure handles the storage and management of settings and state information for AudioUnit effects.
 * It provides mechanisms for sharing settings between different instances and managing preset configurations.
 */
struct AudioUnitEffectSettings {
    /**
     * @brief Shared set of strings to optimize memory usage by avoiding repeated allocations.
     *
     * All instances of AudioUnitEffectSettings share this set to reduce memory overhead and ensure consistency.
     * The effect object and all Settings objects coming from it share this set of strings.
     * Note: The names associated with parameter IDs are not invariant metadata of an AudioUnit effect.
     * For example, AUGraphicEQ changes names of slider parameters when you switch between 10 and 31 bands.
     */
    using StringSet = std::set<wxString>;
    const std::shared_ptr<StringSet> mSharedNames{
        std::make_shared<StringSet>() };

    //! Optionally store a preset
    std::optional<SInt32> mPresetNumber;

    //! Map from numerical parameter IDs (not always a small initial segment
    //! of the integers) to optional pairs of names and floating point values
    using Pair = std::pair<const wxString&, AudioUnitParameterValue>;
    using Map = std::map<AudioUnitParameterID, std::optional<Pair> >;
    Map values;

    AudioUnitEffectSettings() = default;
    AudioUnitEffectSettings(Map map)
        : values{move(map)} {}

    //! Get a pointer to a durable copy of `name`
    //! May allocate memory
    const wxString& Intern(const wxString& name)
    {
        // std::set::insert guarantees this iterator is not at the end
        auto [iter, _] = mSharedNames->insert(name);
        // so dereference it merrily
        return *iter;
    }

    //! Associate nullopt with all keys already present in the map
    void ResetValues()
    {
        for (auto&[_, value] : values) {
            value.reset();
        }
    }
};

/**
 * @class AudioUnitWrapper
 * @brief Manages and interacts with an AudioUnit, providing operations on audio effects.
 *
 * This class hosts the functionality for managing AudioUnit settings, presets, and parameters,
 * allowing manipulation of audio processing units.
 */
struct AudioUnitWrapper
{
    using Parameters = PackedArray::Ptr<const AudioUnitParameterID>;

    static AudioUnitEffectSettings& GetSettings(EffectSettings& settings);
    static const AudioUnitEffectSettings& GetSettings(
        const EffectSettings& settings);

    /*!
     @param pParameters if non-null, use those; else, fetch from the AudioUnit
     */
    AudioUnitWrapper(AudioComponent component, Parameters* pParameters)
        : mComponent{component}
        , mParameters{pParameters ? *pParameters : mOwnParameters}
    {
    }

    // Supply most often used values as defaults for scope and element
    template<typename T>
    OSStatus GetFixedSizeProperty(AudioUnitPropertyID inID, T& property,
                                  AudioUnitScope inScope = kAudioUnitScope_Global,
                                  AudioUnitElement inElement = 0) const
    {
        // Supply mUnit.get() to the non-member function
        return AudioUnitUtils::GetFixedSizeProperty(mUnit.get(),
                                                    inID, property, inScope, inElement);
    }

    // Supply most often used values as defaults for scope and element
    template<typename T>
    OSStatus GetVariableSizeProperty(AudioUnitPropertyID inID,
                                     PackedArray::Ptr<T>& pObject,
                                     AudioUnitScope inScope = kAudioUnitScope_Global,
                                     AudioUnitElement inElement = 0) const
    {
        return AudioUnitUtils::GetVariableSizeProperty(mUnit.get(),
                                                       inID, pObject, inScope, inElement);
    }

    // Supply most often used values as defaults for scope and element
    template<typename T>
    OSStatus SetProperty(AudioUnitPropertyID inID, const T& property,
                         AudioUnitScope inScope = kAudioUnitScope_Global,
                         AudioUnitElement inElement = 0) const
    {
        // Supply mUnit.get() to the non-member function
        return AudioUnitUtils::SetProperty(mUnit.get(),
                                           inID, property, inScope, inElement);
    }

    class ParameterInfo;
    //! Return value: if true, continue visiting
    using ParameterVisitor
        =std::function< bool (const ParameterInfo& pi, AudioUnitParameterID ID) >;
    void ForEachParameter(ParameterVisitor visitor) const;

    bool LoadPreset(const EffectDefinitionInterface& effect, const RegistryPath& group, EffectSettings& settings) const;
    bool LoadFactoryPreset(const EffectDefinitionInterface& effect, int id, EffectSettings* pSettings) const;

    //! Obtain dump of the setting state of an AudioUnit instance
    /*!
     @param binary if false, then produce XML serialization instead; but
     AudioUnits does not need to be told the format again to reinterpret the blob
     @return smart pointer to data, and an error message
     */
    std::pair<CF_ptr<CFDataRef>, TranslatableString>
    MakeBlob(const EffectDefinitionInterface& effect, const AudioUnitEffectSettings& settings, const wxCFStringRef& cfname,
             bool binary) const;

    //! Interpret the dump made before by MakeBlob
    /*!
     @param group only for formatting error messages
     @return an error message
     */
    TranslatableString InterpretBlob(AudioUnitEffectSettings& settings, const wxString& group, const wxMemoryBuffer& buf) const;

    //! May allocate memory, so should be called only in the main thread
    bool FetchSettings(AudioUnitEffectSettings& settings, bool fetchValues, bool fetchPreset = false) const;
    bool StoreSettings(const EffectDefinitionInterface& effect, const AudioUnitEffectSettings& settings) const;

    //! Copy, then clear the optionals in src
    static bool MoveSettingsContents(
        AudioUnitEffectSettings&& src, AudioUnitEffectSettings& dst, bool merge);

    bool CreateAudioUnit();

    AudioUnit GetAudioUnit() const { return mUnit.get(); }
    AudioComponent GetComponent() const { return mComponent; }
    const Parameters& GetParameters() const
    { return mParameters; }

    // @param identifier only for logging messages
    bool SetRateAndChannels(double sampleRate, const wxString& identifier);

protected:
    const AudioComponent mComponent;
    AudioUnitCleanup<AudioUnit, AudioComponentInstanceDispose> mUnit;

    Parameters mOwnParameters;
    Parameters& mParameters;

    // Reassinged in GetRateAndChannels()
    unsigned mAudioIns{ 2 };
    unsigned mAudioOuts{ 2 };
};

/**
 * @class AudioUnitWrapper::ParameterInfo
 * @brief Encapsulates parameter information for an AudioUnit.
 * */
class AudioUnitWrapper::ParameterInfo final
{
public:
    //! Make a structure holding a key for the config file and a value
    ParameterInfo(AudioUnit mUnit, AudioUnitParameterID parmID);
    //! Recover the parameter ID from the key, if well formed
    static std::optional<AudioUnitParameterID> ParseKey(const wxString& key);

    std::optional<wxString> mName;
    AudioUnitUtils::ParameterInfo mInfo{};

private:
    // constants
    static constexpr char idBeg = wxT('<');
    static constexpr char idSep = wxT(',');
    static constexpr char idEnd = wxT('>');
};

#endif

#endif
