/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include "EffectBase.h"

#define BUILTIN_EFFECT_PREFIX wxT("Built-in Effect: ")

class EffectParameterMethods;
class WaveTrack;

class sampleCount;

class EFFECTS_API Effect /* not final */ : public EffectBase
{
    //
    // public methods
    //
    // Used by the outside program to determine properties of an effect and
    // apply the effect to one or more tracks.
    //
public:
    static inline Effect* FetchParameters(Effect& e, EffectSettings&)
    { return &e; }

    // The constructor is called once by each subclass at the beginning of the program.
    // Avoid allocating memory or doing time-consuming processing here.
    Effect();
    virtual ~Effect();

    // ComponentInterface implementation

    PluginPath GetPath() const override;
    bool VisitSettings(
        SettingsVisitor& visitor, EffectSettings& settings) override;
    bool VisitSettings(
        ConstSettingsVisitor& visitor, const EffectSettings& settings)
    const override;

    ComponentInterfaceSymbol GetSymbol() const override;

    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation

    EffectFamilySymbol GetFamily() const override;
    bool IsInteractive() const override;
    bool IsDefault() const override;
    RealtimeSince RealtimeSupport() const override;
    bool SupportsAutomation() const override;

    bool SaveSettings(
        const EffectSettings& settings, CommandParameters& parms) const override;
    bool LoadSettings(
        const CommandParameters& parms, EffectSettings& settings) const override;

    OptionalMessage LoadUserPreset(
        const RegistryPath& name, EffectSettings& settings) const override;
    bool SaveUserPreset(
        const RegistryPath& name, const EffectSettings& settings) const override;

    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings)
    const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings)
    const override;

    // VisitSettings(), SaveSettings(), and LoadSettings()
    // use the functions of EffectParameterMethods.  By default, this function
    // defines an empty list of parameters.
    virtual const EffectParameterMethods& Parameters() const;

    bool CanExportPresets() const override;
    bool HasOptions() const override;

    // EffectPlugin implementation

    const EffectSettingsManager& GetDefinition() const override;
    // time format in Selection toolbar
    virtual NumericFormatID GetSelectionFormat() /* not override? */;

    bool SaveSettingsAsString(
        const EffectSettings& settings, wxString& parms) const override;
    [[nodiscard]] OptionalMessage LoadSettingsFromString(
        const wxString& parms, EffectSettings& settings) const override;
    bool IsBatchProcessing() const override;
    void SetBatchProcessing() override;
    void UnsetBatchProcessing() override;

    // Effect implementation

    unsigned TestUIFlags(unsigned mask);

    //! Re-invoke DoEffect on another Effect object that implements the work
    bool Delegate(Effect& delegate, EffectSettings& settings, InstanceFinder finder = {});

protected:

    //! Default implementation returns false
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;

    //! Default implementation returns `previewLength`
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

    // No more virtuals!

    // The Progress methods all return true if the user has cancelled;
    // you should exit immediately if this happens (cleaning up memory
    // is okay, but don't try to undo).

    // Pass a fraction between 0.0 and 1.0
    bool TotalProgress(double frac, const TranslatableString& = {}) const;

    // Pass a fraction between 0.0 and 1.0, for the current track
    // (when doing one track at a time)
    bool TrackProgress(
        int whichTrack, double frac, const TranslatableString& = {}) const;

    // Pass a fraction between 0.0 and 1.0, for the current track group
    // (when doing stereo groups at a time)
    bool TrackGroupProgress(
        int whichGroup, double frac, const TranslatableString& = {}) const;

    int GetNumWaveTracks() const { return mNumTracks; }
    int GetNumWaveGroups() const { return mNumGroups; }

    // Calculates the start time and length in samples for one or two channels
    void GetBounds(const WaveTrack& track, sampleCount* start, sampleCount* len);

private:
    wxString GetSavedStateGroup();

    bool mIsBatch{ false };
};

//! Convenience for generating EffectDefinitionInterface overrides
//! and static down-casting functions
template<typename Settings, typename Base>
class EffectWithSettings : public Base
{
public:
    EffectSettings MakeSettings() const override
    {
        return EffectSettings::Make<Settings>();
    }

    bool CopySettingsContents(
        const EffectSettings& src, EffectSettings& dst) const override
    {
        return EffectSettings::Copy<Settings>(src, dst);
    }

    //! Assume settings originated from MakeSettings() and copies thereof
    static inline Settings& GetSettings(EffectSettings& settings)
    {
        auto pSettings = settings.cast<Settings>();
        assert(pSettings);
        return *pSettings;
    }

    //! Assume settings originated from MakeSettings() and copies thereof
    static inline const Settings& GetSettings(const EffectSettings& settings)
    {
        return GetSettings(const_cast<EffectSettings&>(settings));
    }

    static inline Settings*
    FetchParameters(Base&, EffectSettings& s)
    {
        return &GetSettings(s);
    }
};

// FIXME:
// FIXME:  Remove this once all effects are using the NEW dialog
// FIXME:

#define ID_EFFECT_PREVIEW ePreviewID

#endif
