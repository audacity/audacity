/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectPlugin.h

   Paul Licameli
   split from EffectInterface.h

**********************************************************************/
#ifndef __AUDACITY_EFFECTPLUGIN_H__
#define __AUDACITY_EFFECTPLUGIN_H__

#include "EffectInterface.h"

#include <functional>
#include <memory>

class EffectSettingsManager;

struct EffectSettings;
class EffectSettingsAccess;
class EffectPlugin;

class TrackList;
class WaveTrackFactory;
class NotifyingSelectedRegion;
class EffectInstanceEx;

/***************************************************************************//**
\class EffectPlugin
@brief Factory of instances of an effect
*******************************************************************************/
class EFFECTS_API EffectPlugin : public EffectInstanceFactory
{
public:
    using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;

    const static wxString kUserPresetIdent;
    const static wxString kFactoryPresetIdent;
    const static wxString kCurrentSettingsIdent;
    const static wxString kFactoryDefaultsIdent;

    EffectPlugin& operator=(EffectPlugin&) = delete;
    virtual ~EffectPlugin();

    //! Whether there are preferences applying to the effect's family, for which
    //! a UI may be needed to change them
    virtual bool HasOptions() const = 0;

    //! Whether the effect supports export of presets to files, and importing too
    virtual bool CanExportPresets() const = 0;

    virtual const EffectSettingsManager& GetDefinition() const = 0;

    virtual bool SaveSettingsAsString(
        const EffectSettings& settings, wxString& parms) const = 0;
    // @return nullptr for failure
    [[nodiscard]] virtual OptionalMessage LoadSettingsFromString(
        const wxString& parms, EffectSettings& settings) const = 0;
    virtual bool IsBatchProcessing() const = 0;
    virtual void SetBatchProcessing() = 0;
    virtual void UnsetBatchProcessing() = 0;

    using InstancePointer = std::shared_ptr<EffectInstanceEx>;
    //! nullopt for failure is distinct from successful null return
    //! If a non-null pointer is returned, assume Init() also succeeded on it
    //! If a null pointer, then DoEffect will use MakeInstance()
    using InstanceFinder = std::function<
        std::optional<InstancePointer>(EffectSettings& settings)
        >;

    /*!
     Will only operate on tracks that have the "selected" flag set to true,
     which is consistent with Audacity's standard UI, and only when
     finder succeeds

     @return true on success
     */
    virtual bool DoEffect(
        EffectSettings& settings, //!< Always given; only for processing
        const InstanceFinder& finder, double projectRate, TrackList* list, WaveTrackFactory* factory,
        NotifyingSelectedRegion& selectedRegion, unsigned flags, const EffectSettingsAccessPtr& pAccess = nullptr
        //!< Sometimes given; only for UI
        ) = 0;
};

/***************************************************************************//**
\class EffectInstanceEx
@brief Performs effect computation
*******************************************************************************/
class EFFECTS_API EffectInstanceEx : public virtual EffectInstance
{
public:
    //! Call once to set up state for whole list of tracks to be processed
    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool Init();

    //! Actually do the effect here.
    /*!
     @return success
     */
    virtual bool Process(EffectSettings& settings) = 0;

    virtual std::string GetLastError() const { return ""; }

    ~EffectInstanceEx() override;
};
#endif
