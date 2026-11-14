/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/PluginDescriptor.h"

//! Builds and decomposes effect plugin IDs of the form
//! "Effect_<family>_<vendor>_<symbol>_<path>".
//!
//! The effect-ID format is owned by the AU4 effects layer (see
//! au::effects::utils in effectsutils.h). This interface is declared here, in
//! the legacy module-manager, only so that PluginManager can resolve effect-ID
//! components without re-implementing that format. The concrete implementation
//! lives in the AU4 effects layer and is injected at start-up via
//! PluginManager::SetEffectIdResolver().
class IEffectIdResolver
{
public:
    virtual ~IEffectIdResolver() = default;

    //! Build the plugin ID for an effect.
    virtual PluginID EffectId(const EffectDefinitionInterface* effect) const = 0;

    //! Whether `id` is a well-formed effect ID that the accessors below can
    //! decompose. They must not be called otherwise.
    virtual bool IsEffectId(const PluginID& id) const = 0;

    //! @pre `IsEffectId(id)`
    //! @{
    virtual wxString EffectFamily(const PluginID& id) const = 0;
    virtual wxString EffectVendor(const PluginID& id) const = 0;
    //! The effect symbol (parts[3]); named "name" by the AU4 utils it wraps.
    virtual wxString EffectName(const PluginID& id) const = 0;
    virtual wxString EffectPath(const PluginID& id) const = 0;
    //! @}
};
