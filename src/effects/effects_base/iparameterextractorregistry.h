/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iparameterextractorservice.h"

#include "framework/global/modularity/imoduleinterface.h"

namespace au::effects {
//! Registry for parameter extractor services.
//! Plugin modules (VST, LV2, AudioUnit) register their extractors here.
//! EffectParametersProvider uses this to lookup the appropriate extractor.
class IParameterExtractorRegistry : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IParameterExtractorRegistry)

public:
    virtual ~IParameterExtractorRegistry() = default;

    //! Register a parameter extractor for a specific effect family
    virtual void registerExtractor(IParameterExtractorServicePtr extractor) = 0;

    //! Get the extractor for a specific effect family, or nullptr if not registered
    virtual IParameterExtractorService* extractorForFamily(EffectFamily family) const = 0;

    //! Check if an extractor is registered for a specific effect family
    virtual bool hasExtractorForFamily(EffectFamily family) const = 0;
};
}
