/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../iparameterextractorregistry.h"

#include <map>

namespace au::effects {
class ParameterExtractorRegistry : public IParameterExtractorRegistry
{
public:
    void registerExtractor(IParameterExtractorServicePtr extractor) override;
    IParameterExtractorService* extractorForFamily(EffectFamily family) const override;
    bool hasExtractorForFamily(EffectFamily family) const override;

private:
    std::map<EffectFamily, IParameterExtractorServicePtr> m_extractors;
};
}
