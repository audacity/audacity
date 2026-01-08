/*
 * Audacity: A Digital Audio Editor
 */
#include "parameterextractorregistry.h"

#include "framework/global/log.h"

using namespace au::effects;

void ParameterExtractorRegistry::registerExtractor(IParameterExtractorServicePtr extractor)
{
    IF_ASSERT_FAILED(extractor) {
        return;
    }

    EffectFamily family = extractor->family();

    if (m_extractors.find(family) != m_extractors.end()) {
        LOGW() << "Overwriting existing parameter extractor for family: " << static_cast<int>(family);
    }

    m_extractors[family] = std::move(extractor);
    LOGI() << "Registered parameter extractor for family: " << static_cast<int>(family);
}

IParameterExtractorService* ParameterExtractorRegistry::extractorForFamily(EffectFamily family) const
{
    auto it = m_extractors.find(family);
    if (it != m_extractors.end()) {
        return it->second.get();
    }
    return nullptr;
}

bool ParameterExtractorRegistry::hasExtractorForFamily(EffectFamily family) const
{
    return m_extractors.find(family) != m_extractors.end();
}
