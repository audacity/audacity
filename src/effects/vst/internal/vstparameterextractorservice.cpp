/*
 * Audacity: A Digital Audio Editor
 */
#include "vstparameterextractorservice.h"
#include "vst3parametersextractor.h"

using namespace au::effects;

ParameterInfoList VstParameterExtractorService::extractParameters(EffectInstance* instance,
                                                                  EffectSettingsAccessPtr settingsAccess) const
{
    return VST3ParametersExtractor::extractParameters(instance, settingsAccess);
}

ParameterInfo VstParameterExtractorService::getParameter(EffectInstance* instance, const muse::String& parameterId) const
{
    return VST3ParametersExtractor::getParameter(instance, parameterId);
}

double VstParameterExtractorService::getParameterValue(EffectInstance* instance, const muse::String& parameterId) const
{
    return VST3ParametersExtractor::getParameterValue(instance, parameterId);
}

bool VstParameterExtractorService::setParameterValue(EffectInstance* instance,
                                                     const muse::String& parameterId,
                                                     double normalizedValue,
                                                     EffectSettingsAccessPtr settingsAccess)
{
    return VST3ParametersExtractor::setParameterValue(instance, parameterId, normalizedValue, settingsAccess);
}

muse::String VstParameterExtractorService::getParameterValueString(EffectInstance* instance,
                                                                   const muse::String& parameterId,
                                                                   double value) const
{
    return VST3ParametersExtractor::getParameterValueString(instance, parameterId, value);
}
