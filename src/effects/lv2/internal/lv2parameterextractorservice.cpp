/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2parameterextractorservice.h"

#include "framework/global/log.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-lv2/LV2Instance.h"
#include "au3-lv2/LV2Ports.h"

using namespace au::effects;
using namespace muse;

namespace {
struct PortLookup {
    const LV2Instance* instance = nullptr;
    LV2ControlPortPtr port;
    size_t valueIndex = 0;
};

const LV2Instance* toLv2Instance(au::effects::EffectInstance* instance)
{
    return dynamic_cast<const LV2Instance*>(instance);
}

std::optional<PortLookup> findPort(au::effects::EffectInstance* instance, const String& parameterId)
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        return std::nullopt;
    }

    const wxString symbol = au::au3::wxFromString(parameterId);
    const LV2ControlPortArray& ports = lv2Instance->GetPorts().mControlPorts;
    for (size_t i = 0; i < ports.size(); ++i) {
        if (ports[i]->mIsInput && ports[i]->mSymbol == symbol) {
            return PortLookup { lv2Instance, ports[i], i };
        }
    }
    return std::nullopt;
}

const LV2EffectSettings* lv2Settings(const EffectSettings& settings)
{
    return settings.cast<LV2EffectSettings>();
}

float storedValue(const PortLookup& lookup, const au::effects::EffectSettingsAccessPtr& settingsAccess)
{
    if (settingsAccess) {
        if (const LV2EffectSettings* settings = lv2Settings(settingsAccess->Get())) {
            if (lookup.valueIndex < settings->values.size()) {
                return settings->values[lookup.valueIndex];
            }
        }
    }
    return lookup.port->mDef;
}

float displayFactor(const LV2ControlPort& port, const LV2Instance& instance)
{
    return port.mSampleRate ? instance.GetSampleRate() : 1.0f;
}

String formatValue(const ParameterInfo& info, double value)
{
    if (info.type == ParameterType::Dropdown) {
        for (size_t i = 0; i < info.enumIndices.size(); ++i) {
            if (std::abs(info.enumIndices[i] - value) < 0.001) {
                return info.enumValues[i];
            }
        }
    }
    return String::number(value, info.numDecimals());
}

ParameterInfo makeInfo(const PortLookup& lookup, float stored)
{
    const LV2ControlPort& port = *lookup.port;
    const float factor = displayFactor(port, *lookup.instance);

    ParameterInfo info;
    info.id = au::au3::wxToString(port.mSymbol);
    info.name = au::au3::wxToString(port.mName);
    info.group = au::au3::wxToString(port.mGroup.Translation());
    info.units = au::au3::wxToString(port.mUnits);

    info.minValue = port.mMin * factor;
    info.maxValue = port.mMax * factor;
    info.defaultValue = port.mDef * factor;

    double value = stored * factor;

    if (port.mToggle) {
        info.type = ParameterType::Toggle;
        info.minValue = 0.0;
        info.maxValue = 1.0;
        info.stepCount = 1;
        info.stepSize = 1.0;
        info.isInteger = true;
        value = value > 0.0 ? 1.0 : 0.0;
    } else if (port.mEnumeration && !port.mScaleValues.empty()) {
        info.type = ParameterType::Dropdown;
        info.enumValues.reserve(port.mScaleValues.size());
        info.enumIndices.reserve(port.mScaleValues.size());
        for (size_t i = 0; i < port.mScaleValues.size(); ++i) {
            info.enumValues.push_back(i < port.mScaleLabels.size()
                                      ? au::au3::wxToString(port.mScaleLabels[i])
                                      : String::number(port.mScaleValues[i]));
            info.enumIndices.push_back(port.mScaleValues[i] * factor);
        }
        info.isInteger = port.mInteger;
        value = port.mScaleValues[port.Discretize(stored)] * factor;
    } else {
        info.type = ParameterType::Slider;
        info.isInteger = port.mInteger;
        info.isLogarithmic = port.mLogarithmic;
        if (port.mInteger) {
            info.stepSize = 1.0;
        }
    }

    info.currentValue = value;
    info.currentValueString = formatValue(info, value);

    return info;
}
} // anonymous namespace

ParameterInfoList Lv2ParameterExtractorService::extractParameters(EffectInstance* instance,
                                                                  EffectSettingsAccessPtr settingsAccess) const
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        LOGW() << "Lv2ParameterExtractorService: instance is not an LV2Instance";
        return {};
    }

    if (!settingsAccess) {
        settingsAccess = sessionSettings(instance);
    }

    const LV2ControlPortArray& ports = lv2Instance->GetPorts().mControlPorts;

    ParameterInfoList result;
    result.reserve(ports.size());

    for (size_t i = 0; i < ports.size(); ++i) {
        const LV2ControlPortPtr& port = ports[i];
        //! Output ports are meters, and trigger ports are momentary actions;
        //! neither maps onto the generic parameter model.
        if (!port->mIsInput || port->mTrigger) {
            continue;
        }
        const PortLookup lookup { lv2Instance, port, i };
        result.push_back(makeInfo(lookup, storedValue(lookup, settingsAccess)));
    }

    return result;
}

ParameterInfo Lv2ParameterExtractorService::getParameter(EffectInstance* instance, const String& parameterId) const
{
    const std::optional<PortLookup> lookup = findPort(instance, parameterId);
    if (!lookup) {
        return {};
    }

    return makeInfo(*lookup, storedValue(*lookup, sessionSettings(instance)));
}

double Lv2ParameterExtractorService::getParameterValue(EffectInstance* instance, const String& parameterId) const
{
    return getParameter(instance, parameterId).currentValue;
}

bool Lv2ParameterExtractorService::setParameterValue(EffectInstance* instance, const String& parameterId,
                                                     double fullRangeValue, EffectSettingsAccessPtr settingsAccess)
{
    const std::optional<PortLookup> lookup = findPort(instance, parameterId);
    if (!lookup) {
        return false;
    }

    if (!settingsAccess) {
        settingsAccess = sessionSettings(instance);
    }
    if (!settingsAccess) {
        LOGW() << "Lv2ParameterExtractorService: no settings access for parameter " << parameterId;
        return false;
    }

    const LV2ControlPort& port = *lookup->port;

    float value = static_cast<float>(fullRangeValue) / displayFactor(port, *lookup->instance);
    if (port.mToggle) {
        value = value > 0.5f ? 1.0f : 0.0f;
    } else {
        if (port.mHasLo) {
            value = std::max(value, port.mMin);
        }
        if (port.mHasHi) {
            value = std::min(value, port.mMax);
        }
        if (port.mInteger && !port.mEnumeration) {
            value = std::round(value);
        }
    }

    settingsAccess->ModifySettings([&](EffectSettings& settings) {
        if (LV2EffectSettings* lv2 = settings.cast<LV2EffectSettings>()) {
            if (lookup->valueIndex < lv2->values.size()) {
                lv2->values[lookup->valueIndex] = value;
            }
        }
        return nullptr;
    });

    return true;
}

String Lv2ParameterExtractorService::getParameterValueString(EffectInstance* instance, const String& parameterId,
                                                             double value) const
{
    const std::optional<PortLookup> lookup = findPort(instance, parameterId);
    if (!lookup) {
        return {};
    }

    return formatValue(makeInfo(*lookup, storedValue(*lookup, sessionSettings(instance))), value);
}

void Lv2ParameterExtractorService::endParameterGesture(EffectInstance* instance, const String&)
{
    if (const EffectSettingsAccessPtr settingsAccess = sessionSettings(instance)) {
        settingsAccess->Flush();
    }
}

void Lv2ParameterExtractorService::onInstanceDestroyed(EffectInstance* instance)
{
    m_sessionSettings.erase(instance);
}

void Lv2ParameterExtractorService::beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess)
{
    m_sessionSettings[instance] = settingsAccess;
}

void Lv2ParameterExtractorService::endParameterEditing(EffectInstance* instance)
{
    if (const EffectSettingsAccessPtr settingsAccess = sessionSettings(instance)) {
        settingsAccess->Flush();
    }
    m_sessionSettings.erase(instance);
}

EffectSettingsAccessPtr Lv2ParameterExtractorService::sessionSettings(EffectInstance* instance) const
{
    const auto it = m_sessionSettings.find(instance);
    return it != m_sessionSettings.end() ? it->second : nullptr;
}
