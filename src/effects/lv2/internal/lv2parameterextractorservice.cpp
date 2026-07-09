/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2parameterextractorservice.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-lv2/LV2Instance.h"
#include "au3-lv2/LV2Ports.h"

#include "log.h"

#include <algorithm>
#include <cmath>

using namespace au::effects;
using namespace muse;

namespace {
LV2Instance* toLv2Instance(au::effects::EffectInstance* instance)
{
    return dynamic_cast<LV2Instance*>(instance);
}

//! Current control-port values: from the settings if available, else the port defaults
std::vector<float> controlPortValues(const LV2Ports& ports, const EffectSettingsAccessPtr& settingsAccess)
{
    if (settingsAccess) {
        if (const LV2EffectSettings* lv2Settings = settingsAccess->Get().cast<LV2EffectSettings>()) {
            return lv2Settings->values;
        }
    }
    std::vector<float> values;
    values.reserve(ports.mControlPorts.size());
    for (const LV2ControlPortPtr& port : ports.mControlPorts) {
        values.push_back(port->mDef);
    }
    return values;
}

//! Position among the control ports of the port with the given symbol
std::optional<size_t> findControlPortIndex(const LV2Ports& ports, const String& parameterId)
{
    const wxString symbol = au::au3::wxFromString(parameterId);
    for (size_t i = 0; i < ports.mControlPorts.size(); ++i) {
        if (ports.mControlPorts[i]->mSymbol == symbol) {
            return i;
        }
    }
    return std::nullopt;
}

//! Display precision, following AU3's plain-UI rule (LV2Editor::BuildPlain):
//! the finer the range, the more decimals.
int numDecimals(const LV2ControlPort& port)
{
    if (port.mInteger) {
        return 0;
    }
    const double range = port.mMax - port.mMin;
    return range < 10 ? 3 : range < 100 ? 2 : 1;
}

//! Step derived from AU3's 1000-position slider, rounded to one significant
//! digit so spin arrows increment by e.g. 0.5 rather than 0.4999.
double sliderStep(const LV2ControlPort& port)
{
    if (port.mInteger) {
        return 1.0;
    }
    const double range = port.mMax - port.mMin;
    if (!(range > 0) || !std::isfinite(range)) {
        return 0.0;
    }
    const double raw = range / 1000.0;
    const double mag = std::pow(10.0, std::floor(std::log10(raw)));
    return mag * std::max(1.0, std::round(raw / mag));
}

String formatValue(const LV2ControlPort& port, double value)
{
    if (!port.mScaleValues.empty() && port.mEnumeration) {
        const size_t i = port.Discretize(static_cast<float>(value));
        return au::au3::wxToString(port.mScaleLabels[i]);
    }
    if (port.mInteger) {
        return String::number(static_cast<int>(std::lround(value)));
    }
    return String::number(value, numDecimals(port));
}

ParameterInfo makeParameterInfo(const LV2ControlPort& port, double value)
{
    ParameterInfo info;

    info.id = au::au3::wxToString(port.mSymbol);
    info.name = au::au3::wxToString(port.mName);
    info.units = au::au3::wxToString(port.mUnits);
    info.group = au::au3::wxToString(port.mGroup.Translation());

    // Note: values of ports with the lv2:sampleRate property are to be
    // multiplied by the sample rate to yield the effective value. AU3's plain
    // UI scaled the displayed value and bounds accordingly; we show the raw
    // value for now (the DSP behavior is unaffected).
    info.minValue = port.mMin;
    info.maxValue = port.mMax;
    info.defaultValue = port.mDef;
    info.currentValue = value;
    info.isInteger = port.mInteger;
    info.isLogarithmic = port.mLogarithmic;
    info.currentValueString = formatValue(port, value);

    if (!port.mIsInput) {
        // AU3 shows output ports as meters; we show the value as text.
        info.type = ParameterType::ReadOnly;
        info.isReadOnly = true;
    } else if (port.mToggle || port.mTrigger) {
        // A trigger port is approximated by a toggle: switching it on writes
        // the "on" value to the port. (AU3 rendered a push button instead.)
        info.type = ParameterType::Toggle;
        info.minValue = 0.0; // LV2 toggled: <= 0 is off, > 0 is on
        info.maxValue = 1.0;
        info.stepCount = 1;
    } else if (port.mEnumeration && !port.mScaleValues.empty()) {
        info.type = ParameterType::Dropdown;
        info.enumValues.reserve(port.mScaleValues.size());
        info.enumIndices.reserve(port.mScaleValues.size());
        for (size_t i = 0; i < port.mScaleValues.size(); ++i) {
            info.enumValues.push_back(au::au3::wxToString(port.mScaleLabels[i]));
            info.enumIndices.push_back(port.mScaleValues[i]);
        }
        // Snap to a scale point so the dropdown finds the current selection
        info.currentValue = port.mScaleValues[port.Discretize(static_cast<float>(value))];
    } else {
        info.type = ParameterType::Slider;
        info.stepSize = sliderStep(port);
        info.numDecimalsOverride = numDecimals(port);
    }

    return info;
}
} // anonymous namespace

ParameterInfoList Lv2ParameterExtractorService::extractParameters(EffectInstance* instance,
                                                                  EffectSettingsAccessPtr settingsAccess) const
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        LOGW() << "Not an LV2 instance";
        return {};
    }

    if (settingsAccess) {
        m_settingsAccess[instance] = settingsAccess;
    }

    const LV2Ports& ports = lv2Instance->GetPorts();
    const std::vector<float> values = controlPortValues(ports, settingsAccess);

    ParameterInfoList result;
    result.reserve(ports.mControlPorts.size());

    // Same ordering as AU3's plain UI: groups sorted by translation, then the
    // ports of each group
    auto groups = ports.mGroups; // mutable copy
    std::sort(groups.begin(), groups.end(), TranslationLess);
    for (const auto& label : groups) {
        for (const int p : ports.mGroupMap.at(label)) { /* won't throw */
            const LV2ControlPortPtr& port = ports.mControlPorts[p];
            const double value = size_t(p) < values.size() ? values[p] : port->mDef;
            result.push_back(makeParameterInfo(*port, value));
        }
    }

    return result;
}

ParameterInfo Lv2ParameterExtractorService::getParameter(EffectInstance* instance, const String& parameterId) const
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        return {};
    }

    const LV2Ports& ports = lv2Instance->GetPorts();
    const std::optional<size_t> index = findControlPortIndex(ports, parameterId);
    if (!index) {
        return {};
    }

    const auto it = m_settingsAccess.find(instance);
    const std::vector<float> values
        = controlPortValues(ports, it != m_settingsAccess.end() ? it->second : nullptr);

    const LV2ControlPortPtr& port = ports.mControlPorts[*index];
    const double value = *index < values.size() ? values[*index] : port->mDef;
    return makeParameterInfo(*port, value);
}

double Lv2ParameterExtractorService::getParameterValue(EffectInstance* instance, const String& parameterId) const
{
    const ParameterInfo param = getParameter(instance, parameterId);
    return param.isValid() ? param.currentValue : 0.0;
}

bool Lv2ParameterExtractorService::setParameterValue(EffectInstance* instance, const String& parameterId,
                                                     double fullRangeValue, EffectSettingsAccessPtr settingsAccess)
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        return false;
    }

    const LV2Ports& ports = lv2Instance->GetPorts();
    const std::optional<size_t> index = findControlPortIndex(ports, parameterId);
    if (!index) {
        LOGW() << "No LV2 control port with symbol " << parameterId;
        return false;
    }

    const LV2ControlPortPtr& port = ports.mControlPorts[*index];
    if (!port->mIsInput) {
        LOGE() << "Cannot set value of output port " << parameterId;
        return false;
    }

    if (!settingsAccess) {
        const auto it = m_settingsAccess.find(instance);
        if (it == m_settingsAccess.end()) {
            return false;
        }
        settingsAccess = it->second;
    }
    m_settingsAccess[instance] = settingsAccess;

    float newValue = static_cast<float>(fullRangeValue);
    if (port->mEnumeration && !port->mScaleValues.empty()) {
        newValue = static_cast<float>(port->mScaleValues[port->Discretize(newValue)]);
    } else if (port->mToggle || port->mTrigger) {
        newValue = fullRangeValue > 0.5 ? 1.0f : 0.0f;
    } else {
        newValue = std::clamp(newValue, port->mMin, port->mMax);
    }

    bool ok = false;
    settingsAccess->ModifySettings([&](EffectSettings& settings) {
        LV2EffectSettings* lv2Settings = settings.cast<LV2EffectSettings>();
        if (lv2Settings && *index < lv2Settings->values.size()) {
            lv2Settings->values[*index] = newValue;
            ok = true;
        }
        return nullptr;
    });

    return ok;
}

String Lv2ParameterExtractorService::getParameterValueString(EffectInstance* instance, const String& parameterId,
                                                             double value) const
{
    const LV2Instance* lv2Instance = toLv2Instance(instance);
    if (!lv2Instance) {
        return {};
    }

    const LV2Ports& ports = lv2Instance->GetPorts();
    const std::optional<size_t> index = findControlPortIndex(ports, parameterId);
    if (!index) {
        return {};
    }

    return formatValue(*ports.mControlPorts[*index], value);
}

void Lv2ParameterExtractorService::beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess)
{
    if (settingsAccess) {
        m_settingsAccess[instance] = settingsAccess;
    }
}

void Lv2ParameterExtractorService::endParameterEditing(EffectInstance* instance)
{
    m_settingsAccess.erase(instance);
}

void Lv2ParameterExtractorService::onInstanceDestroyed(EffectInstance* instance)
{
    m_settingsAccess.erase(instance);
}
