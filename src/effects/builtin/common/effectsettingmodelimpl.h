/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "builtineffectsettingmodel.h"
#include "au3-components/SettingsVisitor.h"

#include <functional>
#include <unordered_map>

namespace au::effects {
template<typename EffectType>
using ParamGetter = std::function<EffectParameter<typename EffectType::Settings, double, double, double>(const EffectType&)>;

template<typename EffectType>
class EffectSettingModelImpl : public BuiltinEffectSettingModel
{
public:
    using SettingsType = typename EffectType::Settings;

    struct Labels {
        const QString title;
        const QString unit;
    };

    using LabelMap = std::unordered_map<QString /*param ID*/, Labels>;

    EffectSettingModelImpl(QObject* parent, int instanceId, LabelMap labelMap, ParamGetter<EffectType> getter)
        : BuiltinEffectSettingModel{parent, instanceId}, m_labelMap{std::move(labelMap)}, m_getter{std::move(getter)} {}

    double value() const final override
    {
        const auto& s = settings<SettingsType>();
        const auto& param = m_getter(effect<EffectType>());
        const double v = s.*param.mem;
        return v;
    }

    void setValue(double newValue) final override
    {
        if (muse::is_equal(value(), newValue)) {
            return;
        }
        modifySettings([this, newValue](EffectSettings& settings) {
            auto* s = settings.cast<SettingsType>();
            IF_ASSERT_FAILED(s) {
                return;
            }
            const auto& param = m_getter(effect<EffectType>());
            s->*param.mem = newValue;
        });
        emit valueChanged();
    }

    double min() const final override
    {
        const auto& param = m_getter(effect<EffectType>());
        return param.min;
    }

    double max() const final override
    {
        const auto& param = m_getter(effect<EffectType>());
        return param.max;
    }

    double step() const final override
    {
        const auto& param = m_getter(effect<EffectType>());
        return param.step;
    }

    double defaultValue() const final override
    {
        const auto& param = m_getter(effect<EffectType>());
        return param.def;
    }

    QString title() const final override
    {
        const auto it = m_labelMap.find(m_paramId);
        IF_ASSERT_FAILED(it != m_labelMap.end()) {
            return {};
        }
        return it->second.title;
    }

    QString unit() const final override
    {
        const auto it = m_labelMap.find(m_paramId);
        IF_ASSERT_FAILED(it != m_labelMap.end()) {
            return {};
        }
        return it->second.unit;
    }

private:
    const LabelMap m_labelMap;
    const ParamGetter<EffectType> m_getter;
};
}
