/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstracteffectsettingmodel.h"
#include "libraries/lib-components/SettingsVisitor.h"

#include <functional>

namespace au::effects {
template<typename EffectType>
using ParamGetter = std::function<const EffectParameter<typename EffectType::Settings, double, double, double>& (const EffectType&)>;

template<typename EffectType>
class EffectSettingModelImpl : public AbstractEffectSettingModel
{
public:
    using SettingsType = typename EffectType::Settings;

    EffectSettingModelImpl(QObject* parent, ParamGetter<EffectType> getter)
        : AbstractEffectSettingModel{parent}, m_getter(std::move(getter)) {}

    double value() const override
    {
        const auto* effect = dynamic_cast<const EffectType*>(this->effect());
        if (!effect) {
            return 0.0;
        }
        const auto& s = settings<SettingsType>();
        const auto& param = m_getter(*effect);
        const double v = s.*param.mem;
        return v;
    }

    void setValue(double newValue) override
    {
        if (muse::is_equal(value(), newValue)) {
            return;
        }
        modifySettings([this, newValue](EffectSettings& settings) {
            auto* s = settings.cast<SettingsType>();
            IF_ASSERT_FAILED(s) {
                return;
            }
            const auto* effect = dynamic_cast<const EffectType*>(this->effect());
            if (!effect) {
                return;
            }
            const auto& param = m_getter(*effect);
            s->*param.mem = newValue;
        });
        emit valueChanged();
    }

    double min() const override
    {
        const auto* effect = dynamic_cast<const EffectType*>(this->effect());
        if (!effect) {
            return 0.0;
        }
        const auto& param = m_getter(*effect);
        return param.min;
    }

    double max() const override
    {
        const auto* effect = dynamic_cast<const EffectType*>(this->effect());
        if (!effect) {
            return 1.0;
        }
        const auto& param = m_getter(*effect);
        return param.max;
    }

    double step() const override
    {
        const auto* effect = dynamic_cast<const EffectType*>(this->effect());
        if (!effect) {
            return 1.0;
        }
        const auto& param = m_getter(*effect);
        return param.step;
    }

private:
    const ParamGetter<EffectType> m_getter;
};
}
