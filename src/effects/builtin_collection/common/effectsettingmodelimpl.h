/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "builtineffectsettingmodel.h"
#include "settingaccess.h"
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

    struct Setting {
        const QString title;
        const QString unit;
        const SettingAccess<SettingsType> access;
    };

    using SettingMap = std::unordered_map<QString /*param ID*/, Setting>;

    //! For settings that are all doubles
    EffectSettingModelImpl(QObject* parent, int instanceId, LabelMap labelMap, ParamGetter<EffectType> getter)
        : BuiltinEffectSettingModel{parent, instanceId}, m_labelMap{std::move(labelMap)}, m_getter{std::move(getter)} {}

    //! For settings of any numeric type
    EffectSettingModelImpl(QObject* parent, int instanceId, const SettingMap& settingMap)
        : BuiltinEffectSettingModel{parent, instanceId}, m_labelMap{labelMap(settingMap)}, m_accessMap{accessMap(settingMap)} {}

    double value() const final override
    {
        return access().get(settings<SettingsType>());
    }

    void setValue(double newValue) final override
    {
        if (muse::is_equal(value(), newValue)) {
            return;
        }
        modifySettings<SettingsType>([this, newValue](EffectSettings& settings) {
            auto* s = settings.cast<SettingsType>();
            IF_ASSERT_FAILED(s) {
                return;
            }
            access().set(*s, newValue);
        });
        emit valueChanged();
    }

    double min() const final override { return access().min; }
    double max() const final override { return access().max; }
    double step() const final override { return access().step; }
    int decimals() const final override { return access().decimals; }
    double defaultValue() const final override { return access().def; }

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
    using AccessMap = std::unordered_map<QString /*param ID*/, SettingAccess<SettingsType> >;

    static LabelMap labelMap(const SettingMap& settingMap)
    {
        LabelMap labels;
        for (const auto& [paramId, setting] : settingMap) {
            labels.emplace(paramId, Labels { setting.title, setting.unit });
        }
        return labels;
    }

    static AccessMap accessMap(const SettingMap& settingMap)
    {
        AccessMap accesses;
        for (const auto& [paramId, setting] : settingMap) {
            accesses.emplace(paramId, setting.access);
        }
        return accesses;
    }

    SettingAccess<SettingsType> access() const
    {
        if (m_getter) {
            return makeSettingAccess(m_getter(effect<EffectType>()));
        }
        const auto it = m_accessMap.find(m_paramId);
        IF_ASSERT_FAILED(it != m_accessMap.end()) {
            return { [](const SettingsType&) { return 0.0; }, [](SettingsType&, double) {} };
        }
        return it->second;
    }

    const LabelMap m_labelMap;
    const ParamGetter<EffectType> m_getter;
    const AccessMap m_accessMap;
};
}
