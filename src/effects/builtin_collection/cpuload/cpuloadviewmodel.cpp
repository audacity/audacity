/*
 * Audacity: A Digital Audio Editor
 */
#include "cpuloadviewmodel.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include "cpuloadeffect.h"

using namespace au::effects;

CpuLoadViewModel::CpuLoadViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void CpuLoadViewModel::doReload()
{
    const CpuLoadSettings& s = settings<CpuLoadSettings>();

    auto makeItem = [this](const QString& key, const QString& title, const QString& unit, double value, const auto& parameter,
                           const Setter& setter) {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["value"] = value;
        item["unit"] = unit;
        item["min"] = parameter.min;
        item["max"] = parameter.max;
        item["step"] = 1.0;
        m_setters.insert(key, setter);
        return item;
    };

    m_paramsList.clear();
    m_paramsList["Load"] = makeItem("Load", muse::qtrc("effects/cpuload", "Load"), "%", s.load, CpuLoadEffect::Load,
                                    [](CpuLoadSettings& settings, double v) { settings.load = v; });
    m_paramsList["Spike"] = makeItem("Spike", muse::qtrc("effects/cpuload", "Spike"), muse::qtrc("global", "ms"), s.spike,
                                     CpuLoadEffect::Spike, [](CpuLoadSettings& settings, double v) { settings.spike = v; });
    m_paramsList["SpikeInterval"] = makeItem("SpikeInterval", muse::qtrc("effects/cpuload", "Spike every"), muse::qtrc("global", "s"),
                                             s.spikeInterval, CpuLoadEffect::SpikeInterval,
                                             [](CpuLoadSettings& settings, double v) { settings.spikeInterval = v; });

    emit paramsListChanged();
}

QVariantMap CpuLoadViewModel::paramsList() const
{
    return m_paramsList;
}

void CpuLoadViewModel::setParam(const QString& key, double val)
{
    const Setter setter = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(setter) {
        return;
    }
    modifySettings<CpuLoadSettings>([setter, val](EffectSettings& settings) {
        setter(*settings.cast<CpuLoadSettings>(), val);
    });
}
