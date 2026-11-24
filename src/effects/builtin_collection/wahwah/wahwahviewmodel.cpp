#include "wahwahviewmodel.h"

#include "wahwaheffect.h"
#include "../common/measureunits.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

#include <cmath>

using namespace au::effects;

namespace {
constexpr double FREQ_STEP = 0.01;
constexpr double PHASE_STEP = 1.0;
constexpr double DEPTH_STEP = 1.0;
constexpr double RES_STEP = 0.1;
constexpr double FREQ_OFS_STEP = 1.0;
constexpr double OUT_GAIN_STEP = 0.1;
}

WahWahViewModel::WahWahViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void WahWahViewModel::doReload()
{
    const EffectWahwahSettings& ws = settings<EffectWahwahSettings>();

    auto makeItem
        = [this](const QString& key, const QString& title, const QString& unit, double value, double min, double max, double def,
                 double step, const Setter& s) {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["value"] = value;
        item["unit"] = unit;
        item["min"] = min;
        item["max"] = max;
        item["defaultValue"] = def;
        item["step"] = step;

        m_setters.insert(key, s);

        return item;
    };

    m_paramsList.clear();

    m_paramsList["Freq"] = makeItem("Freq",
                                    //: Label of the Wahwah effect knob that sets the speed of the low-frequency oscillator (LFO), in hertz
                                    muse::qtrc("effects/wahwah", "LFO frequency"),
                                    units::hertz().m_symbol,
                                    ws.mFreq,
                                    WahWahEffect::Freq.min,
                                    WahWahEffect::Freq.max,
                                    WahWahEffect::Freq.def,
                                    FREQ_STEP,
                                    [](EffectWahwahSettings& settings, double v) { settings.mFreq = v; }
                                    );

    m_paramsList["Phase"] = makeItem("Phase",
                                     //: Label of the Wahwah effect knob that sets the start position of the low-frequency oscillator (LFO) cycle, in degrees
                                     muse::qtrc("effects/wahwah", "LFO start phase"),
                                     units::degrees().m_symbol,
                                     ws.mPhase,
                                     WahWahEffect::Phase.min,
                                     WahWahEffect::Phase.max,
                                     WahWahEffect::Phase.def,
                                     PHASE_STEP,
                                     [](EffectWahwahSettings& settings, double v) { settings.mPhase = v; }
                                     );

    m_paramsList["Depth"] = makeItem("Depth",
                                     //: Label of the Wahwah effect knob that sets how far the filter frequency moves, in percent
                                     muse::qtrc("effects/wahwah", "Depth"),
                                     units::percent().m_symbol,
                                     ws.mDepth,
                                     WahWahEffect::Depth.min,
                                     WahWahEffect::Depth.max,
                                     WahWahEffect::Depth.def,
                                     DEPTH_STEP,
                                     [](EffectWahwahSettings& settings, double v) { settings.mDepth = std::lround(v); }
                                     );

    m_paramsList["Resonance"] = makeItem("Resonance",
                                         //: Label of the Wahwah effect knob that sets how sharp the filter peak is. Higher values give a stronger "wah" sound.
                                         muse::qtrc("effects/wahwah", "Resonance"),
                                         units::none().m_symbol,
                                         ws.mRes,
                                         WahWahEffect::Res.min,
                                         WahWahEffect::Res.max,
                                         WahWahEffect::Res.def,
                                         RES_STEP,
                                         [](EffectWahwahSettings& settings, double v) { settings.mRes = v; }
                                         );

    m_paramsList["Offset"] = makeItem("Offset",
                                      //: Label of the Wahwah effect knob that moves the whole filter sweep up or down in frequency, in percent
                                      muse::qtrc("effects/wahwah", "Wah frequency offset"),
                                      units::percent().m_symbol,
                                      ws.mFreqOfs,
                                      WahWahEffect::FreqOfs.min,
                                      WahWahEffect::FreqOfs.max,
                                      WahWahEffect::FreqOfs.def,
                                      FREQ_OFS_STEP,
                                      [](EffectWahwahSettings& settings, double v) { settings.mFreqOfs = std::lround(v); }
                                      );

    m_paramsList["Gain"] = makeItem("Gain",
                                    //: Label of the Wahwah effect knob that sets the volume of the result, in decibels
                                    muse::qtrc("effects/wahwah", "Output gain"),
                                    units::decibels().m_symbol,
                                    ws.mOutGain,
                                    WahWahEffect::OutGain.min,
                                    WahWahEffect::OutGain.max,
                                    WahWahEffect::OutGain.def,
                                    OUT_GAIN_STEP,
                                    [](EffectWahwahSettings& settings, double v) { settings.mOutGain = v; }
                                    );

    emit paramsListChanged();
}

void WahWahViewModel::doUpdateSettings()
{
    doReload();
}

QVariantMap WahWahViewModel::paramsList() const
{
    return m_paramsList;
}

void WahWahViewModel::setParam(const QString& key, double val)
{
    Setter s = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(s) {
        return;
    }
    modifySettings<EffectWahwahSettings>([s, val](EffectSettings& settings) {
        s(*settings.cast<EffectWahwahSettings>(), val);
    });
}
