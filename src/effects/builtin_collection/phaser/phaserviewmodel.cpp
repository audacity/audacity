#include "phaserviewmodel.h"

#include "phasereffect.h"
#include "../common/measureunits.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

#include <algorithm>
#include <cmath>

using namespace au::effects;

namespace {
constexpr double FREQ_STEP = 0.001;
constexpr double PHASE_STEP = 1.0;
constexpr double STAGES_STEP = 2.0;
constexpr double DEPTH_STEP = 1.0;
constexpr double FEEDBACK_STEP = 1.0;
constexpr double DRY_WET_STEP = 1.0;
constexpr double OUT_GAIN_STEP = 0.1;
}

PhaserViewModel::PhaserViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void PhaserViewModel::doReload()
{
    const EffectPhaserSettings& ps = settings<EffectPhaserSettings>();

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
                                    //: Label of the Phaser effect knob that sets the speed of the low-frequency oscillator (LFO), in hertz
                                    muse::qtrc("effects/phaser", "LFO frequency"),
                                    units::hertz().m_symbol,
                                    ps.mFreq,
                                    PhaserEffect::Freq.min,
                                    PhaserEffect::Freq.max,
                                    PhaserEffect::Freq.def,
                                    FREQ_STEP,
                                    [](EffectPhaserSettings& settings, double v) { settings.mFreq = v; }
                                    );

    m_paramsList["Phase"] = makeItem("Phase",
                                     //: Label of the Phaser effect knob that sets the start position of the low-frequency oscillator (LFO) cycle, in degrees
                                     muse::qtrc("effects/phaser", "LFO start phase"),
                                     units::degrees().m_symbol,
                                     ps.mPhase,
                                     PhaserEffect::Phase.min,
                                     PhaserEffect::Phase.max,
                                     PhaserEffect::Phase.def,
                                     PHASE_STEP,
                                     [](EffectPhaserSettings& settings, double v) { settings.mPhase = v; }
                                     );

    m_paramsList["Stages"] = makeItem("Stages",
                                      //: Label of the Phaser effect knob that sets the number of filter stages. More stages give a stronger effect.
                                      muse::qtrc("effects/phaser", "Stages"),
                                      units::none().m_symbol,
                                      ps.mStages,
                                      PhaserEffect::Stages.min,
                                      PhaserEffect::Stages.max,
                                      PhaserEffect::Stages.def,
                                      STAGES_STEP,
                                      // The stage count must be even
                                      [](EffectPhaserSettings& settings, double v) { settings.mStages = std::lround(v) & ~1; }
                                      );

    m_paramsList["Depth"] = makeItem("Depth",
                                     //: Label of the Phaser effect knob that sets how far the filter frequency moves
                                     muse::qtrc("effects/phaser", "Depth"),
                                     units::none().m_symbol,
                                     ps.mDepth,
                                     PhaserEffect::Depth.min,
                                     PhaserEffect::Depth.max,
                                     PhaserEffect::Depth.def,
                                     DEPTH_STEP,
                                     [](EffectPhaserSettings& settings, double v) { settings.mDepth = std::lround(v); }
                                     );

    m_paramsList["Feedback"] = makeItem("Feedback",
                                        //: Label of the Phaser effect knob that sets how much of the output goes back into the input, in percent
                                        muse::qtrc("effects/phaser", "Feedback"),
                                        units::percent().m_symbol,
                                        ps.mFeedback,
                                        PhaserEffect::Feedback.min,
                                        PhaserEffect::Feedback.max,
                                        PhaserEffect::Feedback.def,
                                        FEEDBACK_STEP,
                                        [](EffectPhaserSettings& settings, double v) { settings.mFeedback = std::lround(v); }
                                        );

    m_paramsList["DryWet"] = makeItem("DryWet",
                                      //: Label of the Phaser effect knob that sets the mix between the original sound (dry) and the processed sound (wet)
                                      muse::qtrc("effects/phaser", "Dry/wet"),
                                      units::none().m_symbol,
                                      ps.mDryWet,
                                      PhaserEffect::DryWet.min,
                                      PhaserEffect::DryWet.max,
                                      PhaserEffect::DryWet.def,
                                      DRY_WET_STEP,
                                      [](EffectPhaserSettings& settings, double v) { settings.mDryWet = std::lround(v); }
                                      );

    m_paramsList["Gain"] = makeItem("Gain",
                                    //: Label of the Phaser effect knob that sets the volume of the result, in decibels
                                    muse::qtrc("effects/phaser", "Output gain"),
                                    units::decibels().m_symbol,
                                    ps.mOutGain,
                                    PhaserEffect::OutGain.min,
                                    PhaserEffect::OutGain.max,
                                    PhaserEffect::OutGain.def,
                                    OUT_GAIN_STEP,
                                    [](EffectPhaserSettings& settings, double v) { settings.mOutGain = v; }
                                    );

    emit paramsListChanged();
}

void PhaserViewModel::doUpdateSettings()
{
    doReload();
}

QVariantMap PhaserViewModel::paramsList() const
{
    return m_paramsList;
}

void PhaserViewModel::setParam(const QString& key, double val)
{
    Setter s = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(s) {
        return;
    }
    const QVariantMap item = m_paramsList.value(key).toMap();
    val = std::clamp(val, item["min"].toDouble(), item["max"].toDouble());
    modifySettings<EffectPhaserSettings>([s, val](EffectSettings& settings) {
        s(*settings.cast<EffectPhaserSettings>(), val);
    });
}
