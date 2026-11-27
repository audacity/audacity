/*
 * Audacity: A Digital Audio Editor
 */
#include "phasersettingmodel.h"

#include "../common/measureunits.h"

#include "framework/global/translation.h"

#include <cmath>

namespace au::effects {
PhaserSettingModel::PhaserSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<PhaserEffect>(parent, instanceId, {
        { "Freq", {
              //: Label of the Phaser effect knob that sets the speed of the low-frequency oscillator (LFO), in hertz
              muse::qtrc("effects/phaser", "LFO frequency"),
              units::hertz().m_symbol,
              makeSettingAccess(PhaserEffect::Freq) } },
        { "Phase", {
              //: Label of the Phaser effect knob that sets the start position of the low-frequency oscillator (LFO) cycle, in degrees
              muse::qtrc("effects/phaser", "LFO start phase"),
              units::degrees().m_symbol,
              makeSettingAccess(PhaserEffect::Phase) } },
        { "Stages", {
              //: Label of the Phaser effect knob that sets the number of filter stages. More stages give a stronger effect.
              muse::qtrc("effects/phaser", "Stages"),
              units::none().m_symbol,
              // The stage count must be even
              makeSettingAccess(PhaserEffect::Stages, [](double v) { return static_cast<double>(std::lround(v) & ~1); }) } },
        { "Depth", {
              //: Label of the Phaser effect knob that sets how far the filter frequency moves
              muse::qtrc("effects/phaser", "Depth"),
              units::none().m_symbol,
              makeSettingAccess(PhaserEffect::Depth) } },
        { "Feedback", {
              //: Label of the Phaser effect knob that sets how much of the output goes back into the input, in percent
              muse::qtrc("effects/phaser", "Feedback"),
              units::percent().m_symbol,
              makeSettingAccess(PhaserEffect::Feedback) } },
        { "DryWet", {
              //: Label of the Phaser effect knob that sets the mix between the original sound (dry) and the processed sound (wet)
              muse::qtrc("effects/phaser", "Dry/wet"),
              units::none().m_symbol,
              makeSettingAccess(PhaserEffect::DryWet) } },
        { "Gain", {
              //: Label of the Phaser effect knob that sets the volume of the result, in decibels
              muse::qtrc("effects/phaser", "Output gain"),
              units::decibels().m_symbol,
              makeSettingAccess(PhaserEffect::OutGain) } },
    })
{
}
}
