/*
 * Audacity: A Digital Audio Editor
 */
#include "wahwahsettingmodel.h"

#include "../common/measureunits.h"

#include "framework/global/translation.h"

namespace au::effects {
WahWahSettingModel::WahWahSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<WahWahEffect>(parent, instanceId, {
        { "Freq", {
              //: Label of the Wahwah effect knob that sets the speed of the low-frequency oscillator (LFO), in hertz
              muse::qtrc("effects/wahwah", "LFO frequency"),
              units::hertz().m_symbol,
              makeSettingAccess(WahWahEffect::Freq) } },
        { "Phase", {
              //: Label of the Wahwah effect knob that sets the start position of the low-frequency oscillator (LFO) cycle, in degrees
              muse::qtrc("effects/wahwah", "LFO start phase"),
              units::degrees().m_symbol,
              makeSettingAccess(WahWahEffect::Phase) } },
        { "Depth", {
              //: Label of the Wahwah effect knob that sets how far the filter frequency moves, in percent
              muse::qtrc("effects/wahwah", "Depth"),
              units::percent().m_symbol,
              makeSettingAccess(WahWahEffect::Depth) } },
        { "Resonance", {
              //: Label of the Wahwah effect knob that sets how sharp the filter peak is. Higher values give a stronger "wah" sound.
              muse::qtrc("effects/wahwah", "Resonance"),
              units::none().m_symbol,
              makeSettingAccess(WahWahEffect::Res) } },
        { "Offset", {
              //: Label of the Wahwah effect knob that moves the whole filter sweep up or down in frequency, in percent
              muse::qtrc("effects/wahwah", "Wah frequency offset"),
              units::percent().m_symbol,
              makeSettingAccess(WahWahEffect::FreqOfs) } },
        { "Gain", {
              //: Label of the Wahwah effect knob that sets the volume of the result, in decibels
              muse::qtrc("effects/wahwah", "Output gain"),
              units::decibels().m_symbol,
              makeSettingAccess(WahWahEffect::OutGain) } },
    })
{
}
}
