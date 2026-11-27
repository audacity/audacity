/*
 * Audacity: A Digital Audio Editor
 */
#include "reverbsettingmodel.h"

#include "framework/global/translation.h"

namespace au::effects {
ReverbSettingModel::ReverbSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<ReverbEffect>(parent, instanceId, {
        { "RoomSize", {
              muse::qtrc("effects/reverb", "Room size"),
              //: Percent sign, used as a unit suffix
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::RoomSize) } },
        { "PreDelay", {
              muse::qtrc("effects/reverb", "Pre-delay"),
              muse::qtrc("effects/reverb", "ms"),
              makeSettingAccess(ReverbEffect::PreDelay) } },
        { "Reverberance", {
              muse::qtrc("effects/reverb", "Reverberance"),
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::Reverberance) } },
        { "HfDamping", {
              muse::qtrc("effects/reverb", "Damping"),
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::HfDamping) } },
        { "ToneLow", {
              muse::qtrc("effects/reverb", "Low tone"),
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::ToneLow) } },
        { "ToneHigh", {
              muse::qtrc("effects/reverb", "High tone"),
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::ToneHigh) } },
        { "WetGain", {
              muse::qtrc("effects/reverb", "Wet gain"),
              muse::qtrc("global", "dB"),
              makeSettingAccess(ReverbEffect::WetGain) } },
        { "DryGain", {
              muse::qtrc("effects/reverb", "Dry gain"),
              muse::qtrc("global", "dB"),
              makeSettingAccess(ReverbEffect::DryGain) } },
        { "StereoWidth", {
              muse::qtrc("effects/reverb", "Stereo width"),
              muse::qtrc("global", "%"),
              makeSettingAccess(ReverbEffect::StereoWidth) } },
        { "WetOnly", {
              muse::qtrc("effects/reverb", "Wet only"),
              QString(),
              makeSettingAccess(ReverbEffect::WetOnly) } },
    })
{
}
}
