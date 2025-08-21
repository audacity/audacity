#include "reverbviewmodel.h"

#include "global/translation.h"

#include "reverbeffect.h"

#include "log.h"

using namespace au::effects;

ReverbViewModel::ReverbViewModel()
{
}

void ReverbViewModel::doReload()
{
    const ReverbSettings& rs = settings<ReverbSettings>();

    auto makeItem
        = [this](const QString& key, const QString& title, const QString& unit, double value, double min, double max, const Setter& s) {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["value"] = value;
        item["unit"] = unit;
        item["min"] = min;
        item["max"] = max;

        m_setters.insert(key, s);

        return item;
    };

    m_paramsList.clear();

    m_paramsList["RoomSize"] = makeItem("RoomSize",
                                        muse::qtrc("effects/reverb", "Room size"),
                                        muse::qtrc("global", "%"),
                                        rs.mRoomSize,
                                        ReverbEffect::RoomSize.min,
                                        ReverbEffect::RoomSize.max,
                                        [](ReverbSettings& settings, double v) { settings.mRoomSize = v; }
                                        );

    m_paramsList["PreDelay"] = makeItem("PreDelay",
                                        muse::qtrc("effects/reverb", "Pre-delay"),
                                        muse::qtrc("effects/reverb", "ms"),
                                        rs.mPreDelay,
                                        ReverbEffect::PreDelay.min,
                                        ReverbEffect::PreDelay.max,
                                        [](ReverbSettings& settings, double v) { settings.mPreDelay = v; }
                                        );

    m_paramsList["Reverberance"] = makeItem("Reverberance",
                                            muse::qtrc("effects/reverb", "Reverberance"),
                                            muse::qtrc("global", "%"),
                                            rs.mReverberance,
                                            ReverbEffect::Reverberance.min,
                                            ReverbEffect::Reverberance.max,
                                            [](ReverbSettings& settings, double v) { settings.mReverberance = v; }
                                            );

    m_paramsList["HfDamping"] = makeItem("HfDamping",
                                         muse::qtrc("effects/reverb", "Damping"),
                                         muse::qtrc("global", "%"),
                                         rs.mHfDamping,
                                         ReverbEffect::HfDamping.min,
                                         ReverbEffect::HfDamping.max,
                                         [](ReverbSettings& settings, double v) { settings.mHfDamping = v; }
                                         );

    m_paramsList["ToneLow"] = makeItem("ToneLow",
                                       muse::qtrc("effects/reverb", "Low tone"),
                                       muse::qtrc("global", "%"),
                                       rs.mToneLow,
                                       ReverbEffect::ToneLow.min,
                                       ReverbEffect::ToneLow.max,
                                       [](ReverbSettings& settings, double v) { settings.mToneLow = v; }
                                       );

    m_paramsList["ToneHigh"] = makeItem("ToneHigh",
                                        muse::qtrc("effects/reverb", "High tone"),
                                        muse::qtrc("global", "%"),
                                        rs.mToneHigh,
                                        ReverbEffect::ToneHigh.min,
                                        ReverbEffect::ToneHigh.max,
                                        [](ReverbSettings& settings, double v) { settings.mToneHigh = v; }
                                        );

    m_paramsList["WetGain"] = makeItem("WetGain",
                                       muse::qtrc("effects/reverb", "Wet gain"),
                                       muse::qtrc("global", "dB"),
                                       rs.mWetGain,
                                       ReverbEffect::WetGain.min,
                                       ReverbEffect::WetGain.max,
                                       [](ReverbSettings& settings, double v) { settings.mWetGain = v; }
                                       );

    m_paramsList["DryGain"] = makeItem("DryGain",
                                       muse::qtrc("effects/reverb", "Dry gain"),
                                       muse::qtrc("global", "dB"),
                                       rs.mDryGain,
                                       ReverbEffect::DryGain.min,
                                       ReverbEffect::DryGain.max,
                                       [](ReverbSettings& settings, double v) { settings.mDryGain = v; }
                                       );

    m_paramsList["StereoWidth"] = makeItem("StereoWidth",
                                           muse::qtrc("effects/reverb", "Stereo width"),
                                           muse::qtrc("global", "%"),
                                           rs.mStereoWidth,
                                           ReverbEffect::StereoWidth.min,
                                           ReverbEffect::StereoWidth.max,
                                           [](ReverbSettings& settings, double v) { settings.mStereoWidth = v; }
                                           );

    emit paramsListChanged();
    emit wetOnlyChanged();
}

QVariantMap ReverbViewModel::paramsList() const
{
    return m_paramsList;
}

void ReverbViewModel::setParam(const QString& key, double val)
{
    Setter s = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(s) {
        return;
    }
    modifySettings([s, val](EffectSettings& settings)
    {
        s(*settings.cast<ReverbSettings>(), val);
        // saintmatthieu: Looks like non-null Message returns are only necessary for VST2 and AU effects but I haven't figured out why.
        // In any case, changing the settings during playback gets reflected in the audio output.
    });
}

bool ReverbViewModel::wetOnly() const
{
    return settings<ReverbSettings>().mWetOnly;
}

void ReverbViewModel::setWetOnly(bool newWetOnly)
{
    modifySettings([newWetOnly](EffectSettings& settings)
    {
        settings.cast<ReverbSettings>()->mWetOnly = newWetOnly;
    });
    emit wetOnlyChanged();
}
