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

    auto makeItem = [this](const QString& key, const QString& title, double value, double min, double max, const Setter& s) {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["value"] = value;
        item["min"] = min;
        item["max"] = max;

        m_setters.insert(key, s);

        return item;
    };

    m_paramsList.clear();
    emit paramsListChanged();

    m_paramsList << makeItem("RoomSize",
                             muse::qtrc("effects", "Room Size (%)"),
                             rs.mRoomSize,
                             ReverbEffect::RoomSize.min,
                             ReverbEffect::RoomSize.max,
                             [this](ReverbSettings& settings, double v) { settings.mRoomSize = v; }
                             );

    m_paramsList << makeItem("PreDelay",
                             muse::qtrc("effects", "Pre-delay (ms)"),
                             rs.mPreDelay,
                             ReverbEffect::PreDelay.min,
                             ReverbEffect::PreDelay.max,
                             [this](ReverbSettings& settings, double v) { settings.mPreDelay = v; }
                             );

    m_paramsList << makeItem("Reverberance",
                             muse::qtrc("effects", "Reverberance (%)"),
                             rs.mReverberance,
                             ReverbEffect::Reverberance.min,
                             ReverbEffect::Reverberance.max,
                             [this](ReverbSettings& settings, double v) { settings.mReverberance = v; }
                             );

    m_paramsList << makeItem("HfDamping",
                             muse::qtrc("effects", "Damping (%)"),
                             rs.mHfDamping,
                             ReverbEffect::HfDamping.min,
                             ReverbEffect::HfDamping.max,
                             [this](ReverbSettings& settings, double v) { settings.mHfDamping = v; }
                             );

    m_paramsList << makeItem("ToneLow",
                             muse::qtrc("effects", "Tone Low (%)"),
                             rs.mToneLow,
                             ReverbEffect::ToneLow.min,
                             ReverbEffect::ToneLow.max,
                             [this](ReverbSettings& settings, double v) { settings.mToneLow = v; }
                             );

    m_paramsList << makeItem("ToneHigh",
                             muse::qtrc("effects", "Tone High (%)"),
                             rs.mToneHigh,
                             ReverbEffect::ToneHigh.min,
                             ReverbEffect::ToneHigh.max,
                             [this](ReverbSettings& settings, double v) { settings.mToneHigh = v; }
                             );

    m_paramsList << makeItem("WetGain",
                             muse::qtrc("effects", "Wet Gain (dB)"),
                             rs.mWetGain,
                             ReverbEffect::WetGain.min,
                             ReverbEffect::WetGain.max,
                             [this](ReverbSettings& settings, double v) { settings.mWetGain = v; }
                             );

    m_paramsList << makeItem("DryGain",
                             muse::qtrc("effects", "Dry Gain (dB)"),
                             rs.mDryGain,
                             ReverbEffect::DryGain.min,
                             ReverbEffect::DryGain.max,
                             [this](ReverbSettings& settings, double v) { settings.mDryGain = v; }
                             );

    m_paramsList << makeItem("StereoWidth",
                             muse::qtrc("effects", "Stereo Width (%)"),
                             rs.mStereoWidth,
                             ReverbEffect::StereoWidth.min,
                             ReverbEffect::StereoWidth.max,
                             [this](ReverbSettings& settings, double v) { settings.mStereoWidth = v; }
                             );

    emit paramsListChanged();
    emit wetOnlyChanged();
}

QVariantList ReverbViewModel::paramsList() const
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
