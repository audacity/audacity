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
    const ReverbSettings &rs = settings<ReverbSettings>();

    auto makeItem = [this](const QString &key, const QString &title,
                           const QString &
                               tooltip,
                           double value, double min, double max,
                           const Setter &s)
    {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["tooltip"] = tooltip;
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
                             muse::qtrc("effects",
                                        "Sets the size of the simulated room"),
                             rs.mRoomSize,
                             ReverbEffect::RoomSize.min,
                             ReverbEffect::RoomSize.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mRoomSize = v;
                             });

    m_paramsList << makeItem("PreDelay",
                             muse::qtrc("effects", "Pre-delay (ms)"),
                             muse::qtrc("effects",
                                        "Sets the delay before the reverb effect starts"),
                             rs.mPreDelay,
                             ReverbEffect::PreDelay.min,
                             ReverbEffect::PreDelay.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mPreDelay = v;
                             });

    m_paramsList << makeItem("Reverberance",
                             muse::qtrc("effects", "Reverberance (%)"),
                             muse::qtrc("effects",
                                        "Sets the length of the reverberation tail"),
                             rs.mReverberance,
                             ReverbEffect::Reverberance.min,
                             ReverbEffect::Reverberance.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mReverberance = v;
                             });

    m_paramsList << makeItem("HfDamping",
                             muse::qtrc("effects", "Damping (%)"),
                             muse::qtrc("effects",
                                        "Sets the damping of the high frequencies for a more \"muted\" effect"),
                             rs.mHfDamping,
                             ReverbEffect::HfDamping.min,
                             ReverbEffect::HfDamping.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mHfDamping = v;
                             });

    m_paramsList << makeItem("ToneLow",
                             muse::qtrc("effects", "Tone Low (%)"),
                             muse::qtrc("effects",
                                        "Controls the low frequencies, creating a more \"boomy\" effect"),
                             rs.mToneLow,
                             ReverbEffect::ToneLow.min,
                             ReverbEffect::ToneLow.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mToneLow = v;
                             });

    m_paramsList << makeItem("ToneHigh",
                             muse::qtrc("effects", "Tone High (%)"),
                             muse::qtrc("effects",
                                        "Controls the high frequencies, creating a more \"bright\" effect"),
                             rs.mToneHigh,
                             ReverbEffect::ToneHigh.min,
                             ReverbEffect::ToneHigh.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mToneHigh = v;
                             });

    m_paramsList << makeItem("WetGain",
                             muse::qtrc("effects", "Wet Gain (dB)"),
                             muse::qtrc("effects",
                                        "Sets the volume of the reverberaed signal"),
                             rs.mWetGain,
                             ReverbEffect::WetGain.min,
                             ReverbEffect::WetGain.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mWetGain = v;
                             });

    m_paramsList << makeItem("DryGain",
                             muse::qtrc("effects", "Dry Gain (dB)"),
                             muse::qtrc("effects",
                                        "Applies volume adjustment to the original (\"dry\") audio in the mix. Increasing this value relative to the \"Wet Gain\" (above) reduces the strength of the reverb. If the Wet Gain and Dry Gain values are the same, then the mix of wet effect and dry audio to be output to the track will be made louder or softer by exactly this value (assuming \"Wet Only\" below is not checked)."),
                             rs.mDryGain,
                             ReverbEffect::DryGain.min,
                             ReverbEffect::DryGain.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mDryGain = v;
                             });

    m_paramsList << makeItem("StereoWidth",
                             muse::qtrc("effects", "Stereo Width (%)"),
                             muse::qtrc("effects",
                                        "Sets the apparent width of the effect, making it feel more \"surround\""),
                             rs.mStereoWidth,
                             ReverbEffect::StereoWidth.min,
                             ReverbEffect::StereoWidth.max,
                             [this](ReverbSettings &settings, double v)
                             {
                                 settings.mStereoWidth = v;
                             });

    emit paramsListChanged();
    emit wetOnlyChanged();
}

QVariantList ReverbViewModel::paramsList() const
{
    return m_paramsList;
}

void ReverbViewModel::setParam(const QString &key, double val)
{
    Setter s = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(s)
    {
        return;
    }
    modifySettings([s, val](EffectSettings &settings)
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
    modifySettings([newWetOnly](EffectSettings &settings)
                   { settings.cast<ReverbSettings>()->mWetOnly = newWetOnly; });
    emit wetOnlyChanged();
}
