#include "basstrebleviewmodel.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include <algorithm>

#include "basstrebleeffect.h"

using namespace au::effects;

namespace {
// AU3 loudness compensation: boosts count at half weight, cuts at quarter weight.
// With "auto-adjust volume" on, `Gain + toneWeight(Bass) + toneWeight(Treble)` is kept constant.
double toneWeight(double v)
{
    return v > 0 ? v / 2.0 : v / 4.0;
}

double toneFromWeight(double w)
{
    return w > 0 ? 2.0 * w : 4.0 * w;
}

// With auto-adjust volume on, Gain is limited to what Bass and Treble can compensate for.
double gainMin(bool link)
{
    using au::effects::BassTrebleEffect;
    return link
           ? std::max(-(toneWeight(BassTrebleEffect::Bass.max) + toneWeight(BassTrebleEffect::Treble.max)), BassTrebleEffect::Gain.min)
           : BassTrebleEffect::Gain.min;
}

double gainMax(bool link)
{
    using au::effects::BassTrebleEffect;
    return link
           ? std::min(-(toneWeight(BassTrebleEffect::Bass.min) + toneWeight(BassTrebleEffect::Treble.min)), BassTrebleEffect::Gain.max)
           : BassTrebleEffect::Gain.max;
}
}

BassTrebleViewModel::BassTrebleViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void BassTrebleViewModel::doReload()
{
    const BassTrebleSettings& bs = settings<BassTrebleSettings>();

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

    m_paramsList["Bass"] = makeItem("Bass",
                                    muse::qtrc("effects/basstreble", "Bass"),
                                    muse::qtrc("global", "dB"),
                                    bs.mBass,
                                    BassTrebleEffect::Bass.min,
                                    BassTrebleEffect::Bass.max,
                                    [this](BassTrebleSettings& settings, double v) {
        // Changes ignored to avoid parameter set loop when values are linked.
        if (m_ignores.value((QString)(char*)BassTrebleEffect::Bass.key, false)) {
            m_ignores.insert((QString)(char*)BassTrebleEffect::Bass.key, false);
            return;
        }
        if (settings.mLink) {
            settings.mGain = compensateGain(settings.mGain, settings.mBass, v);
            m_ignores.insert((QString)(char*)BassTrebleEffect::Gain.key, true);
        }
        settings.mBass = v;
    }
                                    );

    m_paramsList["Treble"] = makeItem("Treble",
                                      muse::qtrc("effects/basstreble", "Treble"),
                                      muse::qtrc("global", "dB"),
                                      bs.mTreble,
                                      BassTrebleEffect::Treble.min,
                                      BassTrebleEffect::Treble.max,
                                      [this](BassTrebleSettings& settings, double v) {
        if (m_ignores.value((QString)(char*)BassTrebleEffect::Treble.key, false)) {
            m_ignores.insert((QString)(char*)BassTrebleEffect::Treble.key, false);
            return;
        }
        if (settings.mLink) {
            settings.mGain = compensateGain(settings.mGain, settings.mTreble, v);
            m_ignores.insert((QString)(char*)BassTrebleEffect::Gain.key, true);
        }
        settings.mTreble = v;
    }
                                      );

    m_paramsList["Gain"] = makeItem("Gain",
                                    muse::qtrc("effects/basstreble", "Output volume"),
                                    muse::qtrc("global", "dB"),
                                    bs.mGain,
                                    BassTrebleEffect::Gain.min,
                                    BassTrebleEffect::Gain.max,
                                    [this](BassTrebleSettings& settings, double v) {
        if (m_ignores.value((QString)(char*)BassTrebleEffect::Gain.key, false)) {
            m_ignores.insert((QString)(char*)BassTrebleEffect::Gain.key, false);
            return;
        }
        if (settings.mLink) {
            // Split the gain delta evenly between bass and treble in weighted (compensated) space.
            // If one tone hits its limit, the remainder goes to the other.
            // Gain is limited to what Bass and Treble can compensate for, otherwise the
            // Gain + weight(Bass) + weight(Treble) relation would be lost once the tones saturate.
            v = std::clamp(v, gainMin(true), gainMax(true));
            const double delta = v - settings.mGain;
            const double bassMin = toneWeight(BassTrebleEffect::Bass.min);
            const double bassMax = toneWeight(BassTrebleEffect::Bass.max);
            const double trebleMin = toneWeight(BassTrebleEffect::Treble.min);
            const double trebleMax = toneWeight(BassTrebleEffect::Treble.max);
            const double bass = toneWeight(settings.mBass) - delta / 2;
            const double treble = toneWeight(settings.mTreble) - delta / 2;
            const double clampedBass = std::clamp(bass, bassMin, bassMax);
            const double clampedTreble = std::clamp(treble, trebleMin, trebleMax);
            settings.mTreble = toneFromWeight(std::clamp(treble + (bass - clampedBass), trebleMin, trebleMax));
            settings.mBass = toneFromWeight(std::clamp(bass + (treble - clampedTreble), bassMin, bassMax));
            m_ignores.insert((QString)(char*)BassTrebleEffect::Treble.key, true);
            m_ignores.insert((QString)(char*)BassTrebleEffect::Bass.key, true);
        }
        settings.mGain = v;
    }
                                    );

    // The knob keeps one fixed scale, but can't be moved past what Bass and Treble can compensate for.
    QVariantMap gainItem = m_paramsList["Gain"].toMap();
    gainItem["limitMin"] = gainMin(bs.mLink);
    gainItem["limitMax"] = gainMax(bs.mLink);
    m_paramsList["Gain"] = gainItem;

    emit paramsListChanged();
    emit linkChanged();
}

QVariantMap BassTrebleViewModel::paramsList() const
{
    return m_paramsList;
}

void BassTrebleViewModel::setParam(const QString& key, double val)
{
    Setter s = m_setters.value(key, nullptr);
    IF_ASSERT_FAILED(s) {
        return;
    }
    modifySettings<BassTrebleSettings>([s, val](EffectSettings& settings)
    {
        s(*settings.cast<BassTrebleSettings>(), val);
    });
}

bool BassTrebleViewModel::link() const
{
    return settings<BassTrebleSettings>().mLink;
}

void BassTrebleViewModel::setLink(bool newLink)
{
    modifySettings<BassTrebleSettings>([newLink](EffectSettings& settings)
    {
        BassTrebleSettings* s = settings.cast<BassTrebleSettings>();
        s->mLink = newLink;
        if (s->mLink) {
            s->mGain = std::clamp(-(toneWeight(s->mBass) + toneWeight(s->mTreble)), BassTrebleEffect::Gain.min, BassTrebleEffect::Gain.max);
        }
    });
    emit linkChanged();
}

double BassTrebleViewModel::compensateGain(const double currentGain, double currentValue, double newValue)
{
    const double gain = currentGain - (toneWeight(newValue) - toneWeight(currentValue));
    return std::clamp(gain, BassTrebleEffect::Gain.min, BassTrebleEffect::Gain.max);
}
