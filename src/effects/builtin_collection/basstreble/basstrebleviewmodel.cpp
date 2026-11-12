#include "basstrebleviewmodel.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include "basstrebleeffect.h"

using namespace au::effects;

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
            settings.mGain = this->compensateGain(settings.mGain, settings.mBass, v);
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
            settings.mGain = this->compensateGain(settings.mGain, settings.mTreble, v);
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
            double delta = v - settings.mGain;
            double treble = settings.mTreble - delta;
            double clampedTreble = std::clamp(treble, BassTrebleEffect::Treble.min, BassTrebleEffect::Treble.max);
            double bass = settings.mBass - delta;
            double clampedBass = std::clamp(bass, BassTrebleEffect::Bass.min, BassTrebleEffect::Bass.max);
            // If the delta cannot be split equally between treble and bass, apply the remaning delta to the other tone.
            settings.mTreble
                = std::clamp(settings.mTreble - delta + (bass - clampedBass), BassTrebleEffect::Treble.min, BassTrebleEffect::Treble.max);
            settings.mBass
                = std::clamp(settings.mBass - delta + (treble - clampedTreble), BassTrebleEffect::Bass.min, BassTrebleEffect::Bass.max);
            m_ignores.insert((QString)(char*)BassTrebleEffect::Treble.key, true);
            m_ignores.insert((QString)(char*)BassTrebleEffect::Bass.key, true);
        }
        settings.mGain = v;
    }
                                    );

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
            s->mGain = std::clamp(-(s->mBass + s->mTreble) / 2, BassTrebleEffect::Gain.min, BassTrebleEffect::Gain.max);
        }
    });
    emit linkChanged();
}

double BassTrebleViewModel::compensateGain(const double currentGain, double currentValue, double newValue)
{
    return std::clamp(currentGain - (newValue - currentValue) / 2, BassTrebleEffect::Gain.min, BassTrebleEffect::Gain.max);
}
