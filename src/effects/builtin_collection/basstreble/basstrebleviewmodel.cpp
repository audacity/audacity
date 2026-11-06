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
        = [this](const QString& key, const QString& title, const QString& unit, double value, double min, double max, double step,
                 const Setter& s) {
        QVariantMap item;
        item["key"] = key;
        item["title"] = title;
        item["value"] = value;
        item["unit"] = unit;
        item["min"] = min;
        item["max"] = max;
        item["step"] = step;
        item["decimals"] = 1;

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
                                    BassTrebleEffect::Bass.step,
                                    [](BassTrebleSettings& settings, double v) { settings.mBass = v; }
                                    );

    m_paramsList["Treble"] = makeItem("Treble",
                                      muse::qtrc("effects/basstreble", "Treble"),
                                      muse::qtrc("global", "dB"),
                                      bs.mTreble,
                                      BassTrebleEffect::Treble.min,
                                      BassTrebleEffect::Treble.max,
                                      BassTrebleEffect::Treble.step,
                                      [](BassTrebleSettings& settings, double v) { settings.mTreble = v; }
                                      );

    m_paramsList["Gain"] = makeItem("Gain",
                                    muse::qtrc("effects/basstreble", "Output volume"),
                                    muse::qtrc("global", "dB"),
                                    bs.mGain,
                                    BassTrebleEffect::Gain.min,
                                    BassTrebleEffect::Gain.max,
                                    BassTrebleEffect::Gain.step,
                                    [](BassTrebleSettings& settings, double v) { settings.mGain = v; }
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
        settings.cast<BassTrebleSettings>()->mLink = newLink;
    });
    emit linkChanged();
}
