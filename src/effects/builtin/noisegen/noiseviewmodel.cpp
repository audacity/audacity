/*
 * Audacity: A Digital Audio Editor
 */
#include "noiseviewmodel.h"
#include "noisegenerator.h"

#include "global/translation.h"

using namespace au::effects;

bool NoiseViewModel::isApplyAllowed() const
{
    return settings<NoiseSettings>().isApplyAllowed() && GeneratorEffectModel::isApplyAllowed();
}

QVariantList NoiseViewModel::types() const
{
    static QVariantList items;
    if (items.empty()) {
        items.append(QVariantMap {
            { "text", muse::qtrc("effects/noise", "White", "not a color, but 'white noise' having a uniform spectrum") },
            { "value", (int)NoiseSettings::Type::White }
        });
        items.append(QVariantMap {
            { "text",
              muse::qtrc("effects/noise", "Pink", "not a color, but 'pink noise' having a spectrum with more power in low frequencies") },
            { "value", (int)NoiseSettings::Type::Pink }
        });
        items.append(QVariantMap {
            { "text", muse::qtrc("effects/noise", "Brownian", "a kind of noise spectrum also known as 'red' or 'brown'") },
            { "value", (int)NoiseSettings::Type::Brownian }
        });
    }
    return items;
}

void NoiseViewModel::doEmitSignals()
{
    emit amplitudeChanged();
    emit typeChanged();
    emit isApplyAllowedChanged();
}

double NoiseViewModel::amplitude() const
{
    return settings<NoiseSettings>().amplitude;
}

void NoiseViewModel::prop_setAmplitude(double newAmplitude)
{
    if (!m_inited) {
        return;
    }

    bool wasAllowed = isApplyAllowed();

    mutSettings<NoiseSettings>().amplitude = newAmplitude;

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

int NoiseViewModel::type() const
{
    return static_cast<int>(settings<NoiseSettings>().type);
}

void NoiseViewModel::prop_setType(int type)
{
    if (!m_inited) {
        return;
    }

    bool wasAllowed = isApplyAllowed();

    NoiseSettings::Type noiseType = static_cast<NoiseSettings::Type>(type);

    DO_ASSERT(noiseType < NoiseSettings::Type::Count);

    mutSettings<NoiseSettings>().type = noiseType;
    emit typeChanged();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}
