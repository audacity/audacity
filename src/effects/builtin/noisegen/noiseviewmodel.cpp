/*
 * Audacity: A Digital Audio Editor
 */
#include "noiseviewmodel.h"

#include "global/translation.h"

using namespace au::effects;

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
}

double NoiseViewModel::amplitude() const
{
    return settings<NoiseSettings>().amplitude;
}

void NoiseViewModel::setAmplitude(double newAmplitude)
{
    mutSettings<NoiseSettings>().amplitude = newAmplitude;
    emit amplitudeChanged();
}

int NoiseViewModel::type() const
{
    return static_cast<int>(settings<NoiseSettings>().type);
}

void NoiseViewModel::setType(int type)
{
    mutSettings<NoiseSettings>().type = static_cast<NoiseSettings::Type>(type);
    emit typeChanged();
}
