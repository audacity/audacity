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

double NoiseViewModel::amplitude() const
{
    return settings().amplitude;
}

void NoiseViewModel::setAmplitude(double newAmplitude)
{
    mutSettings().amplitude = newAmplitude;
    emit amplitudeChanged();
}

int NoiseViewModel::type() const
{
    return static_cast<int>(settings().type);
}

void NoiseViewModel::setType(int type)
{
    mutSettings().type = static_cast<NoiseSettings::Type>(type);
    emit typeChanged();
}

NoiseSettings& NoiseViewModel::mutSettings()
{
    return NoiseGenerator::GetSettings(*AbstractEffectModel::settings());
}

const NoiseSettings& NoiseViewModel::settings() const
{
    return NoiseGenerator::GetSettings(*AbstractEffectModel::settings());
}
