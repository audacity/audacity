/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractspectrogramsettingsmodel.h"
#include "../spectrogramtypes.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include <limits>

namespace au::spectrogram {
AbstractSpectrogramSettingsModel::AbstractSpectrogramSettingsModel(QObject* parent)
    : QObject(parent)
{}

QString AbstractSpectrogramSettingsModel::colorSchemeName(int scheme) const
{
    switch (static_cast<SpectrogramColorScheme>(scheme)) {
    case SpectrogramColorScheme::Roseus:
        return "Roseus";
    case SpectrogramColorScheme::Classic:
        return "Classic";
    case SpectrogramColorScheme::Grayscale:
        return "Grayscale";
    case SpectrogramColorScheme::InverseGrayscale:
        return "Inverse Grayscale";
    default:
        assert(false);
        return "Unknown";
    }
}

QList<QString> AbstractSpectrogramSettingsModel::colorSchemeNames() const
{
    QList<QString> names;
    const auto numSchemes = static_cast<int>(SpectrogramColorScheme::_count);
    names.reserve(numSchemes);
    for (int i = 0; i < numSchemes; ++i) {
        names.push_back(colorSchemeName(i));
    }
    return names;
}

QString AbstractSpectrogramSettingsModel::scaleName(int scale) const
{
    switch (static_cast<SpectrogramScale>(scale)) {
    case SpectrogramScale::Linear:
        return "Linear";
    case SpectrogramScale::Logarithmic:
        return "Logarithmic";
    case SpectrogramScale::Mel:
        return "Mel";
    case SpectrogramScale::Bark:
        return "Bark";
    case SpectrogramScale::ERB:
        return "ERB";
    case SpectrogramScale::Period:
        return "Period";
    default:
        assert(false);
        return "Unknown";
    }
}

QList<QString> AbstractSpectrogramSettingsModel::scaleNames() const
{
    QList<QString> names;
    const auto numScales = static_cast<int>(SpectrogramScale::_count);
    names.reserve(numScales);
    for (int i = 0; i < numScales; ++i) {
        names.push_back(scaleName(i));
    }
    return names;
}
}
