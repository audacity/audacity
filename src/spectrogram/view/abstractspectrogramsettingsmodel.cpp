/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractspectrogramsettingsmodel.h"
#include "../spectrogramtypes.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include <algorithm>

namespace au::spectrogram {
AbstractSpectrogramSettingsModel::AbstractSpectrogramSettingsModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{}

void AbstractSpectrogramSettingsModel::setMinFreq(int value)
{
    doSetMinFreq(std::clamp(value, frequencyHardMinimum(), frequencyHardMaximum()));
    if (maxFreq() <= minFreq()) {
        doSetMaxFreq(minFreq());
    }
}

void AbstractSpectrogramSettingsModel::setMaxFreq(int value)
{
    doSetMaxFreq(std::clamp(value, frequencyHardMinimum(), frequencyHardMaximum()));
    if (minFreq() >= maxFreq()) {
        doSetMinFreq(maxFreq());
    }
}

int AbstractSpectrogramSettingsModel::frequencyHardMaximum() const
{
    const auto sampleRates = audioDevicesProvider()->sampleRates();
    return static_cast<int>(*std::max_element(sampleRates.begin(), sampleRates.end()) / 2);
}

QString AbstractSpectrogramSettingsModel::colorSchemeName(int scheme) const
{
    switch (static_cast<SpectrogramColorScheme>(scheme)) {
    case SpectrogramColorScheme::Roseus:
        return muse::qtrc("spectrogram/preferences", "Roseus");
    case SpectrogramColorScheme::Classic:
        return muse::qtrc("spectrogram/preferences", "Classic");
    case SpectrogramColorScheme::Grayscale:
        return muse::qtrc("spectrogram/preferences", "Grayscale");
    case SpectrogramColorScheme::InverseGrayscale:
        return muse::qtrc("spectrogram/preferences", "Inverse grayscale");
    default:
        assert(false);
        return muse::qtrc("spectrogram/preferences", "Unknown");
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
        return muse::qtrc("spectrogram/preferences", "Linear");
    case SpectrogramScale::Logarithmic:
        return muse::qtrc("spectrogram/preferences", "Logarithmic");
    case SpectrogramScale::Mel:
        return muse::qtrc("spectrogram/preferences", "Mel");
    case SpectrogramScale::Bark:
        return muse::qtrc("spectrogram/preferences", "Bark");
    case SpectrogramScale::ERB:
        return muse::qtrc("spectrogram/preferences", "ERB");
    case SpectrogramScale::Period:
        return muse::qtrc("spectrogram/preferences", "Period");
    default:
        assert(false);
        return muse::qtrc("spectrogram/preferences", "Unknown");
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
