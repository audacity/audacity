/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/spectrogramtypes.h"
#include "spectrogram/internal/au3/SpectrogramSettings.h" // for now, track settings provider coming

#include "au3-fft/FFT.h" // eWindowFunctions

namespace au::spectrogram {
constexpr auto toAu3ColorScheme(spectrogram::SpectrogramColorScheme scheme)
{
    switch (scheme) {
    case spectrogram::SpectrogramColorScheme::Roseus:
        return spectrogram::SpectrogramSettings::ColorScheme::csColorNew;
    case spectrogram::SpectrogramColorScheme::Classic:
        return spectrogram::SpectrogramSettings::ColorScheme::csColorTheme;
    case spectrogram::SpectrogramColorScheme::Grayscale:
        return spectrogram::SpectrogramSettings::ColorScheme::csGrayscale;
    case spectrogram::SpectrogramColorScheme::InverseGrayscale:
        return spectrogram::SpectrogramSettings::ColorScheme::csInvGrayscale;
    default:
        assert(false);
        return spectrogram::SpectrogramSettings::ColorScheme::csColorNew;
    }
}

constexpr int fromAu3ColorScheme(spectrogram::SpectrogramSettings::ColorScheme scheme)
{
    switch (scheme) {
    case spectrogram::SpectrogramSettings::ColorScheme::csColorNew:
        return static_cast<int>(spectrogram::SpectrogramColorScheme::Roseus);
    case spectrogram::SpectrogramSettings::ColorScheme::csColorTheme:
        return static_cast<int>(spectrogram::SpectrogramColorScheme::Classic);
    case spectrogram::SpectrogramSettings::ColorScheme::csGrayscale:
        return static_cast<int>(spectrogram::SpectrogramColorScheme::Grayscale);
    case spectrogram::SpectrogramSettings::ColorScheme::csInvGrayscale:
        return static_cast<int>(spectrogram::SpectrogramColorScheme::InverseGrayscale);
    default:
        assert(false);
        return 0;
    }
}

constexpr auto toAu3Scale(spectrogram::SpectrogramScale scale)
{
    switch (scale) {
    case spectrogram::SpectrogramScale::Linear:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stLinear;
    case spectrogram::SpectrogramScale::Logarithmic:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stLogarithmic;
    case spectrogram::SpectrogramScale::Mel:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stMel;
    case spectrogram::SpectrogramScale::Bark:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stBark;
    case spectrogram::SpectrogramScale::ERB:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stErb;
    case spectrogram::SpectrogramScale::Period:
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stPeriod;
    default:
        assert(false);
        return spectrogram::SpectrogramSettings::ScaleTypeValues::stLinear;
    }
}

constexpr int fromAu3Scale(spectrogram::SpectrogramSettings::ScaleTypeValues scale)
{
    switch (scale) {
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stLinear:
        return static_cast<int>(spectrogram::SpectrogramScale::Linear);
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stLogarithmic:
        return static_cast<int>(spectrogram::SpectrogramScale::Logarithmic);
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stMel:
        return static_cast<int>(spectrogram::SpectrogramScale::Mel);
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stBark:
        return static_cast<int>(spectrogram::SpectrogramScale::Bark);
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stErb:
        return static_cast<int>(spectrogram::SpectrogramScale::ERB);
    case spectrogram::SpectrogramSettings::ScaleTypeValues::stPeriod:
        return static_cast<int>(spectrogram::SpectrogramScale::Period);
    default:
        assert(false);
        return 0;
    }
}

constexpr auto toAu3WindowType(spectrogram::SpectrogramWindowType windowType)
{
    switch (windowType) {
    case spectrogram::SpectrogramWindowType::Rectangular:
        return eWindowFunctions::eWinFuncRectangular;
    case spectrogram::SpectrogramWindowType::Bartlett:
        return eWindowFunctions::eWinFuncBartlett;
    case spectrogram::SpectrogramWindowType::Hamming:
        return eWindowFunctions::eWinFuncHamming;
    case spectrogram::SpectrogramWindowType::Hann:
        return eWindowFunctions::eWinFuncHann;
    case spectrogram::SpectrogramWindowType::Blackman:
        return eWindowFunctions::eWinFuncBlackman;
    case spectrogram::SpectrogramWindowType::BlackmanHarris:
        return eWindowFunctions::eWinFuncBlackmanHarris;
    case spectrogram::SpectrogramWindowType::Welch:
        return eWindowFunctions::eWinFuncWelch;
    case spectrogram::SpectrogramWindowType::Gaussian25:
        return eWindowFunctions::eWinFuncGaussian25;
    case spectrogram::SpectrogramWindowType::Gaussian35:
        return eWindowFunctions::eWinFuncGaussian35;
    case spectrogram::SpectrogramWindowType::Gaussian45:
        return eWindowFunctions::eWinFuncGaussian45;
    default:
        assert(false);
        return static_cast<eWindowFunctions>(0);
    }
}

constexpr int fromAu3WindowType(eWindowFunctions windowType)
{
    switch (windowType) {
    case eWindowFunctions::eWinFuncRectangular:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Rectangular);
    case eWindowFunctions::eWinFuncBartlett:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Bartlett);
    case eWindowFunctions::eWinFuncHamming:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Hamming);
    case eWindowFunctions::eWinFuncHann:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Hann);
    case eWindowFunctions::eWinFuncBlackman:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Blackman);
    case eWindowFunctions::eWinFuncBlackmanHarris:
        return static_cast<int>(spectrogram::SpectrogramWindowType::BlackmanHarris);
    case eWindowFunctions::eWinFuncWelch:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Welch);
    case eWindowFunctions::eWinFuncGaussian25:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Gaussian25);
    case eWindowFunctions::eWinFuncGaussian35:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Gaussian35);
    case eWindowFunctions::eWinFuncGaussian45:
        return static_cast<int>(spectrogram::SpectrogramWindowType::Gaussian45);
    default:
        assert(false);
        return 0;
    }
}

constexpr auto toAu3Algorithm(spectrogram::SpectrogramAlgorithm algorithm)
{
    switch (algorithm) {
    case spectrogram::SpectrogramAlgorithm::Frequencies:
        return spectrogram::SpectrogramSettings::AlgorithmValues::algSTFT;
    case spectrogram::SpectrogramAlgorithm::Reassignment:
        return spectrogram::SpectrogramSettings::AlgorithmValues::algReassignment;
    case spectrogram::SpectrogramAlgorithm::Pitch:
        return spectrogram::SpectrogramSettings::AlgorithmValues::algPitchEAC;
    default:
        assert(false);
        return static_cast<spectrogram::SpectrogramSettings::AlgorithmValues>(0);
    }
}

constexpr int fromAu3Algorithm(spectrogram::SpectrogramSettings::AlgorithmValues algorithm)
{
    switch (algorithm) {
    case spectrogram::SpectrogramSettings::AlgorithmValues::algSTFT:
        return static_cast<int>(spectrogram::SpectrogramAlgorithm::Frequencies);
    case spectrogram::SpectrogramSettings::AlgorithmValues::algReassignment:
        return static_cast<int>(spectrogram::SpectrogramAlgorithm::Reassignment);
    case spectrogram::SpectrogramSettings::AlgorithmValues::algPitchEAC:
        return static_cast<int>(spectrogram::SpectrogramAlgorithm::Pitch);
    default:
        assert(false);
        return 0;
    }
}
} // namespace au::trackedit
