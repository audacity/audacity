/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/spectrogramtypes.h"
#include "spectrogram/internal/au3/au3spectrogramsettings.h" // for now, track settings provider coming

#include "au3-fft/FFT.h" // eWindowFunctions

namespace au::spectrogram {
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
} // namespace au::trackedit
