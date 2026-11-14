/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FormantShifterLoggerInterface.h

  A visualization helper for the development of the formant shifter.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <complex>
#include <functional>

class FormantShifterLoggerInterface
{
public:
    virtual ~FormantShifterLoggerInterface() = default;

    virtual void NewSamplesComing(int sampleCount) = 0;

    virtual void Log(int value, const char* name) const = 0;

    virtual void
    Log(const float* samples, size_t size, const char* name) const = 0;

    virtual void Log(
        const std::complex<float>* samples, size_t size, const char* name,
        const std::function<float(const std::complex<float>&)>& transform)
    const = 0;

    /*!
     * @brief If not already, disables the logging and marks the spectrum with an
     * audible event to make clear where in the signal the logging took place.
     * (Of course not for use in production :D)
     */
    virtual void
    ProcessFinished(std::complex<float>* spectrum, size_t fftSize) = 0;
};
