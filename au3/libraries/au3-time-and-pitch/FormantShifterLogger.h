/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FormantShifterLogger.h

  Implements FormantShifterLoggerInterface, and also provides tuning utilities
  to override algorithm parameters.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "FormantShifterLoggerInterface.h"
#include <fstream>
#include <memory>

class FormantShifterLogger : public FormantShifterLoggerInterface
{
public:
    FormantShifterLogger(int sampleRate, int logTimeInSamples);
    ~FormantShifterLogger() override;

    void NewSamplesComing(int sampleCount) override;

    // Methods intended to be called from FormantShifter
public:
    void Log(int value, const char* name) const override;
    void Log(const float* samples, size_t size, const char* name) const override;
    void Log(
        const std::complex<float>* samples, size_t size, const char* name,
        const std::function<float(const std::complex<float>&)>& transform)
    const override;
    /*!
     * @brief If not already, disables the logging and marks the spectrum with an
     * audible event to make clear where in the signal the logging took place.
     * (Of course not for use in production :D)
     */
    void ProcessFinished(std::complex<float>* spectrum, size_t fftSize) override;

private:
    const int mSampleRate;
    const int mLogSample;
    bool mWasLogged = false;
    std::unique_ptr<std::ofstream> mOfs;
    int mSampleCount = 0;
};
