/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DummyFormantShifterLogger.h

  A class for shifting the formants of a voice signal.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "FormantShifterLoggerInterface.h"

class DummyFormantShifterLogger : public FormantShifterLoggerInterface
{
public:
    ~DummyFormantShifterLogger() override;
    void NewSamplesComing(int sampleCount) override;
    void Log(int value, const char* name) const override;
    void Log(const float* samples, size_t size, const char* name) const override;
    void Log(
        const std::complex<float>* samples, size_t size, const char* name,
        const std::function<float(const std::complex<float>&)>& transform)
    const override;
    void ProcessFinished(std::complex<float>* spectrum, size_t fftSize) override;
};
