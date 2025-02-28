/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DummyFormantShifterLogger.cpp

  A class for shifting the formants of a voice signal.

  Matthieu Hodgkinson

**********************************************************************/
#include "DummyFormantShifterLogger.h"

DummyFormantShifterLogger::~DummyFormantShifterLogger()
{
}

void DummyFormantShifterLogger::NewSamplesComing(int sampleCount)
{
}

void DummyFormantShifterLogger::Log(int value, const char* name) const
{
}

void DummyFormantShifterLogger::Log(
    const float* samples, size_t size, const char* name) const
{
}

void DummyFormantShifterLogger::Log(
    const std::complex<float>* samples, size_t size, const char* name,
    const std::function<float(const std::complex<float>&)>& transform) const
{
}

void DummyFormantShifterLogger::ProcessFinished(
    std::complex<float>* spectrum, size_t fftSize)
{
}
