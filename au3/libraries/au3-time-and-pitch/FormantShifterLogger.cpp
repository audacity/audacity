/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FormantShifterLogger.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "FormantShifterLogger.h"
#include "TimeAndPitchExperimentalSettings.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace {
std::string GetLogDir()
{
    const char* appDataDir = getenv("APPDATA");
    if (appDataDir) {
        return std::string(appDataDir) + "/TimeAndPitchTuning";
    } else {
        return "";
    }
}

template<typename Iterator>
void PrintPythonVector(
    std::ofstream& ofs, Iterator begin, Iterator end, const char* name)
{
    ofs << name << " = [";
    std::for_each(begin, end, [&](float x) { ofs << x << ","; });
    ofs << "]\n";
}
} // namespace

FormantShifterLogger::FormantShifterLogger(int sampleRate, int logSample)
    : mSampleRate{sampleRate}
    , mLogSample{logSample}
{
}

FormantShifterLogger::~FormantShifterLogger()
{
}

void FormantShifterLogger::NewSamplesComing(int sampleCount)
{
    mSampleCount += sampleCount;
    if (!mWasLogged && mLogSample <= mSampleCount) {
        // Ready for logging.
        mOfs = std::make_unique<std::ofstream>(
            TimeAndPitchExperimentalSettings::GetLogDir()
            + "/FormantShifterLog.py");
        *mOfs << "sampleRate = " << mSampleRate << "\n";
        mWasLogged = true;
    }
}

void FormantShifterLogger::Log(int value, const char* name) const
{
    if (mOfs) {
        *mOfs << name << " = " << value << "\n";
    }
}

void FormantShifterLogger::Log(
    const float* samples, size_t size, const char* name) const
{
    if (!mOfs) {
        // Keep it lightweight if we're not logging.
        return;
    }
    PrintPythonVector(*mOfs, samples, samples + size, name);
}

void FormantShifterLogger::Log(
    const std::complex<float>* cv, size_t cvSize, const char* name,
    const std::function<float(const std::complex<float>&)>& transform) const
{
    if (!mOfs) {
        return;
    }
    std::vector<float> v(cvSize);
    std::transform(cv, cv + cvSize, v.begin(), transform);
    PrintPythonVector(*mOfs, v.begin(), v.end(), name);
}

void FormantShifterLogger::ProcessFinished(
    std::complex<float>* spectrum, size_t fftSize)
{
    if (!mOfs) {
        return;
    }
    // Such a spectrum of only (1 + 0j) is that of a click, which should be
    // audible ...
    std::fill(spectrum, spectrum + fftSize / 2 + 1, 1.f);
    mOfs.reset();
}
