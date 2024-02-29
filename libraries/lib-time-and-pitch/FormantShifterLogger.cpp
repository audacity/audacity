/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FormantShifterLogger.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "FormantShifterLogger.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace
{
const std::string logDir { CMAKE_CURRENT_SOURCE_DIR };

template <typename T>
void PrintPythonVector(std::ofstream& ofs, const T& v, const char* name)
{
   ofs << name << " = [";
   std::for_each(v.begin(), v.end(), [&](float x) { ofs << x << ","; });
   ofs << "]\n";
}

template <typename T> std::optional<T> GetFromFile(const char* filenameStem)
{
   std::ifstream file { logDir + "/" + filenameStem + ".txt" };
   if (!file.is_open())
      return {};
   // Check if file is empty or first character is newline.
   if (file.peek() == std::ifstream::traits_type::eof() || file.peek() == '\n')
      return {};
   T value;
   file >> value;
   return value;
}

std::optional<int> GetLogSample(int sampleRate)
{
   if (const auto logTime = GetFromFile<double>("overrideLogTime"))
      return static_cast<int>(*logTime * sampleRate);
   return {};
}
} // namespace

std::optional<double> FormantShifterLogger::GetCutoffQuefrencyOverride()
{
   return GetFromFile<double>("overrideCutoffQuefrency");
}

std::optional<int> FormantShifterLogger::GetFftSizeOverride()
{
   if (const auto fftSizeExponent = GetFromFile<int>("overrideFftSizeExponent"))
      return 1 << *fftSizeExponent;
   return {};
}

std::optional<bool> FormantShifterLogger::GetReduceImagingOverride()
{
   if (const auto reduceImaging = GetFromFile<int>("overrideReduceImaging"))
      return static_cast<bool>(*reduceImaging);
   return {};
}

FormantShifterLogger::FormantShifterLogger(int sampleRate)
    : mSampleRate { sampleRate }
    , mLogSample { GetLogSample(sampleRate) }
{
}

void FormantShifterLogger::NewSamplesComing(int sampleCount)
{
   mSampleCount += sampleCount;
   if (mLogSample.has_value() && *mLogSample <= mSampleCount)
   {
      // Ready for logging.
      mOfs = std::make_unique<std::ofstream>(logDir + "/FormantShifterLog.py");
      *mOfs << "sampleRate = " << mSampleRate << "\n";
      mLogSample.reset();
   }
}

void FormantShifterLogger::Log(int value, const char* name) const
{
   if (mOfs)
      *mOfs << name << " = " << value << "\n";
}

void FormantShifterLogger::Log(
   const float* samples, size_t size, const char* name) const
{
   if (!mOfs)
      // Keep it lightweight if we're not logging.
      return;
   PrintPythonVector(
      *mOfs, std::vector<float> { samples, samples + size }, name);
}

void FormantShifterLogger::Log(
   const std::complex<float>* cv, size_t cvSize, const char* name,
   const std::function<float(const std::complex<float>&)>& transform) const
{
   if (!mOfs)
      return;
   std::vector<float> v(cvSize);
   std::transform(cv, cv + cvSize, v.begin(), transform);
   PrintPythonVector(*mOfs, v, name);
}

void FormantShifterLogger::ProcessFinished(
   std::complex<float>* spectrum, size_t fftSize)
{
   if (!mOfs)
      return;
   // Such a spectrum of only (1 + 0j) is that of a click, which should be
   // audible ...
   std::fill(spectrum, spectrum + fftSize / 2 + 1, 1.f);
   mOfs.reset();
}
