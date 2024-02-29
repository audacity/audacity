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
#include <optional>

class FormantShifterLogger : public FormantShifterLoggerInterface
{
   // FormantShifterLoggerInterface, intended to be called from FormantShifter
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

public:
   static std::optional<double> GetCutoffQuefrencyOverride();
   static std::optional<int> GetFftSizeOverride();
   static std::optional<bool> GetReduceImagingOverride();

   FormantShifterLogger(int sampleRate);
   void NewSamplesComing(int sampleCount);

private:
   const int mSampleRate;
   std::optional<int> mLogSample;
   std::unique_ptr<std::ofstream> mOfs;
   int mSampleCount = 0;
};
