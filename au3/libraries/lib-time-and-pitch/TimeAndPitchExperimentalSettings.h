/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchExperimentalSettings.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <optional>
#include <string>

namespace TimeAndPitchExperimentalSettings {
std::string GetLogDir();
std::optional<int> GetLogSample(int sampleRate);
std::optional<double> GetCutoffQuefrencyOverride();
std::optional<int> GetFftSizeOverride();
std::optional<bool> GetReduceImagingOverride();
} // namespace TimeAndPitchExperimentalSettings
