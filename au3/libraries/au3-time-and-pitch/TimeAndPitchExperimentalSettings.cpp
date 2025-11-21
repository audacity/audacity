/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchExperimentalSettings.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "TimeAndPitchExperimentalSettings.h"
#include "FileNames.h"
#include "Prefs.h"
#include <cstdlib>
#include <fstream>

namespace {
template<typename T> std::optional<T> GetFromFile(const char* filenameStem)
{
    bool tuningOn = false;
    gPrefs->Read(wxT("/TimeAndPitch/TuningOn"), &tuningOn, false);
    if (!tuningOn) {
        return {};
    }
    T value;
    std::ifstream file { TimeAndPitchExperimentalSettings::GetLogDir() + "/"
                         + filenameStem + ".txt" };
    if (!file.is_open()) {
        return {};
    }
    // Check if file is empty or first character is newline.
    if (file.peek() == std::ifstream::traits_type::eof() || file.peek() == '\n') {
        return {};
    }
    file >> value;
    return value;
}
} // namespace

std::string TimeAndPitchExperimentalSettings::GetLogDir()
{
    return FileNames::ConfigDir().ToStdString() + "/TimeAndPitchTuning/";
}

std::optional<int>
TimeAndPitchExperimentalSettings::GetLogSample(int sampleRate)
{
    if (const auto logTime = GetFromFile<double>("overrideLogTime")) {
        return static_cast<int>(*logTime * sampleRate);
    }
    return {};
}

std::optional<double>
TimeAndPitchExperimentalSettings::GetCutoffQuefrencyOverride()
{
    return GetFromFile<double>("overrideCutoffQuefrency");
}

std::optional<int> TimeAndPitchExperimentalSettings::GetFftSizeOverride()
{
    if (const auto fftSizeExponent = GetFromFile<int>("overrideFftSizeExponent")) {
        return 1 << *fftSizeExponent;
    }
    return {};
}

std::optional<bool> TimeAndPitchExperimentalSettings::GetReduceImagingOverride()
{
    if (const auto reduceImaging = GetFromFile<int>("overrideReduceImaging")) {
        return static_cast<bool>(*reduceImaging);
    }
    return {};
}
