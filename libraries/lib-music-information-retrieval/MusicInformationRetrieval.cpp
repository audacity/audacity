/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MusicInformationRetrieval.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MusicInformationRetrieval.h"
#include "GetMeterUsingTatumQuantizationFit.h"
#include "MirAudioReader.h"
#include "MirUtils.h"
#include "StftFrameProvider.h"

#include <array>
#include <cassert>
#include <cmath>
#include <numeric>
#include <regex>

namespace MIR
{
namespace
{
constexpr auto numTimeSignatures = static_cast<int>(TimeSignature::_count);

auto RemovePathPrefix(const std::string& filename)
{
   return filename.substr(filename.find_last_of("/\\") + 1);
}

// When we get time-signature estimate, we may need a map for that, since 6/8
// has 1.5 quarter notes per beat.
constexpr std::array<double, numTimeSignatures> quarternotesPerBeat { 2., 1.,
                                                                      1., 1.5 };

void FillMetadata(
   const std::string& filename, const MirAudioReader& audio,
   FalsePositiveTolerance tolerance,
   const std::function<void(double progress)>& progressCallback,
   std::optional<double>& bpm, std::optional<TimeSignature>& timeSignature)
{
   const auto bpmFromFilename = GetBpmFromFilename(filename);
   if (bpmFromFilename.has_value())
      bpm = bpmFromFilename;
   else if (const auto meter =
               GetMusicalMeterFromSignal(audio, tolerance, progressCallback);
            meter.has_value())
   {
      bpm = meter->bpm;
      timeSignature = meter->timeSignature;
   }
}
} // namespace

int GetNumerator(TimeSignature ts)
{
   constexpr std::array<int, numTimeSignatures> numerators = { 2, 4, 3, 6 };
   return numerators[static_cast<int>(ts)];
}

int GetDenominator(TimeSignature ts)
{
   constexpr std::array<int, numTimeSignatures> denominators = { 2, 4, 4, 8 };
   return denominators[static_cast<int>(ts)];
}

MusicInformation::MusicInformation(
   const std::string& filename, double duration, const MirAudioReader& audio,
   FalsePositiveTolerance tolerance,
   std::function<void(double progress)> progressCallback)
    : filename { RemovePathPrefix(filename) }
    , duration { duration }
{
   FillMetadata(
      filename, audio, tolerance, std::move(progressCallback),
      const_cast<std::optional<double>&>(mBpm),
      const_cast<std::optional<TimeSignature>&>(mTimeSignature));
}

MusicInformation::operator bool() const
{
   // For now suffices to say that we have detected music content if there is
   // rhythm.
   return mBpm.has_value();
}

ProjectSyncInfo MusicInformation::GetProjectSyncInfo(
   const std::optional<double>& projectTempo) const
{
   assert(*this);
   if (!*this)
      return {};

   const auto error = *mBpm - bpmExpectedValue;

   const auto qpm =
      *mBpm * quarternotesPerBeat[static_cast<int>(
                 mTimeSignature.value_or(TimeSignature::FourFour))];

   auto recommendedStretch = 1.0;
   if (projectTempo.has_value())
      recommendedStretch =
         std::pow(2., std::round(std::log2(*projectTempo / qpm)));

   auto excessDurationInQuarternotes = 0.;
   auto numQuarters = duration * qpm / 60.;
   const auto roundedNumQuarters = std::round(numQuarters);
   const auto delta = numQuarters - roundedNumQuarters;
   // If there is an excess less than a 32nd, we treat it as an edit error.
   if (0 < delta && delta / 8)
      excessDurationInQuarternotes = delta;

   return { qpm, mTimeSignature, recommendedStretch,
            excessDurationInQuarternotes };
}

std::optional<double> GetBpmFromFilename(const std::string& filename)
{
   // regex matching a forward or backward slash:

   // Regex: <(anything + (directory) separator) or nothing> <2 or 3 digits>
   // <optional separator> <bpm (case-insensitive)> <separator or nothing>
   const std::regex bpmRegex {
      R"((?:.*(?:_|-|\s|\.|/|\\))?(\d+)(?:_|-|\s|\.)?bpm(?:(?:_|-|\s|\.).*)?)",
      std::regex::icase
   };
   std::smatch matches;
   if (std::regex_match(filename, matches, bpmRegex))
      try
      {
         const auto value = std::stoi(matches[1]);
         return 30 <= value && value <= 300 ? std::optional<double> { value } :
                                              std::nullopt;
      }
      catch (const std::invalid_argument& e)
      {
         assert(false);
      }
   return {};
}

std::optional<MusicalMeter> GetMusicalMeterFromSignal(
   const MirAudioReader& audio, FalsePositiveTolerance tolerance,
   const std::function<void(double)>& progressCallback,
   QuantizationFitDebugOutput* debugOutput)
{
   if (audio.GetNumSamples() / audio.GetSampleRate() > 60)
      // A file longer than 1 minute is most likely not a loop, and processing
      // it would be costly.
      return {};
   return GetMeterUsingTatumQuantizationFit(
      audio, tolerance, progressCallback, debugOutput);
}
} // namespace MIR
