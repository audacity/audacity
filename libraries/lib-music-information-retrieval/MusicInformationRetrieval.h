/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MusicInformationRetrieval.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirTypes.h"

#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace MIR
{
class MirAudioReader;

struct LoopClassifierSettings
{
   /*!
    * False positive rate allowed for the classifier.
    */
   const double allowedFalsePositiveRate;

   /*!
    * Classifier score threshold above which the analyzed audio file can be
    */
   const double threshold;
};

static const std::unordered_map<FalsePositiveTolerance, LoopClassifierSettings>
   loopClassifierSettings {
      { FalsePositiveTolerance::Strict, { .01, 0.8367879995455243 } },
      { FalsePositiveTolerance::Lenient, { .1, 0.7336582045210642 } },
   };

/*!
 * Information needed to time-synchronize the audio file with the project.
 */
struct ProjectSyncInfo
{
   /*!
    * The tempo of the raw audio file, in quarter-notes per minute.
    */
   const double rawAudioTempo;

   /*!
    * The time-signature of the raw audio file.
    */
   const std::optional<TimeSignature> timeSignature;

   /*!
    * Should be 1 most of the time, but may be 0.5 or 2 to reduce the amount
    * of stretching needed to match the project tempo.
    */
   const double stretchMinimizingPowOfTwo;

   /*!
    * It is common that loops fill up a bit more than the intended number of
    * bars. If this is detected, this value is written here and may be used for
    * trimming.
    */
   const double excessDurationInQuarternotes;
};

MUSIC_INFORMATION_RETRIEVAL_API int GetNumerator(TimeSignature ts);
MUSIC_INFORMATION_RETRIEVAL_API int GetDenominator(TimeSignature ts);

class MUSIC_INFORMATION_RETRIEVAL_API MusicInformation
{
public:
   /**
    * @brief Construct a new Music Information object
    * @detail For now we only exploit the filename and duration ...
    */
   MusicInformation(
      const std::string& filename, double duration,
      const MirAudioReader& source, FalsePositiveTolerance tolerance,
      std::function<void(double progress)> progressCallback);

   const std::string filename;
   const double duration;

   /*!
    * @brief Tells whether the file contains music content.
    */
   operator bool() const;

   /**
    * @brief Get the information needed to synchronize the corresponding file
    * with the project it belongs to.
    * @pre Music content was detected, i.e., `*this == true`
    */
   ProjectSyncInfo
   GetProjectSyncInfo(const std::optional<double>& projectTempo) const;

private:
   const std::optional<MusicalMeter> mMusicalMeter;

   // Additional information (key(s), genre, etc) to be added here.
};

// Used internally by `MusicInformation`, made public for testing.
MUSIC_INFORMATION_RETRIEVAL_API std::optional<double>
GetBpmFromFilename(const std::string& filename);

MUSIC_INFORMATION_RETRIEVAL_API std::optional<MusicalMeter>
GetMusicalMeterFromSignal(
   const MirAudioReader& source, FalsePositiveTolerance tolerance,
   const std::function<void(double)>& progressCallback,
   QuantizationFitDebugOutput* debugOutput = nullptr);
} // namespace MIR
