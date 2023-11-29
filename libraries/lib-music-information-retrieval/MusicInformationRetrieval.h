/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MusicInformationRetrieval.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <optional>
#include <string>
#include <vector>

namespace MIR
{
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

class MUSIC_INFORMATION_RETRIEVAL_API MusicInformation
{
public:
   /**
    * @brief Construct a new Music Information object
    * @detail For now we only exploit the filename and duration ...
    */
   MusicInformation(const std::string& filename, double duration);

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
   /*!
    * For now we either detect constant tempo or no tempo. We may need to
    * extend it to a `map<double , double >` when we have a master track with
    * tempo automation.
    * Note that BPM isn't quarter-notes per minute (QPM), which is the value we
    * need to adjust project tempo. For this, a `timeSignature` is also needed.
    * Else, the most likely QPM can be guessed, but there's always a risk of
    * over- or underestimating by a factor of two.
    */
   std::optional<double> mBpm;

   // Additional information (time signature(s), key(s), genre, etc) to be added
   // here.
};

// Used internally by `MusicInformation`, made public for testing.
MUSIC_INFORMATION_RETRIEVAL_API std::optional<double>
GetBpmFromFilename(const std::string& filename);

} // namespace MIR
