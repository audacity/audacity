/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetAcidizerTags.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <optional>
#include <string>
#include <vector>

// SNDFILE is defined differently between libsndfile versions
// making forward declarations impossible.
#include <sndfile.h>

namespace LibFileFormats {
struct AcidizerTags;
}

namespace LibImportExport {
/*!
 * @brief Get the Acidizer tags from a file if from a trusted
 * distributor.
 *
 * @details Acidizer tags are looked for in the header (
 * https://exiftool.org/TagNames/RIFF.html#Acidizer). Metadata with incorrect
 * BPM values are nevertheless very common, so the function will return non-null
 * only if the distributor is trusted. The distributor is looked for as the
 * "DistributedBy" RIFF info tag (https://exiftool.org/TagNames/RIFF.html#Info).
 */
std::optional<LibFileFormats::AcidizerTags> IMPORT_EXPORT_API GetAcidizerTags(
    SNDFILE& file, const std::vector<std::string>& trustedDistributors);
} // namespace LibImportExport
