/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LibsndfileTagger.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AcidizerTags.h"

// SNDFILE is defined differently between libsndfile versions
// making forward declarations impossible.
#include <sndfile.h>

#include <memory>
#include <string>

namespace LibImportExport {
namespace Test {
/*!
 * @brief In the Audacity code, we only are interested in
 * LibFileFormats::AcidizerTags and the information it carries, but for testing
 * we need to mimic more than one way ACID metadata may be set out there. We
 * therefore need to be able to set tempo, beats and one-shot status
 * independently.
 */
struct AcidizerTags : LibFileFormats::AcidizerTags
{
    struct Beats
    {
        explicit Beats(int beats)
            : beats{beats}
        {
        }

        const int beats;
    };

    using LibFileFormats::AcidizerTags::AcidizerTags;

    AcidizerTags(Beats beats)
        : beats{beats.beats}
    {
    }

    const std::optional<int> beats;
};

/*!
 * @brief When adding tags, the allocated memory must be preserved until the
 * file is closed. This class handles that, beside the regular file opening and
 * closing.
 */
class IMPORT_EXPORT_API LibsndfileTagger final
{
public:
    LibsndfileTagger(double duration = 0., const std::string& filename = "");
    ~LibsndfileTagger();

    operator bool() const;
    void AddAcidizerTags(const Test::AcidizerTags& acidTags);
    void AddDistributorInfo(const std::string& distributor);
    SNDFILE& ReopenInReadMode();

private:
    const std::string mFilename;
    SNDFILE* mFile;
    std::unique_ptr<uint8_t[]> mAcidData;
    std::unique_ptr<uint8_t[]> mDistributorData;
};
} // namespace Test
} // namespace LibImportExport
