/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LibsndfileTagger.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <memory>
#include <string>

typedef struct SNDFILE_tag SNDFILE;

namespace LibFileFormats
{
struct AcidizerTags;
}

namespace LibImportExport
{
namespace Test
{
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
   void AddAcidizerTags(const LibFileFormats::AcidizerTags& acidTags);
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
