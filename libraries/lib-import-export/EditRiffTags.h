#pragma once

#include <string>

typedef struct SNDFILE_tag SNDFILE;

namespace LibFileFormats
{
struct AcidizerTags;
}

namespace LibImportExport
{
// Unfortunately, libsndfile has support for neither writing loop info (although
// it does have support for reading it) nor writing LIST chunks.
void IMPORT_EXPORT_API
AddAcidizerTags(const LibFileFormats::AcidizerTags& acidTags, SNDFILE* file);
void IMPORT_EXPORT_API
AddDistributorInfo(const std::string& distributor, SNDFILE* file);
} // namespace LibImportExport
