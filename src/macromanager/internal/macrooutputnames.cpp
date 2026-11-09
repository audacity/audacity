/*
* Audacity: A Digital Audio Editor
*/
#include "macrooutputnames.h"

#include <algorithm>
#include <iterator>

#include "importexport/export/exportutils.h"

using namespace muse;

std::vector<std::string> au::macromanager::uniqueOutputNames(const io::paths_t& files)
{
    importexport::utils::UniqueFileNames usedNames;
    std::vector<std::string> names;
    names.reserve(files.size());

    std::ranges::transform(files, std::back_inserter(names), [&usedNames](const io::path_t& file) {
        return usedNames.registerName(io::escapeFileName(io::completeBasename(file)).toStdString());
    });

    return names;
}
