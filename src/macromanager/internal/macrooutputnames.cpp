/*
* Audacity: A Digital Audio Editor
*/
#include "macrooutputnames.h"

#include <set>

using namespace muse;

namespace {
std::string outputKey(const std::string& name)
{
    return io::escapeFileName(name).toQString().toLower().toStdString();
}
}

std::vector<std::string> au::macromanager::uniqueOutputNames(const io::paths_t& files)
{
    std::set<std::string> usedKeys;
    std::vector<std::string> names;
    names.reserve(files.size());

    for (const io::path_t& file : files) {
        const std::string baseName = io::completeBasename(file).toStdString();

        std::string name = baseName;
        for (int suffix = 2; !usedKeys.insert(outputKey(name)).second; ++suffix) {
            name = baseName + "_" + std::to_string(suffix);
        }

        names.push_back(name);
    }

    return names;
}
