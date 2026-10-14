/*
* Audacity: A Digital Audio Editor
*/
#include "runonfileslauncher.h"

#include <algorithm>

#include "global/translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri RUN_ON_FILES_URI("audacity://macromanager/runonfiles");
}

void RunOnFilesLauncher::launch(const Macro& macro)
{
    if (runner()->isRunning()) {
        return;
    }

    if (!globalContext()->currentTrackeditProject()->trackIdList().empty()) {
        interactive()->warning(trc("macromanager", "Run on files"),
                               trc("macromanager", "Save and close the current project first."));
        return;
    }

    io::paths_t files = selectFiles();
    if (files.empty()) {
        return;
    }

    std::ranges::sort(files, {}, &io::path_t::toStdString);

    ValList fileVals;
    for (const io::path_t& file : files) {
        fileVals.push_back(Val(file.toStdString()));
    }

    UriQuery uri(RUN_ON_FILES_URI);
    uri.addParam("macroName", Val(macro.name));
    uri.addParam("files", Val(fileVals));
    interactive()->open(uri);

    runner()->runOnFiles(macro, files);
}

io::paths_t RunOnFilesLauncher::selectFiles() const
{
    std::string extensions;
    for (const std::string& ext : importer()->supportedExtensions()) {
        if (ext.empty()) {
            continue;
        }

        if (!extensions.empty()) {
            extensions += " ";
        }

        extensions += "*." + ext;
    }

    const std::vector<std::string> filter {
        trc("macromanager", "Audio files") + " (" + extensions + ")"
    };

    io::path_t defaultDir = projectConfiguration()->lastOpenedProjectsPath();
    if (defaultDir.empty()) {
        defaultDir = projectConfiguration()->userProjectsPath();
    }

    return interactive()->selectOpeningFilesSync(trc("macromanager", "Select files"), defaultDir, filter);
}
