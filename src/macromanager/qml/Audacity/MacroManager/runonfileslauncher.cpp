/*
* Audacity: A Digital Audio Editor
*/
#include "runonfileslauncher.h"

#include <algorithm>

#include "framework/global/translation.h"

using namespace au::macromanager;
using namespace muse;

void RunOnFilesLauncher::launch(const Macro& macro)
{
    io::paths_t files = selectFiles();
    if (files.empty()) {
        return;
    }

    std::ranges::sort(files, {}, &io::path_t::toStdString);

    const modularity::ContextPtr ctx = application()->setupNewContext({ u"--session-type", u"start-with-new" });
    requests()->push(ctx, { macro, files });
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
