/*
* Audacity: A Digital Audio Editor
*/
#include "runonfilesstarter.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri RUN_ON_FILES_URI("audacity://macromanager/runonfiles");
}

void RunOnFilesStarter::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        startIfRequested();
    });

    startIfRequested();
}

void RunOnFilesStarter::startIfRequested()
{
    if (!globalContext()->currentProject()) {
        return;
    }

    const std::optional<RunOnFilesRequest> request = requests()->take(iocContext());
    if (!request) {
        return;
    }

    ValList fileVals;
    for (const io::path_t& file : request->files) {
        fileVals.push_back(Val(file.toStdString()));
    }

    UriQuery uri(RUN_ON_FILES_URI);
    uri.addParam("macroName", Val(request->macro.name));
    uri.addParam("files", Val(fileVals));
    interactive()->open(uri);

    runner()->runOnFiles(request->macro, request->files);
}
