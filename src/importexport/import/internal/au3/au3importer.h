/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "context/iglobalcontext.h"

#include "libraries/lib-import-export/Import.h"

#include "../../iimporter.h"

namespace au::importexport {
class Au3Importer : public IImporter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3Importer() = default;

    void init() override;
    bool import(const muse::io::path_t& filePath) override;

private:
    void addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks);
};
}
