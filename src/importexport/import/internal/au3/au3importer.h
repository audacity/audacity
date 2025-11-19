/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "context/iglobalcontext.h"

#include "au3-import-export/Import.h"

#include "../../iimporter.h"

namespace au::importexport {
class Au3Importer : public IImporter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3Importer() = default;

    void init() override;
    FileInfo fileInfo(const muse::io::path_t& filePath) override;
    bool import(const muse::io::path_t& filePath) override;

private:
    void addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks);
};
}
