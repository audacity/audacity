/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/itracksinteraction.h"
#include "trackedit/iselectioncontroller.h"

#include "au3-import-export/Import.h"

#include "../../iimporter.h"

namespace au::importexport {
class Au3Importer : public IImporter
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ITracksInteraction> tracksInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    Au3Importer() = default;

    void init() override;

    FileInfo fileInfo(const muse::io::path_t& filePath) override;

    bool import(const muse::io::path_t& filePath) override;
    bool importIntoTrack(const muse::io::path_t& filePath, trackedit::TrackId dstTrackId, muse::secs_t startTime) override;

private:
    void addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks);
};
}
