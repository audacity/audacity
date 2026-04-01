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

#include <memory>

namespace au::importexport {
class TempoDetection;

class Au3Importer : public IImporter, public muse::Contextable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::ITracksInteraction> tracksInteraction{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };

public:
    Au3Importer(const muse::modularity::ContextPtr& ctx);
    ~Au3Importer() override;

    void init() override;

    FileInfo fileInfo(const muse::io::path_t& filePath) override;

    bool import(const muse::io::path_t& filePath) override;
    bool importIntoTrack(const muse::io::path_t& filePath, trackedit::TrackId dstTrackId, muse::secs_t startTime) override;
    bool importFromSystemClipboard(const std::vector<muse::io::path_t>& filePaths, muse::secs_t startTime) override;
    std::vector<std::string> supportedExtensions() const override;

private:
    bool isProjectEmpty() const;
    void applyImportedProjectTitleIfNeeded(const muse::io::path_t& filePath);
    void addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks, std::vector<WaveTrack*>* outWaveTracks = nullptr);

    std::unique_ptr<TempoDetection> m_tempoDetection;
};
}
