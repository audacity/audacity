/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "framework/global/io/ifilesystem.h"
#include "framework/global/io/path.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/retval.h"

namespace au::projectrecovery {
//! 'AUDY', the application_id of every Audacity project file
constexpr int64_t AUDACITY_APPLICATION_ID = 1096107097;

enum class Problem {
    //! SQLite cannot open the file, or the file has no sampleblocks table
    DatabaseNotReadable,
    //! Unknown application_id
    NotAudacityProject,
    //! The project was created in a newer version, for safety refuse to open it
    NewerProjectVersion,
    // Sidecar was not applied for some reason
    WalNotApplied,
    //! The file has no project and no autosave document
    NoDocument,
    //! The document that Audacity could not decode
    DocumentNotReadable,
    //! The document that contains references to blocks that are missing from the database
    MissingBlocks,
    //! A missing block start time before the next block, or after the sequence end
    MissingBlockWithoutLength,
    //! PRAGMA integrity_check result
    IntegrityErrors,
};

struct DatabaseInfo {
    muse::io::path_t path;
    uint64_t fileSize = 0;
    uint64_t walSize = 0;
    bool walApplied = false;
    std::string walError;
    bool opens = false;
    int64_t pageSize = 0;
    int64_t pageCount = 0;
    int64_t freePageCount = 0;
    int64_t applicationId = 0;
    int64_t userVersion = 0;
    int64_t blockRows = 0;
    int64_t orphanBlocks = 0;
    bool integrityChecked = false;
    std::vector<std::string> integrityMessages;
};

struct DocumentInfo {
    //! "project", "autosave" or "history <generation>"
    std::string source;
    bool readable = false;
    std::string error;
    std::string audacityVersion;
    double rate = 0;
    size_t waveTracks = 0;
    size_t clips = 0;
    size_t blockReferences = 0;
    size_t distinctBlocks = 0;
    size_t missingBlocks = 0;
};

struct MissingBlock {
    int64_t id = 0;
    int64_t samples = 0;
    std::string track;
    std::string clip;
    //! The time of the block in its track
    //! not to be cofused with a clip time, as this time does not include trims!
    double startSeconds = 0;
    double endSeconds = 0;
};

struct Inspection {
    DatabaseInfo database;
    std::vector<DocumentInfo> documents;
    //! The document that used by Audacity: "autosave" if present, "project" otherwise
    std::string activeDocument;
    std::vector<MissingBlock> missingBlocks;
    std::vector<Problem> problems;

    bool isHealthy() const { return problems.empty(); }
    bool has(Problem problem) const;
};

struct InspectionOptions {
    muse::io::path_t workDir;
    bool checkIntegrity = true;
    size_t maxIntegrityMessages = 100;
};

class ProjectInspector
{
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    muse::RetVal<Inspection> inspect(const muse::io::path_t& path, const InspectionOptions& options = {}) const;
};
}
