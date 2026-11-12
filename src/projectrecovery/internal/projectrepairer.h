/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

#include "framework/global/io/ifilesystem.h"
#include "framework/global/io/path.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/retval.h"

#include "projectinspector.h"

namespace au::projectrecovery {
struct RepairReport {
    //! The blocks that are silence of the same length in the output
    std::vector<MissingBlock> silencedBlocks;
    //! The inspection of the output, which has no problems
    Inspection verification;
};

class ProjectRepairer
{
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    //! Writes the repaired project to `output`, which must not exist. Never writes to the original or to its -wal and -shm
    //! files. When the repair fails, no output stays.
    //! workDir: the folder for the copy of an original with -wal frames; empty means a folder in the temporary directory
    muse::RetVal<RepairReport> repair(const muse::io::path_t& original, const muse::io::path_t& output,
                                      const muse::io::path_t& workDir = {}) const;

private:
    muse::Ret copyToOutput(const muse::io::path_t& original, const muse::io::path_t& output, const muse::io::path_t& workDir) const;
    muse::Ret silenceMissingBlocks(const muse::io::path_t& output, const Inspection& inspection) const;
    void removeOutput(const muse::io::path_t& output) const;
};
}
