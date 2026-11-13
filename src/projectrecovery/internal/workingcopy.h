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
#include "framework/global/types/datetime.h"
#include "framework/global/types/ret.h"

namespace au::projectrecovery {
// This is the file we are working with, creates a copy of the original project and sidecars
class WorkingCopy
{
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    WorkingCopy() = default;
    ~WorkingCopy();

    WorkingCopy(const WorkingCopy&) = delete;
    WorkingCopy& operator=(const WorkingCopy&) = delete;

    // empty means temporary dir
    muse::Ret prepare(const muse::io::path_t& original, const muse::io::path_t& workDir = {});

    const muse::io::path_t& path() const { return m_path; }
    bool isOriginal() const { return !m_isCopy; }
    uint64_t walSize() const { return m_walSize; }
    bool walApplied() const { return m_walApplied; }
    const std::string& walError() const { return m_walError; }
    //! True when the original or its -wal file changed after prepare()
    bool originalChanged() const;

private:
    struct FileStamp {
        bool exists = false;
        uint64_t size = 0;
        muse::DateTime modified;

        bool operator==(const FileStamp& other) const
        {
            return exists == other.exists && size == other.size && modified == other.modified;
        }
    };

    std::vector<FileStamp> stamp(const muse::io::path_t& original) const;
    void removeCopy();

    muse::io::path_t m_original;
    std::vector<FileStamp> m_stamp;
    muse::io::path_t m_path;
    muse::io::path_t m_workDir;
    bool m_isCopy = false;
    uint64_t m_walSize = 0;
    bool m_walApplied = false;
    std::string m_walError;
};
}
