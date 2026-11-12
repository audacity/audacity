/*
 * Audacity: A Digital Audio Editor
 */
#include "workingcopy.h"

#include <atomic>
#include <functional>

#include "framework/global/log.h"

#include "database.h"

using namespace au::projectrecovery;

WorkingCopy::~WorkingCopy()
{
    removeCopy();
}

muse::Ret WorkingCopy::prepare(const muse::io::path_t& original, const muse::io::path_t& workDir)
{
    removeCopy();
    m_original = original;
    m_stamp = stamp(original);
    m_path = original;
    m_walApplied = false;
    m_walError.clear();

    if (fileSystem()->entryType(original) != muse::io::EntryType::File) {
        return muse::make_ret(muse::Ret::Code::BadArgs, "not a file: " + original.toStdString());
    }

    const muse::io::path_t wal = original + "-wal";
    m_walSize = 0;
    if (fileSystem()->exists(wal)) {
        const muse::RetVal<uint64_t> walSize = fileSystem()->fileSize(wal);
        if (!walSize.ret) {
            return walSize.ret;
        }
        m_walSize = walSize.val;
    }
    if (m_walSize == 0) {
        return muse::make_ok();
    }

    // we are working in a temporary folder, DO NOT touch the original file(s)
    const muse::io::path_t parent = workDir.empty()
                                    ? fileSystem()->temporaryDirectoryPath().appendingComponent("audacity-projectrecovery")
                                    : workDir;
    const size_t tag = std::hash<std::string> {}(fileSystem()->absoluteFilePath(original).toStdString());
    static std::atomic<uint64_t> copies { 0 };
    const muse::io::path_t folder = parent.appendingComponent(muse::io::completeBasename(original) + "-" + std::to_string(tag) + "-"
                                                              + std::to_string(++copies));
    const muse::io::path_t target = folder.appendingComponent(muse::io::filename(original));
    if (fileSystem()->absoluteFilePath(target) == fileSystem()->absoluteFilePath(original)) {
        return muse::make_ret(muse::Ret::Code::BadArgs, "the copy would replace the original: " + original.toStdString());
    }
    muse::Ret ret = fileSystem()->makePath(folder);
    if (!ret) {
        return ret;
    }

    m_path = target;
    m_workDir = folder;
    m_isCopy = true;
    for (const char* suffix : { "", "-wal" }) {
        ret = fileSystem()->copy(original + suffix, target + suffix, true);
        if (!ret) {
            removeCopy();
            m_path = original;
            return ret;
        }
    }

    Database database;
    ret = database.open(target, Database::Mode::ReadWrite);
    if (ret) {
        int64_t busy = 0;
        ret = database.query("PRAGMA wal_checkpoint(TRUNCATE);", {}, [&busy](const DatabaseRow& row) {
            busy = row.integer(0);
        });
        if (ret && busy != 0) {
            ret = muse::make_ret(muse::Ret::Code::InternalError, "another connection blocks the checkpoint");
        }
    }
    m_walApplied = ret.success();
    if (!ret) {
        m_walError = ret.text();
        LOGW() << "the -wal frames of " << original << " were not applied: " << ret.toString();
    }
    return muse::make_ok();
}

bool WorkingCopy::originalChanged() const
{
    return !(stamp(m_original) == m_stamp);
}

std::vector<WorkingCopy::FileStamp> WorkingCopy::stamp(const muse::io::path_t& original) const
{
    std::vector<FileStamp> stamps;
    for (const muse::io::path_t& file : { original, original + "-wal" }) {
        FileStamp stamp;
        stamp.exists = fileSystem()->exists(file);
        if (stamp.exists) {
            stamp.size = fileSystem()->fileSize(file).val;
            stamp.modified = fileSystem()->lastModified(file);
        }
        stamps.push_back(stamp);
    }
    return stamps;
}

void WorkingCopy::removeCopy()
{
    if (!m_isCopy) {
        return;
    }
    for (const char* suffix : { "", "-wal", "-shm" }) {
        const muse::io::path_t file = m_path + suffix;
        if (fileSystem()->exists(file)) {
            fileSystem()->remove(file);
        }
    }
    fileSystem()->remove(m_workDir, true);
    m_isCopy = false;
}
