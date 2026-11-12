/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrepairer.h"

#include <filesystem>
#include <set>
#include <string>
#include <system_error>

#include "global/io/ioretcodes.h"

#include "database.h"
#include "documentedit.h"
#include "workingcopy.h"

using namespace au::projectrecovery;

namespace {
constexpr const char* SIDE_FILES[] = { "-wal", "-shm" };

std::string problemText(Problem problem)
{
    switch (problem) {
    case Problem::DatabaseNotReadable:
        return "the project database could not be read";
    case Problem::NotAudacityProject:
        return "the file is not an Audacity project";
    case Problem::WalNotApplied:
        return "the -wal file cannot be applied";
    case Problem::NewerProjectVersion:
        return "the project was saved by a newer version of Audacity";
    case Problem::NoDocument:
        return "the project has no document";
    case Problem::DocumentNotReadable:
        return "the project document can't be read";
    case Problem::MissingBlocks:
        return "the project references blocks missing from the database";
    case Problem::MissingBlockWithoutLength:
        return "a missing block has no length";
    case Problem::IntegrityErrors:
        return "the database has integrity errors";
    }
    return "an unknown problem";
}

//! Only missing blocks have a repair for now
muse::Ret checkRepairable(const Inspection& inspection)
{
    std::string others;
    for (const Problem problem : inspection.problems) {
        if (problem != Problem::MissingBlocks) {
            others += (others.empty() ? "" : "; ") + problemText(problem);
        }
    }
    if (!others.empty()) {
        return muse::make_ret(muse::Ret::Code::NotSupported, "no repair for these problems: " + others);
    }
    if (!inspection.has(Problem::MissingBlocks)) {
        return muse::make_ret(muse::Ret::Code::BadArgs, "the project has no problem to repair");
    }
    return muse::make_ok();
}
}

muse::RetVal<RepairReport> ProjectRepairer::repair(const muse::io::path_t& original, const muse::io::path_t& output,
                                                   const muse::io::path_t& workDir) const
{
    // SQLite would apply a -wal file that it finds next to the output
    for (const char* suffix : { "", "-wal", "-shm" }) {
        if (fileSystem()->exists(output + suffix)) {
            return muse::RetVal<RepairReport>::make_ret(muse::make_ret(muse::Ret::Code::BadArgs,
                                                                       "the file exists: " + (output + suffix).toStdString()));
        }
    }

    muse::Ret ret = copyToOutput(original, output, workDir);
    if (!ret) {
        return muse::RetVal<RepairReport>::make_ret(ret);
    }

    // The output has no -wal frames, so this inspection reads what the repair edits
    muse::RetVal<InspectedProject> inspected = ProjectInspector().inspectProject(output);
    const Inspection& inspection = inspected.val.inspection;
    ret = inspected.ret ? checkRepairable(inspection) : inspected.ret;
    if (ret && !inspected.val.state) {
        ret = muse::make_ret(muse::Ret::Code::InternalError, "no document to repair");
    }
    if (ret) {
        std::set<int64_t> ids;
        for (const MissingBlock& block : inspection.missingBlocks) {
            ids.insert(block.id);
        }
        ret = silenceBlocks(inspected.val.state->document, ids);
        inspected.val.state->promoted = inspected.val.state->table == "autosave";
    }
    if (ret) {
        ret = writeState(output, *inspected.val.state);
    }
    if (!ret) {
        removeOutput(output);
        return muse::RetVal<RepairReport>::make_ret(ret);
    }

    const muse::RetVal<Inspection> verification = ProjectInspector().inspect(output);
    if (!verification.ret || !verification.val.isHealthy()) {
        removeOutput(output);
        std::string problems = verification.ret ? std::string() : "; " + verification.ret.text();
        for (const Problem problem : verification.val.problems) {
            problems += "; " + problemText(problem);
        }
        return muse::RetVal<RepairReport>::make_ret(muse::make_ret(muse::Ret::Code::InternalError,
                                                                   "the output failed its check and was deleted" + problems));
    }

    RepairReport report;
    report.silencedBlocks = inspection.missingBlocks;
    report.verification = verification.val;
    return muse::RetVal<RepairReport>::make_ok(report);
}

muse::Ret ProjectRepairer::copyToOutput(const muse::io::path_t& original, const muse::io::path_t& output,
                                        const muse::io::path_t& workDir) const
{
    WorkingCopy copy;
    muse::Ret ret = copy.prepare(original, workDir);
    if (!ret) {
        return ret;
    }
    // Without wal applied the content of the file misses commited data
    if (copy.walSize() > 0 && !copy.walApplied()) {
        return muse::make_ret(muse::Ret::Code::NotSupported, "the -wal file cannot be applied: " + copy.walError());
    }
    ret = fileSystem()->copy(copy.path(), output);
    if (!ret) {
        // Another repair created the output after the check in repair()
        if (ret.code() != static_cast<int>(muse::io::Err::FSAlreadyExists)) {
            removeOutput(output);
        }
        return ret;
    }
    if (copy.originalChanged()) {
        removeOutput(output);
        return muse::make_ret(muse::Ret::Code::Busy, "the project was changed by another program");
    }
    // The copy keeps the read-only flag of a read-only original
    std::error_code error;
    std::filesystem::permissions(output.toStdWString(), std::filesystem::perms::owner_write, std::filesystem::perm_options::add, error);
    if (error) {
        removeOutput(output);
        return muse::make_ret(muse::Ret::Code::UnknownError, "cannot make " + output.toStdString() + " writable: " + error.message());
    }
    return muse::make_ok();
}

muse::Ret ProjectRepairer::writeState(const muse::io::path_t& output, const ProjectState& state) const
{
    Database database;
    muse::Ret ret = database.open(output, Database::Mode::ReadWrite);
    if (!ret) {
        return ret;
    }
    ret = database.exec("BEGIN;");
    if (ret) {
        const BinaryXml binary = encodeBinaryXml(state.document);
        const std::string table = state.promoted ? "project" : state.table;
        ret = database.query("INSERT OR REPLACE INTO " + table + "(id, dict, doc) VALUES(1, ?1, ?2);", { binary.dict, binary.doc });
    }
    muse::RetVal<bool> hasHistory;
    if (ret && state.promoted) {
        hasHistory = database.hasTable("project_history");
        ret = hasHistory.ret;
    }
    // As ProjectFileIO::WriteDoc writes a save: the project row, then the same document in the journal of the last 10
    if (ret && state.promoted && hasHistory.val) {
        ret = database.exec("INSERT INTO project_history(saved_at, dict, doc)"
                            " SELECT CAST(strftime('%s','now') AS INTEGER), dict, doc FROM project WHERE id = 1;"
                            "DELETE FROM project_history WHERE generation NOT IN"
                            " (SELECT generation FROM project_history ORDER BY generation DESC LIMIT 10);");
    }
    if (ret && state.promoted) {
        ret = database.exec("DELETE FROM autosave;");
    }
    if (ret) {
        ret = database.exec("COMMIT;");
    } else {
        database.exec("ROLLBACK;");
    }
    database.close();
    if (!ret) {
        return ret;
    }

    // Audacity opens a project with a -wal file as one that did not close
    for (const char* suffix : SIDE_FILES) {
        if (fileSystem()->exists(output + suffix)) {
            return muse::make_ret(muse::Ret::Code::InternalError, std::string("the output keeps its ") + suffix + " file");
        }
    }
    return muse::make_ok();
}

void ProjectRepairer::removeOutput(const muse::io::path_t& output) const
{
    for (const char* suffix : { "", "-wal", "-shm" }) {
        if (fileSystem()->exists(output + suffix)) {
            fileSystem()->remove(output + suffix);
        }
    }
}
