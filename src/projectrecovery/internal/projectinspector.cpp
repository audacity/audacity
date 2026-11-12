/*
 * Audacity: A Digital Audio Editor
 */
#include "projectinspector.h"

#include <algorithm>
#include <cstdio>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <utility>

#include "binaryxml.h"
#include "database.h"
#include "projectdocument.h"
#include "workingcopy.h"

#include "au3-project/ProjectFormatVersion.h"

using namespace au::projectrecovery;

namespace {
struct StoredDocument {
    std::string source;
    BinaryXml binary;
    std::optional<ProjectDocument> project;
    std::string error;
    std::optional<XmlDocument> xml = std::nullopt;
};

struct BlockReference {
    const WaveTrack* track = nullptr;
    const WaveClip* clip = nullptr;
    WaveBlock block;
    double startSeconds = 0;
    double endSeconds = 0;
};

std::vector<BlockReference> collectReferences(const ProjectDocument& project)
{
    std::vector<BlockReference> references;
    for (const WaveTrack& track : project.waveTracks) {
        const double rate = track.rate > 0 ? track.rate : 1;
        for (const WaveClip& clip : track.clips) {
            const double stretch = effectiveStretchRatio(clip, project);
            for (const Sequence& sequence : clip.sequences) {
                for (const WaveBlock& block : sequence.blocks) {
                    const double start = clip.offset + static_cast<double>(block.start) * stretch / rate;
                    const double end = start + static_cast<double>(block.length) * stretch / rate;
                    references.push_back({ &track, &clip, block, start, end });
                }
            }
        }
    }
    return references;
}

//! As ProjectFileIO::HandleXMLTag, which refuses the project element without them
bool hasVersions(const ProjectDocument& project)
{
    int major = 0;
    return !project.audacityVersion.empty() && std::sscanf(project.version.c_str(), "%i", &major) == 1;
}

void readDocuments(Database& database, std::vector<StoredDocument>& documents)
{
    for (const char* table : { "project", "autosave" }) {
        const muse::RetVal<bool> exists = database.hasTable(table);
        if (!exists.ret) {
            documents.push_back({ table, {}, std::nullopt, exists.ret.text() });
            continue;
        }
        if (!exists.val) {
            continue;
        }
        const muse::Ret ret
            = database.query(std::string("SELECT dict, doc FROM ") + table + " WHERE id = 1;", {}, [&](const DatabaseRow& row) {
            documents.push_back({ table, { row.blob(0), row.blob(1) }, std::nullopt, {} });
        });
        // skip documents that could not loaded in Audacity
        if (!ret) {
            documents.push_back({ table, {}, std::nullopt, ret.text() });
        }
    }
    const muse::RetVal<bool> hasHistory = database.hasTable("project_history");
    if (!hasHistory.ret) {
        documents.push_back({ "history", {}, std::nullopt, hasHistory.ret.text() });
    } else if (hasHistory.val) {
        const muse::Ret ret = database.query("SELECT generation, dict, doc FROM project_history ORDER BY generation;", {},
                                             [&](const DatabaseRow& row) {
            documents.push_back({ "history " + std::to_string(row.integer(0)), { row.blob(1), row.blob(2) }, std::nullopt, {} });
        });
        if (!ret) {
            documents.push_back({ "history", {}, std::nullopt, ret.text() });
        }
    }
}

void checkIntegrity(Database& database, size_t maxMessages, DatabaseInfo& info)
{
    std::vector<std::string> lines;
    const muse::Ret ret = database.query("PRAGMA integrity_check(" + std::to_string(maxMessages) + ");", {},
                                         [&lines](const DatabaseRow& row) {
        std::istringstream text(row.text(0));
        for (std::string line; std::getline(text, line);) {
            lines.push_back(line);
        }
    });
    info.integrityChecked = ret.success();
    if (!ret) {
        info.integrityMessages.push_back(ret.text());
    } else if (!(lines.size() == 1 && lines.front() == "ok")) {
        info.integrityMessages = std::move(lines);
    }
}

DocumentInfo summarize(StoredDocument& document, const std::set<int64_t>& blockIds, std::vector<BlockReference>& references)
{
    DocumentInfo summary;
    summary.source = document.source;
    if (!document.error.empty()) {
        summary.error = document.error;
        return summary;
    }
    muse::RetVal<XmlDocument> decoded = decodeBinaryXml(document.binary);
    if (decoded.ret) {
        document.project = readProjectDocument(decoded.val);
        document.xml = std::move(decoded.val);
        if (!document.project) {
            summary.error = "the document has no project element";
        } else if (!hasVersions(*document.project)) {
            summary.error = "the project element has no version or audacityversion attribute that Audacity reads";
            document.project.reset();
        }
    } else {
        summary.error = decoded.ret.text();
    }
    summary.readable = document.project.has_value();
    if (!document.project) {
        return summary;
    }

    const ProjectDocument& project = *document.project;
    summary.audacityVersion = project.audacityVersion;
    summary.waveTracks = project.waveTracks.size();
    for (const WaveTrack& track : project.waveTracks) {
        summary.clips += track.clips.size();
    }
    references = collectReferences(project);
    std::set<int64_t> distinct;
    std::set<int64_t> missing;
    for (const BlockReference& reference : references) {
        if (reference.block.blockId > 0) {
            distinct.insert(reference.block.blockId);
            if (!blockIds.count(reference.block.blockId)) {
                missing.insert(reference.block.blockId);
            }
        }
    }
    summary.blockReferences = references.size();
    summary.distinctBlocks = distinct.size();
    summary.missingBlocks = missing.size();
    return summary;
}
}

bool Inspection::has(Problem problem) const
{
    return std::find(problems.begin(), problems.end(), problem) != problems.end();
}

muse::RetVal<Inspection> ProjectInspector::inspect(const muse::io::path_t& path, const InspectionOptions& options) const
{
    const muse::RetVal<InspectedProject> inspected = inspectProject(path, options);
    return inspected.ret ? muse::RetVal<Inspection>::make_ok(inspected.val.inspection) : muse::RetVal<Inspection>::make_ret(inspected.ret);
}

muse::RetVal<InspectedProject> ProjectInspector::inspectProject(const muse::io::path_t& path, const InspectionOptions& options) const
{
    WorkingCopy copy;
    const muse::Ret prepared = copy.prepare(path, options.workDir);
    if (!prepared) {
        return muse::RetVal<InspectedProject>::make_ret(prepared);
    }

    InspectedProject result;
    Inspection& inspection = result.inspection;
    DatabaseInfo& info = inspection.database;
    info.path = path;
    info.fileSize = fileSystem()->fileSize(path).val;
    info.walSize = copy.walSize();
    info.walApplied = copy.walApplied();
    info.walError = copy.walError();
    if (info.walSize > 0 && !info.walApplied) {
        inspection.problems.push_back(Problem::WalNotApplied);
    }

    // If file is being changed by another app, refuse to continue
    const auto finish = [&copy, &result]() {
        if (copy.originalChanged()) {
            return muse::RetVal<InspectedProject>::make_ret(muse::make_ret(muse::Ret::Code::Busy,
                                                                           "the project was changed by another program"));
        }
        return muse::RetVal<InspectedProject>::make_ok(result);
    };

    // Nothing writes either file now: the copy has its -wal frames applied, and the original has none
    Database database;
    std::set<int64_t> blockIds;
    muse::Ret ret = database.open(copy.path(), Database::Mode::Immutable);
    if (ret) {
        ret = database.query("SELECT blockid FROM sampleblocks;", {}, [&blockIds](const DatabaseRow& row) {
            blockIds.insert(row.integer(0));
        });
    }
    info.opens = ret.success();
    if (!info.opens) {
        inspection.problems.push_back(Problem::DatabaseNotReadable);
        return finish();
    }
    info.blockRows = static_cast<int64_t>(blockIds.size());
    const std::pair<const char*, int64_t*> pragmas[] = { { "page_size", &info.pageSize }, { "page_count", &info.pageCount },
        { "freelist_count", &info.freePageCount }, { "application_id", &info.applicationId }, { "user_version", &info.userVersion } };
    for (const auto& [name, field] : pragmas) {
        const muse::RetVal<int64_t> value = database.pragmaInteger(name);
        if (!value.ret) {
            inspection.problems.push_back(Problem::DatabaseNotReadable);
            return finish();
        }
        *field = value.val;
    }
    // Refuse opening unknown project types or newer versions
    if (info.applicationId != AUDACITY_APPLICATION_ID) {
        inspection.problems.push_back(Problem::NotAudacityProject);
    }
    const ProjectFormatVersion fileVersion = ProjectFormatVersion::FromPacked(static_cast<uint32_t>(info.userVersion));
    if (ProjectSchemaVersion < fileVersion) {
        inspection.problems.push_back(Problem::NewerProjectVersion);
    }

    std::vector<StoredDocument> documents;
    readDocuments(database, documents);
    std::map<std::string, std::vector<BlockReference> > references;
    for (StoredDocument& document : documents) {
        inspection.documents.push_back(summarize(document, blockIds, references[document.source]));
    }

    // As ProjectFileIO::LoadProject: the autosave row when there is one, else the project row
    const auto autosave = std::find_if(documents.begin(), documents.end(), [](const StoredDocument& d) {
        return d.source == "autosave";
    });
    const auto project = std::find_if(documents.begin(), documents.end(), [](const StoredDocument& d) {
        return d.source == "project";
    });
    const auto loaded = autosave != documents.end() ? autosave : project;
    if (loaded == documents.end()) {
        inspection.problems.push_back(Problem::NoDocument);
    } else if (!loaded->project) {
        inspection.activeDocument = loaded->source;
        inspection.problems.push_back(Problem::DocumentNotReadable);
    } else {
        inspection.activeDocument = loaded->source;
        result.state = ProjectState { loaded->source, std::move(*loaded->xml) };
        for (const BlockReference& reference : references[loaded->source]) {
            const int64_t id = reference.block.blockId;
            if (id <= 0 || blockIds.count(id)) {
                continue;
            }
            inspection.missingBlocks.push_back({ id, reference.block.length, reference.track->name, reference.clip->name,
                                                 reference.startSeconds, reference.endSeconds });
        }
        if (!inspection.missingBlocks.empty()) {
            inspection.problems.push_back(Problem::MissingBlocks);
        }
        const bool withoutLength = std::any_of(inspection.missingBlocks.begin(), inspection.missingBlocks.end(),
                                               [](const MissingBlock& block) { return block.samples < 0; });
        if (withoutLength) {
            inspection.problems.push_back(Problem::MissingBlockWithoutLength);
        }
    }

    if (options.checkIntegrity) {
        checkIntegrity(database, options.maxIntegrityMessages, info);
        if (!info.integrityMessages.empty()) {
            inspection.problems.push_back(Problem::IntegrityErrors);
        }
    }
    return finish();
}
