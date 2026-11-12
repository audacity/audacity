/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "projectrecovery/internal/binaryxml.h"

#include <cstdint>
#include <filesystem>
#include <string>
#include <vector>

namespace au::projectrecovery::tests {
struct TestBlock {
    int64_t id = 0;
    int64_t samples = 0;
    //! False: the document references the block, but the file has no row for it
    bool stored = true;
};

struct TestClip {
    std::string name;
    double offset = 0;
    std::vector<TestBlock> blocks;
};

struct TestTrack {
    std::string name;
    double rate = 44100;
    std::vector<TestClip> clips;
};

struct TestProject {
    std::vector<TestTrack> tracks;
};

//! A project document with the elements and attributes that recovery reads
XmlDocument makeDocument(const TestProject& project);

//! Writes a project file as ProjectFileIO does: the schema, the project row and the stored blocks
void writeProjectFile(const std::filesystem::path& path, const TestProject& project);

//! Adds an autosave row to a project file
void writeAutosave(const std::filesystem::path& path, const TestProject& autosave);

//! Writes a project file, then adds rows that stay in its -wal file; the file and its -wal are copied to `path`
void writeProjectFileWithWal(const std::filesystem::path& path, const TestProject& project, const std::vector<TestBlock>& walBlocks);

std::vector<uint8_t> readFileBytes(const std::filesystem::path& path);
}
