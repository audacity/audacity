/*
 * Audacity: A Digital Audio Editor
 */
#include "testprojectbuilder.h"

#include <gtest/gtest.h>
#include <sqlite3.h>

#include <fstream>
#include <iterator>

#include "au3-sqlite-helpers/sqlite/SQLiteUtils.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
// The schema of ProjectFileIO.cpp; application_id 'AUDY'
constexpr const char* PROJECT_SCHEMA
    = "PRAGMA application_id = 1096107097;"
      "PRAGMA user_version = 67108864;"
      "CREATE TABLE project(id INTEGER PRIMARY KEY, dict BLOB, doc BLOB);"
      "CREATE TABLE autosave(id INTEGER PRIMARY KEY, dict BLOB, doc BLOB);"
      "CREATE TABLE sampleblocks(blockid INTEGER PRIMARY KEY AUTOINCREMENT, sampleformat INTEGER, summin REAL, summax REAL,"
      " sumrms REAL, summary256 BLOB, summary64k BLOB, samples BLOB);"
      "CREATE TABLE project_history(generation INTEGER PRIMARY KEY AUTOINCREMENT, saved_at INTEGER, dict BLOB, doc BLOB);";

constexpr int64_t FLOAT_SAMPLE_FORMAT = 0x4000F;

XmlAttribute stringAttribute(const std::string& name, const std::string& value)
{
    XmlAttribute attribute;
    attribute.name = name;
    attribute.value.type = FieldType::String;
    attribute.value.text = value;
    return attribute;
}

XmlAttribute integerAttribute(const std::string& name, FieldType type, int64_t value)
{
    XmlAttribute attribute;
    attribute.name = name;
    attribute.value.type = type;
    attribute.value.integer = value;
    return attribute;
}

XmlAttribute doubleAttribute(const std::string& name, double value)
{
    XmlAttribute attribute;
    attribute.name = name;
    attribute.value.type = FieldType::Double;
    attribute.value.real = value;
    attribute.value.digits = 19;
    return attribute;
}

sqlite3* openDatabase(const std::filesystem::path& path)
{
    sqlite3* db = nullptr;
    EXPECT_FALSE(audacity::sqlite::Initialize().IsError());
    EXPECT_EQ(sqlite3_open(path.string().c_str(), &db), SQLITE_OK);
    return db;
}

void execute(sqlite3* db, const std::string& sql)
{
    char* message = nullptr;
    EXPECT_EQ(sqlite3_exec(db, sql.c_str(), nullptr, nullptr, &message), SQLITE_OK) << (message ? message : "");
    sqlite3_free(message);
}

void insertDocument(sqlite3* db, const char* table, const TestProject& project)
{
    const BinaryXml binary = encodeBinaryXml(makeDocument(project));
    sqlite3_stmt* statement = nullptr;
    const std::string sql = std::string("INSERT INTO ") + table + "(id, dict, doc) VALUES(1, ?1, ?2);";
    ASSERT_EQ(sqlite3_prepare_v2(db, sql.c_str(), -1, &statement, nullptr), SQLITE_OK);
    sqlite3_bind_blob(statement, 1, binary.dict.constData(), static_cast<int>(binary.dict.size()), SQLITE_TRANSIENT);
    sqlite3_bind_blob(statement, 2, binary.doc.constData(), static_cast<int>(binary.doc.size()), SQLITE_TRANSIENT);
    EXPECT_EQ(sqlite3_step(statement), SQLITE_DONE);
    sqlite3_finalize(statement);
}

void insertBlock(sqlite3* db, const TestBlock& block)
{
    std::vector<float> samples(static_cast<size_t>(block.samples));
    for (size_t i = 0; i < samples.size(); ++i) {
        samples[i] = static_cast<float>(i % 100) / 100.0f;
    }
    sqlite3_stmt* statement = nullptr;
    ASSERT_EQ(sqlite3_prepare_v2(db,
                                 "INSERT INTO sampleblocks(blockid, sampleformat, summin, summax, sumrms, summary256, summary64k, samples)"
                                 " VALUES(?1, ?2, 0, 1, 0.5, x'', x'', ?3);", -1, &statement, nullptr), SQLITE_OK);
    sqlite3_bind_int64(statement, 1, block.id);
    sqlite3_bind_int64(statement, 2, FLOAT_SAMPLE_FORMAT);
    sqlite3_bind_blob(statement, 3, samples.data(), static_cast<int>(samples.size() * sizeof(float)), SQLITE_TRANSIENT);
    EXPECT_EQ(sqlite3_step(statement), SQLITE_DONE);
    sqlite3_finalize(statement);
}

void insertStoredBlocks(sqlite3* db, const TestProject& project)
{
    for (const TestTrack& track : project.tracks) {
        for (const TestClip& clip : track.clips) {
            for (const TestBlock& block : clip.blocks) {
                if (block.stored) {
                    insertBlock(db, block);
                }
            }
        }
    }
}
}

XmlDocument au::projectrecovery::tests::makeDocument(const TestProject& project)
{
    XmlNode root;
    root.tag = "project";
    root.attributes.push_back(stringAttribute("version", "1.3.0"));
    root.attributes.push_back(stringAttribute("audacityversion", "4.0.2"));
    root.attributes.push_back(doubleAttribute("rate", 44100));
    for (const TestTrack& track : project.tracks) {
        XmlNode trackNode;
        trackNode.tag = "wavetrack";
        trackNode.attributes.push_back(stringAttribute("name", track.name));
        trackNode.attributes.push_back(integerAttribute("channel", FieldType::Int, 0));
        trackNode.attributes.push_back(integerAttribute("linked", FieldType::Int, 0));
        trackNode.attributes.push_back(doubleAttribute("rate", track.rate));
        for (const TestClip& clip : track.clips) {
            XmlNode clipNode;
            clipNode.tag = "waveclip";
            clipNode.attributes.push_back(doubleAttribute("offset", clip.offset));
            clipNode.attributes.push_back(doubleAttribute("trimLeft", 0));
            clipNode.attributes.push_back(doubleAttribute("trimRight", 0));
            clipNode.attributes.push_back(doubleAttribute("clipStretchRatio", 1));
            clipNode.attributes.push_back(stringAttribute("name", clip.name));
            XmlNode sequenceNode;
            sequenceNode.tag = "sequence";
            int64_t start = 0;
            for (const TestBlock& block : clip.blocks) {
                XmlNode blockNode;
                blockNode.tag = "waveblock";
                blockNode.attributes.push_back(integerAttribute("start", FieldType::LongLong, start));
                blockNode.attributes.push_back(integerAttribute("blockid", FieldType::LongLong, block.id));
                sequenceNode.children.push_back(std::move(blockNode));
                start += block.samples;
            }
            sequenceNode.attributes.push_back(integerAttribute("maxsamples", FieldType::SizeT, 262144));
            sequenceNode.attributes.push_back(integerAttribute("sampleformat", FieldType::SizeT, FLOAT_SAMPLE_FORMAT));
            sequenceNode.attributes.push_back(integerAttribute("numsamples", FieldType::LongLong, start));
            clipNode.children.push_back(std::move(sequenceNode));
            trackNode.children.push_back(std::move(clipNode));
        }
        root.children.push_back(std::move(trackNode));
    }
    XmlDocument document;
    document.elements.push_back(std::move(root));
    return document;
}

void au::projectrecovery::tests::writeProjectFile(const std::filesystem::path& path, const TestProject& project)
{
    sqlite3* db = openDatabase(path);
    execute(db, PROJECT_SCHEMA);
    insertDocument(db, "project", project);
    insertStoredBlocks(db, project);
    sqlite3_close(db);
}

void au::projectrecovery::tests::writeAutosave(const std::filesystem::path& path, const TestProject& autosave)
{
    sqlite3* db = openDatabase(path);
    insertDocument(db, "autosave", autosave);
    sqlite3_close(db);
}

void au::projectrecovery::tests::writeProjectFileWithWal(const std::filesystem::path& path, const TestProject& project,
                                                         const std::vector<TestBlock>& walBlocks)
{
    const std::filesystem::path source = path.parent_path() / ("source-" + path.filename().string());
    writeProjectFile(source, project);

    // While this connection is open, its -wal file keeps the new rows
    sqlite3* db = openDatabase(source);
    execute(db, "PRAGMA journal_mode = WAL; PRAGMA wal_autocheckpoint = 0;");
    for (const TestBlock& block : walBlocks) {
        insertBlock(db, block);
    }
    for (const char* suffix : { "", "-wal" }) {
        std::filesystem::copy_file(source.string() + suffix, path.string() + suffix,
                                   std::filesystem::copy_options::overwrite_existing);
    }
    sqlite3_close(db);
}

std::vector<uint8_t> au::projectrecovery::tests::readFileBytes(const std::filesystem::path& path)
{
    std::ifstream stream(path, std::ios::binary);
    return std::vector<uint8_t>(std::istreambuf_iterator<char>(stream), std::istreambuf_iterator<char>());
}
