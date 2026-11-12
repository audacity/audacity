/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/database.h"
#include "projectrecovery/internal/projectdocument.h"
#include "projectrecovery/internal/projectrepairer.h"

#include "testprojectbuilder.h"

#include <fstream>

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
class ProjectRepairerTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        const auto* info = ::testing::UnitTest::GetInstance()->current_test_info();
        m_dir = std::filesystem::temp_directory_path() / "au_projectrecovery_tests" / info->name();
        std::filesystem::remove_all(m_dir);
        std::filesystem::create_directories(m_dir);
    }

    void TearDown() override
    {
        std::error_code ec;
        std::filesystem::remove_all(m_dir, ec);
    }

    std::filesystem::path file(const std::string& name) const { return m_dir / name; }

    muse::RetVal<RepairReport> repair(const std::filesystem::path& original, const std::filesystem::path& output) const
    {
        return ProjectRepairer().repair(muse::io::path_t(original.string()), muse::io::path_t(output.string()),
                                        muse::io::path_t((m_dir / "work").string()));
    }

    static void expectNoSideFiles(const std::filesystem::path& path)
    {
        EXPECT_FALSE(std::filesystem::exists(path.string() + "-wal"));
        EXPECT_FALSE(std::filesystem::exists(path.string() + "-shm"));
    }

    std::filesystem::path m_dir;
};

TestProject voiceProject(bool firstBlockStored, bool lastBlockStored)
{
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, {
                                   { "Take 1", 1.0, { { 1, 262144, firstBlockStored }, { 2, 262144 } } },
                                   { "Take 2", 20.0, { { 3, 1000, lastBlockStored } } },
                               } });
    return project;
}

BinaryXml readDocumentRow(const std::filesystem::path& path, const std::string& table)
{
    BinaryXml binary;
    Database database;
    EXPECT_TRUE(database.open(muse::io::path_t(path.string()), Database::Mode::Immutable));
    EXPECT_TRUE(database.query("SELECT dict, doc FROM " + table + " WHERE id = 1;", {}, [&binary](const DatabaseRow& row) {
        binary = { row.blob(0), row.blob(1) };
    }));
    return binary;
}

std::vector<int64_t> blockIds(const std::filesystem::path& path, const std::string& table)
{
    const muse::RetVal<XmlDocument> decoded = decodeBinaryXml(readDocumentRow(path, table));
    EXPECT_TRUE(decoded.ret) << decoded.ret.toString();
    std::vector<int64_t> ids;
    const auto project = readProjectDocument(decoded.val);
    if (!project) {
        return ids;
    }
    for (const WaveTrack& track : project->waveTracks) {
        for (const WaveClip& clip : track.clips) {
            for (const Sequence& sequence : clip.sequences) {
                for (const WaveBlock& block : sequence.blocks) {
                    ids.push_back(block.blockId);
                }
            }
        }
    }
    return ids;
}
}

TEST_F(ProjectRepairerTests, MissingBlocksBecomeSilenceOfTheSameLength)
{
    //! [GIVEN] A project without the rows of block 1 and block 3
    const auto original = file("missing.aup4");
    writeProjectFile(original, voiceProject(false, false));
    const auto originalBytes = readFileBytes(original);
    const auto output = file("missing (Recovered).aup4");

    //! [WHEN] It is repaired
    const muse::RetVal<RepairReport> report = repair(original, output);

    //! [THEN] The output references silence in place of the two blocks, and its check finds no problem
    ASSERT_TRUE(report.ret) << report.ret.toString();
    ASSERT_EQ(report.val.silencedBlocks.size(), 2u);
    EXPECT_EQ(report.val.silencedBlocks[0].id, 1);
    EXPECT_EQ(report.val.silencedBlocks[0].clip, "Take 1");
    EXPECT_EQ(report.val.silencedBlocks[1].id, 3);
    EXPECT_TRUE(report.val.verification.isHealthy());
    EXPECT_EQ(blockIds(output, "project"), (std::vector<int64_t> { -262144, 2, -1000 }));
    expectNoSideFiles(output);

    //! [THEN] The original does not change
    EXPECT_EQ(readFileBytes(original), originalBytes);
    expectNoSideFiles(original);
}

TEST_F(ProjectRepairerTests, OnlyTheAutosaveIsRepairedWhenAudacityLoadsIt)
{
    //! [GIVEN] A complete project document and an autosave that references a block the file does not hold
    const auto original = file("autosave.aup4");
    writeProjectFile(original, voiceProject(true, true));
    TestProject autosave = voiceProject(true, true);
    autosave.tracks.front().clips.push_back({ "Take 3", 30.0, { { 9, 4096, false } } });
    writeAutosave(original, autosave);
    const auto output = file("autosave (Recovered).aup4");

    //! [WHEN] It is repaired
    const muse::RetVal<RepairReport> report = repair(original, output);

    //! [THEN] The autosave has silence in place of block 9, and the project row has the same bytes as before
    ASSERT_TRUE(report.ret) << report.ret.toString();
    EXPECT_EQ(report.val.verification.activeDocument, "autosave");
    EXPECT_EQ(blockIds(output, "autosave"), (std::vector<int64_t> { 1, 2, 3, -4096 }));
    const BinaryXml before = readDocumentRow(original, "project");
    const BinaryXml after = readDocumentRow(output, "project");
    EXPECT_EQ(after.dict, before.dict);
    EXPECT_EQ(after.doc, before.doc);
}

TEST_F(ProjectRepairerTests, RowsInTheWalFileAreInTheOutput)
{
    //! [GIVEN] A project whose block 1 exists only in the -wal file, and whose block 3 is missing
    const auto original = file("wal.aup4");
    writeProjectFileWithWal(original, voiceProject(false, false), { { 1, 262144 } });
    const auto walPath = std::filesystem::path(original.string() + "-wal");
    const auto originalBytes = readFileBytes(original);
    const auto walBytes = readFileBytes(walPath);
    const auto output = file("wal (Recovered).aup4");

    //! [WHEN] It is repaired
    const muse::RetVal<RepairReport> report = repair(original, output);

    //! [THEN] Only block 3 is silence, the output has no -wal file, and the original and its -wal file do not change
    ASSERT_TRUE(report.ret) << report.ret.toString();
    ASSERT_EQ(report.val.silencedBlocks.size(), 1u);
    EXPECT_EQ(report.val.silencedBlocks.front().id, 3);
    EXPECT_EQ(report.val.verification.database.blockRows, 2);
    EXPECT_EQ(blockIds(output, "project"), (std::vector<int64_t> { 1, 2, -1000 }));
    expectNoSideFiles(output);
    EXPECT_EQ(readFileBytes(original), originalBytes);
    EXPECT_EQ(readFileBytes(walPath), walBytes);
}

TEST_F(ProjectRepairerTests, AnExistingFileIsNotReplaced)
{
    const auto original = file("missing.aup4");
    writeProjectFile(original, voiceProject(false, true));
    const auto output = file("taken.aup4");
    std::ofstream(output) << "Another file";
    const auto outputBytes = readFileBytes(output);

    EXPECT_FALSE(repair(original, output).ret);
    EXPECT_EQ(readFileBytes(output), outputBytes);
}

TEST_F(ProjectRepairerTests, AHealthyProjectGetsNoOutput)
{
    const auto original = file("healthy.aup4");
    writeProjectFile(original, voiceProject(true, true));
    const auto output = file("healthy (Recovered).aup4");

    EXPECT_FALSE(repair(original, output).ret);
    EXPECT_FALSE(std::filesystem::exists(output));
    expectNoSideFiles(output);
}

TEST_F(ProjectRepairerTests, AProblemWithoutARepairGetsNoOutput)
{
    //! [GIVEN] A project whose document is cut off in the middle
    const auto original = file("damaged.aup4");
    writeProjectFile(original, voiceProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(original.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("UPDATE project SET doc = substr(doc, 1, length(doc) / 2);"));
    }
    const auto output = file("damaged (Recovered).aup4");

    //! [WHEN] It is repaired
    const muse::RetVal<RepairReport> report = repair(original, output);

    //! [THEN] The repair fails with the reason, and no output stays
    EXPECT_FALSE(report.ret);
    EXPECT_EQ(report.ret.code(), static_cast<int>(muse::Ret::Code::NotSupported));
    EXPECT_FALSE(std::filesystem::exists(output));
    expectNoSideFiles(output);
}
