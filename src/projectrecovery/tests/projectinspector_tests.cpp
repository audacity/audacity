/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/database.h"
#include "projectrecovery/internal/projectinspector.h"
#include "projectrecovery/internal/workingcopy.h"

#include "testprojectbuilder.h"

#include "au3-project/ProjectFormatVersion.h"

#include <fstream>

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
class ProjectInspectorTests : public ::testing::Test
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

    muse::RetVal<Inspection> inspect(const std::filesystem::path& path) const
    {
        InspectionOptions options;
        options.workDir = muse::io::path_t((m_dir / "work").string());
        return ProjectInspector().inspect(muse::io::path_t(path.string()), options);
    }

    std::filesystem::path m_dir;
};

TestProject twoClipProject(bool firstBlockStored, bool lastBlockStored)
{
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, {
                                   { "Take 1", 1.0, { { 1, 262144, firstBlockStored }, { 2, 262144 } } },
                                   { "Take 2", 20.0, { { 3, 1000, lastBlockStored } } },
                               } });
    return project;
}
}

TEST_F(ProjectInspectorTests, HealthyProject)
{
    //! [GIVEN] A project whose document references only stored blocks
    const auto path = file("healthy.aup4");
    writeProjectFile(path, twoClipProject(true, true));

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] It has no problems, and the facts of the file are read
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    EXPECT_TRUE(result.isHealthy());
    EXPECT_TRUE(result.database.opens);
    EXPECT_EQ(result.database.applicationId, 1096107097);
    EXPECT_EQ(result.database.blockRows, 3);
    EXPECT_EQ(result.database.orphanBlocks, 0);
    EXPECT_TRUE(result.database.integrityChecked);
    EXPECT_TRUE(result.database.integrityMessages.empty());
    EXPECT_EQ(result.activeDocument, "project");
    ASSERT_EQ(result.documents.size(), 1u);
    const DocumentInfo& document = result.documents.front();
    EXPECT_TRUE(document.readable);
    EXPECT_EQ(document.audacityVersion, "4.0.2");
    EXPECT_EQ(document.rate, 44100);
    EXPECT_EQ(document.waveTracks, 1u);
    EXPECT_EQ(document.clips, 2u);
    EXPECT_EQ(document.blockReferences, 3u);
    EXPECT_EQ(document.distinctBlocks, 3u);
    EXPECT_EQ(document.missingBlocks, 0u);
}

TEST_F(ProjectInspectorTests, MissingBlocksHaveTheirTrackClipAndTime)
{
    //! [GIVEN] A project without the rows of block 1 and block 3
    const auto path = file("missing.aup4");
    writeProjectFile(path, twoClipProject(false, false));

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] Both blocks are missing, each with its place in the project
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    EXPECT_TRUE(result.has(Problem::MissingBlocks));
    ASSERT_EQ(result.missingBlocks.size(), 2u);
    const MissingBlock& first = result.missingBlocks[0];
    EXPECT_EQ(first.id, 1);
    EXPECT_EQ(first.samples, 262144);
    EXPECT_EQ(first.track, "Voice");
    EXPECT_EQ(first.clip, "Take 1");
    EXPECT_DOUBLE_EQ(first.startSeconds, 1.0);
    EXPECT_DOUBLE_EQ(first.endSeconds, 1.0 + 262144.0 / 44100.0);
    const MissingBlock& second = result.missingBlocks[1];
    EXPECT_EQ(second.id, 3);
    EXPECT_EQ(second.clip, "Take 2");
    EXPECT_DOUBLE_EQ(second.startSeconds, 20.0);
    EXPECT_EQ(result.documents.front().missingBlocks, 2u);
}

TEST_F(ProjectInspectorTests, AMissingBlockThatTwoClipsShareIsReportedForEachClip)
{
    //! [GIVEN] Two clips that reference the same missing block, as a copied clip does
    const auto path = file("shared.aup4");
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, {
                                   { "Take 1", 1.0, { { 1, 262144, false } } },
                                   { "Take 1 copy", 20.0, { { 1, 262144, false } } },
                               } });
    writeProjectFile(path, project);

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] Both clips are reported, and the block is counted once
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    ASSERT_EQ(result.missingBlocks.size(), 2u);
    EXPECT_EQ(result.missingBlocks[0].clip, "Take 1");
    EXPECT_EQ(result.missingBlocks[1].clip, "Take 1 copy");
    EXPECT_EQ(result.documents.front().missingBlocks, 1u);
}

TEST_F(ProjectInspectorTests, RowsThatNoDocumentReferencesAreOrphans)
{
    //! [GIVEN] A healthy project with two more rows: one only the autosave references, one no document references
    const auto path = file("orphans.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    TestProject autosave = twoClipProject(true, true);
    autosave.tracks.front().clips.push_back({ "Take 3", 30.0, { { 8, 4096 } } });
    writeAutosave(path, autosave);
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("INSERT INTO sampleblocks(blockid, sampleformat, summin, summax, sumrms, summary256, summary64k, samples)"
                            " VALUES(8, 262159, 0, 0, 0, x'', x'', x''), (9, 262159, 0, 0, 0, x'', x'', x'');"));
    }

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] Only the row that no document references is an orphan
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_EQ(inspection.val.database.blockRows, 5);
    EXPECT_EQ(inspection.val.database.orphanBlocks, 1);
}

TEST_F(ProjectInspectorTests, AutosaveIsTheDocumentThatAudacityLoads)
{
    //! [GIVEN] A complete project document and an autosave that references a block the file does not hold
    const auto path = file("autosave.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    TestProject autosave = twoClipProject(true, true);
    autosave.tracks.front().clips.push_back({ "Take 3", 30.0, { { 9, 4096, false } } });
    writeAutosave(path, autosave);

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] The missing block of the autosave is reported, and the project document stays complete
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    EXPECT_EQ(result.activeDocument, "autosave");
    ASSERT_EQ(result.missingBlocks.size(), 1u);
    EXPECT_EQ(result.missingBlocks.front().id, 9);
    ASSERT_EQ(result.documents.size(), 2u);
    EXPECT_EQ(result.documents[0].source, "project");
    EXPECT_EQ(result.documents[0].missingBlocks, 0u);
    EXPECT_EQ(result.documents[1].source, "autosave");
    EXPECT_EQ(result.documents[1].missingBlocks, 1u);
}

TEST_F(ProjectInspectorTests, RowsInTheWalFileAreNotMissing)
{
    //! [GIVEN] A project whose block 1 exists only in the -wal file
    const auto path = file("wal.aup4");
    writeProjectFileWithWal(path, twoClipProject(false, true), { { 1, 262144 } });
    const auto walPath = std::filesystem::path(path.string() + "-wal");
    ASSERT_GT(std::filesystem::file_size(walPath), 0u);
    const auto originalBytes = readFileBytes(path);
    const auto walBytes = readFileBytes(walPath);

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] The frames are applied to a copy, and nothing is missing
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    EXPECT_TRUE(result.database.walApplied) << result.database.walError;
    EXPECT_GT(result.database.walSize, 0u);
    EXPECT_TRUE(result.isHealthy());

    //! [THEN] The original and its -wal file do not change, and the copy is gone
    EXPECT_EQ(readFileBytes(path), originalBytes);
    EXPECT_EQ(readFileBytes(walPath), walBytes);
    EXPECT_TRUE(!std::filesystem::exists(m_dir / "work") || std::filesystem::is_empty(m_dir / "work"));
}

TEST_F(ProjectInspectorTests, OriginalWithoutWalIsNotChanged)
{
    //! [GIVEN] A project with missing blocks and no -wal file
    const auto path = file("readonly.aup4");
    writeProjectFile(path, twoClipProject(false, true));
    const auto originalBytes = readFileBytes(path);
    const auto modified = std::filesystem::last_write_time(path);

    //! [WHEN] It is inspected
    ASSERT_TRUE(inspect(path).ret);

    //! [THEN] The file is the same, and SQLite made no -wal or -shm file next to it
    EXPECT_EQ(readFileBytes(path), originalBytes);
    EXPECT_TRUE(std::filesystem::last_write_time(path) == modified);
    EXPECT_FALSE(std::filesystem::exists(path.string() + "-wal"));
    EXPECT_FALSE(std::filesystem::exists(path.string() + "-shm"));
}

TEST_F(ProjectInspectorTests, FileThatIsNotADatabase)
{
    const auto path = file("text.aup4");
    std::ofstream(path) << "This is not an Audacity project.";

    const muse::RetVal<Inspection> inspection = inspect(path);

    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.has(Problem::DatabaseNotReadable));
}

TEST_F(ProjectInspectorTests, DocumentThatDoesNotDecode)
{
    //! [GIVEN] A project whose document is cut off in the middle
    const auto path = file("damaged.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("UPDATE project SET doc = substr(doc, 1, length(doc) / 2);"));
    }

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] The document is reported as not readable, with the reason
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    const Inspection& result = inspection.val;
    EXPECT_TRUE(result.has(Problem::DocumentNotReadable));
    ASSERT_EQ(result.documents.size(), 1u);
    EXPECT_FALSE(result.documents.front().readable);
    EXPECT_FALSE(result.documents.front().error.empty());
}

TEST_F(ProjectInspectorTests, FileWithoutDocument)
{
    //! [GIVEN] A project file with blocks but no project or autosave row, as an unsaved recording leaves it
    const auto path = file("nodocument.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("DELETE FROM project;"));
    }

    const muse::RetVal<Inspection> inspection = inspect(path);

    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.has(Problem::NoDocument));
    EXPECT_EQ(inspection.val.database.blockRows, 3);
}

TEST_F(ProjectInspectorTests, BlocksOfCutLinesAreNotReferenced)
{
    //! [GIVEN] A clip with nested cut lines, where the inner one references a block the file does not hold
    const auto path = file("nested.aup4");
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 1, 100 } } } } });
    writeProjectFile(path, project);
    XmlDocument document = makeDocument(project);
    XmlNode& clip = document.elements.front().children.front().children.front();
    XmlNode inner = clip;
    inner.children.front().children.front().attributes[1].value.integer = 7;
    XmlNode outer = clip;
    outer.children.push_back(inner);
    clip.children.push_back(outer);
    const BinaryXml binary = encodeBinaryXml(document);
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.query("UPDATE project SET dict = ?1, doc = ?2 WHERE id = 1;", { binary.dict, binary.doc }));
    }

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] Audacity does not load cut lines, so nothing is missing
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.missingBlocks.empty());
    EXPECT_TRUE(inspection.val.isHealthy());
}

TEST_F(ProjectInspectorTests, AMissingBlockAfterTheEndOfItsSequenceHasNoLength)
{
    //! [GIVEN] A missing block that starts after the sample count of its sequence
    const auto path = file("nolength.aup4");
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 1, 1000 }, { 2, 1000, false } } } } });
    writeProjectFile(path, project);
    XmlDocument document = makeDocument(project);
    XmlNode& clip = document.elements.front().children.front().children.front();
    for (XmlNode& child : clip.children) {
        if (child.tag == "sequence") {
            for (XmlAttribute& attribute : child.attributes) {
                if (attribute.name == "numsamples") {
                    attribute.value.integer = 500;
                }
            }
        }
    }
    const BinaryXml binary = encodeBinaryXml(document);
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.query("UPDATE project SET dict = ?1, doc = ?2 WHERE id = 1;", { binary.dict, binary.doc }));
    }

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] The block is missing, and no length of silence can replace it
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    ASSERT_EQ(inspection.val.missingBlocks.size(), 1u);
    EXPECT_EQ(inspection.val.missingBlocks.front().samples, -500);
    EXPECT_TRUE(inspection.val.has(Problem::MissingBlockWithoutLength));
}

TEST_F(ProjectInspectorTests, AChangeOfTheOriginalAfterTheCopyIsSeen)
{
    //! [GIVEN] A working copy of a project
    const auto path = file("changing.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    WorkingCopy copy;
    ASSERT_TRUE(copy.prepare(muse::io::path_t(path.string()), muse::io::path_t((m_dir / "work").string())));
    EXPECT_FALSE(copy.originalChanged());

    //! [WHEN] Another program writes to the original
    std::ofstream(path, std::ios::binary | std::ios::app) << "more";

    //! [THEN] The copy knows that what it read may mix two states
    EXPECT_TRUE(copy.originalChanged());
}

TEST_F(ProjectInspectorTests, AnAutosaveRowThatCannotBeReadIsNotReplacedByTheProject)
{
    //! [GIVEN] A healthy project document and an autosave table that SQLite cannot read the document from
    const auto path = file("autosavecolumns.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("DROP TABLE autosave; CREATE TABLE autosave(id INTEGER PRIMARY KEY, dict BLOB);"
                            " INSERT INTO autosave(id, dict) VALUES(1, x'00');"));
    }

    //! [WHEN] It is inspected
    const muse::RetVal<Inspection> inspection = inspect(path);

    //! [THEN] The autosave is the document that Audacity loads, and it is not readable
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_EQ(inspection.val.activeDocument, "autosave");
    EXPECT_TRUE(inspection.val.has(Problem::DocumentNotReadable));
}

TEST_F(ProjectInspectorTests, AFileOfAnotherApplicationIsNotAnAudacityProject)
{
    const auto path = file("foreign.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("PRAGMA application_id = 0;"));
    }

    const muse::RetVal<Inspection> inspection = inspect(path);

    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.has(Problem::NotAudacityProject));
}

TEST_F(ProjectInspectorTests, AProjectOfANewerSchemaIsNotSupported)
{
    const auto path = file("newer.aup4");
    writeProjectFile(path, twoClipProject(true, true));
    {
        Database db;
        ASSERT_TRUE(db.open(muse::io::path_t(path.string()), Database::Mode::ReadWrite));
        ASSERT_TRUE(db.exec("PRAGMA user_version = " + std::to_string(ProjectSchemaVersion.GetPacked() + 1) + ";"));
    }

    const muse::RetVal<Inspection> inspection = inspect(path);

    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.has(Problem::NewerProjectVersion));
    EXPECT_FALSE(inspection.val.has(Problem::NotAudacityProject));
}

TEST_F(ProjectInspectorTests, AWorkFolderThatHoldsTheOriginalLeavesTheOriginal)
{
    //! [GIVEN] A project with -wal frames, and the work folder set to the folder of the original
    const auto path = file("samefolder.aup4");
    writeProjectFileWithWal(path, twoClipProject(false, true), { { 1, 262144 } });
    const auto walPath = std::filesystem::path(path.string() + "-wal");
    const auto originalBytes = readFileBytes(path);
    const auto walBytes = readFileBytes(walPath);

    //! [WHEN] It is inspected
    InspectionOptions options;
    options.workDir = muse::io::path_t(m_dir.string());
    const muse::RetVal<Inspection> inspection = ProjectInspector().inspect(muse::io::path_t(path.string()), options);

    //! [THEN] The copy goes to a folder of its own, and the original and its -wal file stay as they are
    ASSERT_TRUE(inspection.ret) << inspection.ret.toString();
    EXPECT_TRUE(inspection.val.database.walApplied);
    EXPECT_EQ(readFileBytes(path), originalBytes);
    EXPECT_EQ(readFileBytes(walPath), walBytes);
}
