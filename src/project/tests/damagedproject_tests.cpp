/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>

#include <QFile>
#include <QTemporaryDir>
#include <sqlite3.h>

#include <string>
#include <vector>

#include "au3-label-track/LabelTrack.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectSerializer.h"
#include "au3-xml/XMLFileReader.h"
#include "au3-track/Track.h"
#include "au3-xml/XMLTagHandler.h"
#include "au3wrap/internal/au3project.h"
#include "project/projecterrors.h"
#include "testing/testcontext.h"

namespace {
//! LabelTrack.cpp registers the "labeltrack" reader; without a reference the linker may drop it
[[maybe_unused]] const auto& forceLabelTrackLink = LabelTrack::ClassTypeInfo();

constexpr int RATE = 44100;
constexpr int BLOCK_SAMPLES = 4096;

//! Writes the tags it reads, so a document can hold what Audacity itself never saves
class DocumentWriter final : public XMLTagHandler
{
public:
    explicit DocumentWriter(XMLWriter& writer)
        : m_writer(writer) {}

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override
    {
        m_writer.StartTag(wxString::FromUTF8(tag.data(), tag.size()));
        for (const auto& attr : attrs) {
            m_writer.WriteAttr(wxString::FromUTF8(attr.first.data(), attr.first.size()), attr.second.ToWString());
        }
        return true;
    }

    void HandleXMLEndTag(const std::string_view& tag) override
    {
        m_writer.EndTag(wxString::FromUTF8(tag.data(), tag.size()));
    }

    XMLTagHandler* HandleXMLChild(const std::string_view&) override { return this; }

private:
    XMLWriter& m_writer;
};

//! One clip of silence; `numSamples` may disagree with its single block
wxString clipXml(double trimLeft, double trimRight, int numSamples = BLOCK_SAMPLES)
{
    return wxString::Format(
        "<waveclip offset=\"0\" trimLeft=\"%.17g\" trimRight=\"%.17g\" name=\"clip\">"
        "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">"
        "<waveblock start=\"0\" blockid=\"-%d\"/>"
        "</sequence><envelope numpoints=\"0\"/></waveclip>",
        trimLeft, trimRight, numSamples, BLOCK_SAMPLES);
}

//! One clip whose blocks are rows of the file, BLOCK_SAMPLES samples each
wxString storedClipXml(const std::vector<int>& blockIds)
{
    wxString blocks;
    for (size_t i = 0; i < blockIds.size(); ++i) {
        blocks += wxString::Format("<waveblock start=\"%d\" blockid=\"%d\"/>", int(i) * BLOCK_SAMPLES, blockIds[i]);
    }
    return wxString::Format(
        "<waveclip offset=\"0\" trimLeft=\"0\" trimRight=\"0\" name=\"clip\">"
        "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">%s</sequence>"
        "<envelope numpoints=\"0\"/></waveclip>",
        int(blockIds.size()) * BLOCK_SAMPLES, blocks);
}

wxString trackXml(const wxString& clip, int channel = 0, int linked = 0)
{
    return wxString::Format("<wavetrack name=\"track\" channel=\"%d\" linked=\"%d\" rate=\"%d\" sampleformat=\"262159\">%s</wavetrack>",
                            channel, linked, RATE, clip);
}

wxString projectXml(const wxString& tracks)
{
    return wxString::Format("<project version=\"1.3.0\" audacityversion=\"4.0.1\" rate=\"%d\">%s</project>", RATE, tracks);
}
}

namespace au::project {
class DamagedProjectTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        ASSERT_TRUE(m_directory.isValid());
        m_path = m_directory.filePath("damaged.aup4");
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", m_path));
    }

    //! Stores `xml` as the project document, the way ProjectFileIO::WriteDoc does
    void setDocument(const wxString& xml)
    {
        ProjectSerializer serializer;
        DocumentWriter writer(serializer);
        XMLFileReader reader;
        ASSERT_TRUE(reader.ParseString(&writer, xml));

        const MemoryStream& dict = serializer.GetDict();
        const MemoryStream& doc = serializer.GetData();
        sqlite3* db = nullptr;
        ASSERT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        ASSERT_EQ(sqlite3_exec(db, "DELETE FROM autosave;", nullptr, nullptr, nullptr), SQLITE_OK);
        sqlite3_stmt* statement = nullptr;
        ASSERT_EQ(sqlite3_prepare_v2(db, "INSERT INTO project(id, dict, doc) VALUES(1, ?1, ?2)"
                                         " ON CONFLICT(id) DO UPDATE SET dict = ?1, doc = ?2;", -1, &statement, nullptr), SQLITE_OK);
        sqlite3_bind_blob(statement, 1, dict.GetData(), int(dict.GetSize()), SQLITE_TRANSIENT);
        sqlite3_bind_blob(statement, 2, doc.GetData(), int(doc.GetSize()), SQLITE_TRANSIENT);
        EXPECT_EQ(sqlite3_step(statement), SQLITE_DONE);
        sqlite3_finalize(statement);
        sqlite3_close(db);
    }

    //! Stores bytes the document decoder cannot read: a start tag whose name is not in the dictionary
    void setUnreadableDocument()
    {
        sqlite3* db = nullptr;
        ASSERT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        EXPECT_EQ(sqlite3_exec(db, "DELETE FROM autosave;"
                                   " INSERT INTO project(id, dict, doc) VALUES(1, x'', x'013412')"
                                   " ON CONFLICT(id) DO UPDATE SET dict = x'', doc = x'013412';", nullptr, nullptr, nullptr), SQLITE_OK);
        sqlite3_close(db);
    }

    //! A row of silence with this id, as the sequence of a stored clip reads it
    void insertBlock(int blockId)
    {
        sqlite3* db = nullptr;
        ASSERT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        const std::string sql = "INSERT INTO sampleblocks(blockid, sampleformat, summin, summax, sumrms, summary256, summary64k, samples)"
                                " VALUES(" + std::to_string(blockId) + ", 262159, 0, 0, 0, zeroblob(12), zeroblob(12), zeroblob("
                                + std::to_string(BLOCK_SAMPLES * 4) + "));";
        EXPECT_EQ(sqlite3_exec(db, sql.c_str(), nullptr, nullptr, nullptr), SQLITE_OK);
        sqlite3_close(db);
    }

    int64_t blockRows() const
    {
        int64_t count = -1;
        sqlite3* db = nullptr;
        EXPECT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        sqlite3_stmt* statement = nullptr;
        EXPECT_EQ(sqlite3_prepare_v2(db, "SELECT count(*) FROM sampleblocks;", -1, &statement, nullptr), SQLITE_OK);
        if (sqlite3_step(statement) == SQLITE_ROW) {
            count = sqlite3_column_int64(statement, 0);
        }
        sqlite3_finalize(statement);
        sqlite3_close(db);
        return count;
    }

    std::vector<uint8_t> document() const
    {
        std::vector<uint8_t> out;
        sqlite3* db = nullptr;
        EXPECT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        sqlite3_stmt* statement = nullptr;
        EXPECT_EQ(sqlite3_prepare_v2(db, "SELECT doc FROM project WHERE id = 1;", -1, &statement, nullptr), SQLITE_OK);
        if (sqlite3_step(statement) == SQLITE_ROW) {
            const auto* bytes = static_cast<const uint8_t*>(sqlite3_column_blob(statement, 0));
            out.assign(bytes, bytes + sqlite3_column_bytes(statement, 0));
        }
        sqlite3_finalize(statement);
        sqlite3_close(db);
        return out;
    }

    muse::Ret load()
    {
        au3::Au3ProjectAccessor project(testutils::makeTestContext());
        const muse::Ret ret = project.load(m_path.toStdString(), false);
        if (ret) {
            project.close();
        }
        return ret;
    }

    void expectRefusedAndUnchanged()
    {
        const auto before = document();
        ASSERT_FALSE(before.empty());
        EXPECT_EQ(load().code(), int(Err::CorruptionUponOpeningError));
        EXPECT_EQ(document(), before);
    }

    QTemporaryDir m_directory;
    QString m_path;
};

TEST_F(DamagedProjectTests, ClipTrimmedByMoreThanItsLengthIsRefused)
{
    //! [GIVEN] Trims of 2 x 2205 samples on a clip of 4096 samples, as a clip saved with a wrong rate has
    setDocument(projectXml(trackXml(clipXml(0.05, 0.05))));

    //! [THEN] The load is refused and the document stays as it was
    expectRefusedAndUnchanged();
}

TEST_F(DamagedProjectTests, ClipOfOneSampleStillOpens)
{
    //! [GIVEN] A clip trimmed to one sample, which the load removes as empty
    setDocument(projectXml(trackXml(clipXml(double(BLOCK_SAMPLES - 1) / RATE, 0))));

    //! [THEN] The project opens
    EXPECT_TRUE(load());
}

TEST_F(DamagedProjectTests, MissingRightChannelIsRefused)
{
    //! [GIVEN] A left channel whose right channel is not in the document
    setDocument(projectXml(trackXml(clipXml(0, 0), 0, 3)));

    expectRefusedAndUnchanged();
}

TEST_F(DamagedProjectTests, LeftChannelBeforeALabelTrackIsRefused)
{
    //! [GIVEN] A left channel followed by a track that is no wave track
    setDocument(projectXml(trackXml(clipXml(0, 0), 0, 3) + "<labeltrack name=\"labels\" numlabels=\"0\"/>"));

    //! [THEN] The load is refused instead of reading the name of a right channel that is not there
    expectRefusedAndUnchanged();
}

TEST_F(DamagedProjectTests, SequenceLongerThanItsBlocksIsRefused)
{
    //! [GIVEN] A sequence that claims more samples than its blocks hold
    setDocument(projectXml(trackXml(clipXml(0, 0, BLOCK_SAMPLES + 1000))));

    expectRefusedAndUnchanged();
}

TEST_F(DamagedProjectTests, MissingBlockAfterLoadedAudioLeavesNoTracks)
{
    //! [GIVEN] Blocks 1 and 2 are in the file and block 999 is not, so the parse stops inside the second sequence
    insertBlock(1);
    insertBlock(2);
    setDocument(projectXml(trackXml(storedClipXml({ 1 })) + trackXml(storedClipXml({ 2, 999 }))));

    //! [WHEN] The project is opened
    au3::Au3ProjectAccessor project(testutils::makeTestContext());
    const muse::Ret ret = project.load(m_path.toStdString(), false);

    //! [THEN] The load fails and drops the tracks it had built, with their blocks locked
    EXPECT_EQ(ret.code(), int(Err::AudacityExceptionError));
    EXPECT_TRUE(TrackList::Get(*reinterpret_cast<AudacityProject*>(project.au3ProjectPtr())).empty());

    //! [THEN] Both rows stay in the file
    EXPECT_EQ(blockRows(), 2);
}

TEST_F(DamagedProjectTests, MissingBlockAfterStereoPairKeepsRightChannelRows)
{
    //! [GIVEN] A stereo pair on blocks 1 and 2, then a track whose block 999 is not in the file
    insertBlock(1);
    insertBlock(2);
    setDocument(projectXml(trackXml(storedClipXml({ 1 }), 0, 3) + trackXml(storedClipXml({ 2 }), 1, 0)
                           + trackXml(storedClipXml({ 999 }))));

    //! [THEN] The load fails before the pair is joined, and the rows of both channels stay in the file
    EXPECT_EQ(load().code(), int(Err::AudacityExceptionError));
    EXPECT_EQ(blockRows(), 2);
}

TEST_F(DamagedProjectTests, UnreadableDocumentIsALoadError)
{
    //! [GIVEN] A document the decoder rejects, which sets no SQLite error code
    setUnreadableDocument();

    //! [THEN] The load fails with a project error instead of the code 0 that means success
    EXPECT_EQ(load().code(), int(Err::ProjectLoadError));
}
}
