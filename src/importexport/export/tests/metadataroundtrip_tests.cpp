/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <QTemporaryDir>

#include <cstdint>
#include <fstream>
#include <iterator>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <sndfile.h>
#include <wx/filename.h>

#include "au3-files/wxFileNameWrapper.h"
#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-import-export/Import.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-tags/Tags.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3project.h"
#include "trackedit/tests/tracktemplatefactory.h"
#include "project/tests/testtools.h"

//! Exports a track with metadata to every format whose exporter writes tags,
//! imports the file back, and checks that the tags survive.
//! For the formats that carry an ID3v2 tag, also checks that the tag is
//! ID3v2.3: libid3tag writes v2.4 unless built from a fork that has
//! `ID3_TAG_OPTION_ID3V2_3`, and the exporters silently fall back to v2.4.

namespace au::importexport {
namespace {
constexpr double SAMPLE_RATE = 48000.0; // Opus only supports a few rates, 48 kHz among them
constexpr double DURATION = 1.0;
constexpr unsigned NUM_CHANNELS = 2;

enum class Id3Location {
    None,          // The format has its own tag format (Vorbis comments, APE tags)
    FileStart,     // MP3, MP2
    WavChunk,      // `id3 ` chunk in the RIFF file
    AiffChunk,     // `ID3 ` chunk in the FORM file
};

struct MetadataFormat {
    const char* name; // As in FormatInfo::format
    const char* extension;
    Id3Location id3Location;
};

std::ostream& operator<<(std::ostream& os, const MetadataFormat& format)
{
    return os << format.name;
}

//! Splitting the literals keeps hex escapes from swallowing the next letter.
const wxString UNICODE_TITLE = wxString::FromUTF8(
    "\xC3\x9C" "n" "\xC3\xAF" "c" "\xC3\xB8" "d" "\xC3\xA9" " " "\xE6\x97\xA5\xE6\x9C\xAC");

const std::vector<std::pair<wxString, wxString> > TAGS {
    { TAG_TITLE, UNICODE_TITLE },
    { TAG_ARTIST, wxT("Artist") },
    { TAG_ALBUM, wxT("Album") },
    { TAG_TRACK, wxT("3") },
    { TAG_YEAR, wxT("2026") },
    { TAG_GENRE, wxT("Rock") },
    { TAG_COMMENTS, wxT("A comment") },
};

class NullExportDelegate final : public ExportProcessorDelegate
{
public:
    bool IsCancelled() const override { return false; }
    bool IsStopped() const override { return false; }
    void SetStatusString(const TranslatableString&) override {}
    void OnProgress(double) override {}
};

std::vector<uint8_t> readFile(const std::string& path)
{
    std::ifstream file(path, std::ios::binary);
    return { std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>() };
}

uint32_t readUint32(const std::vector<uint8_t>& bytes, size_t pos, bool bigEndian)
{
    const uint32_t b0 = bytes[pos], b1 = bytes[pos + 1], b2 = bytes[pos + 2], b3 = bytes[pos + 3];
    return bigEndian ? (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
           : (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
}

//! The payload of the first chunk with this ID in a RIFF or FORM file.
std::optional<std::vector<uint8_t> > findChunk(const std::vector<uint8_t>& bytes, const char* id, bool bigEndian)
{
    size_t pos = 12; // Skip the RIFF/FORM header: ID, size and form type
    while (pos + 8 <= bytes.size()) {
        const uint32_t size = readUint32(bytes, pos + 4, bigEndian);
        const size_t payloadStart = pos + 8;
        if (std::equal(id, id + 4, bytes.begin() + pos)) {
            if (payloadStart + size > bytes.size()) {
                return std::nullopt;
            }
            return std::vector<uint8_t>(bytes.begin() + payloadStart, bytes.begin() + payloadStart + size);
        }
        pos = payloadStart + size + (size & 1); // Chunks are padded to an even size
    }
    return std::nullopt;
}

//! The bytes the ID3v2 tag starts with, if the file has one where the format puts it.
std::optional<std::vector<uint8_t> > id3TagStart(const std::string& path, Id3Location location)
{
    const std::vector<uint8_t> bytes = readFile(path);
    switch (location) {
    case Id3Location::FileStart:
        return bytes;
    case Id3Location::WavChunk:
        return findChunk(bytes, "id3 ", false);
    case Id3Location::AiffChunk:
        return findChunk(bytes, "ID3 ", true);
    case Id3Location::None:
        break;
    }
    return std::nullopt;
}
}

class MetadataRoundTripTests : public ::testing::TestWithParam<MetadataFormat>
{
public:
    void SetUp() override
    {
        ASSERT_TRUE(m_dir.isValid());

        m_projectAccessor = std::make_shared<au3::Au3ProjectAccessor>(muse::modularity::globalCtx());

        const std::string source
            = (muse::String::fromUtf8(exporter_tests_DATA_ROOT) + "/../../../trackedit/tests/data/empty.aup4").toStdString();
        const std::string projectPath = m_dir.filePath("metadata.aup4").toStdString();
        ASSERT_TRUE(testtools::copyFile(source, projectPath));

        constexpr auto discardAutosave = false;
        ASSERT_TRUE(m_projectAccessor->load(muse::io::path_t(projectPath), discardAutosave));

        trackedit::TrackTemplateFactory factory(project(), SAMPLE_RATE);
        factory.addTrackFromTemplate("track", {
                { 0.0, { { DURATION, trackedit::TrackTemplateFactory::createNoise } } }
            });
    }

    void TearDown() override
    {
        au3::Au3TrackList::Get(project()).Clear();
        m_projectAccessor->clearSavedState();
        m_projectAccessor->close();
        m_projectAccessor.reset();
    }

protected:
    au3::Au3Project& project() const
    {
        return *reinterpret_cast<au3::Au3Project*>(m_projectAccessor->au3ProjectPtr());
    }

    std::string exportPath(const MetadataFormat& format) const
    {
        return m_dir.filePath(QString("metadata.") + format.extension).toStdString();
    }

    void exportWithTags(const MetadataFormat& format, const Tags& tags, const std::string& path)
    {
        auto [plugin, formatIndex] = ExportPluginRegistry::Get().FindFormat(
            format.id3Location == Id3Location::AiffChunk ? wxString(wxT("WAV")) : wxString(format.name));
        ASSERT_NE(plugin, nullptr) << "no exporter for " << format.name;

        ExportProcessor::Parameters parameters;
        if (format.id3Location == Id3Location::AiffChunk) {
            //! AIFF is a named PCM format on macOS only; elsewhere it is exported
            //! through the last PCM format, "Other uncompressed files", whose
            //! header type and encoding are parameters.
            formatIndex = plugin->GetFormatCount() - 1;
            constexpr ExportOptionID headerTypeOptionId = 0; // OptionIDSFType in ExportPCM.cpp
            parameters.emplace_back(headerTypeOptionId, ExportValue { static_cast<int>(SF_FORMAT_AIFF) });
            // The encoding option's ID is the header type
            parameters.emplace_back(static_cast<ExportOptionID>(SF_FORMAT_AIFF), ExportValue { static_cast<int>(SF_FORMAT_PCM_16) });
        } else {
            const std::unique_ptr<ExportOptionsEditor> editor = plugin->CreateOptionsEditor(formatIndex, nullptr);
            ASSERT_NE(editor, nullptr);
            parameters = ExportUtils::ParametersFromEditor(*editor);
        }
        ASSERT_TRUE(plugin->GetFormatInfo(formatIndex).canMetaData);

        const std::unique_ptr<ExportProcessor> processor = plugin->CreateProcessor(formatIndex);
        ASSERT_NE(processor, nullptr);
        constexpr auto selectedOnly = false;
        ASSERT_TRUE(processor->Initialize(project(), parameters, wxFileNameWrapper { wxFileName(wxString::FromUTF8(path)) },
                                          0.0, DURATION, selectedOnly, SAMPLE_RATE, NUM_CHANNELS, nullptr, &tags));

        NullExportDelegate delegate;
        ASSERT_EQ(processor->Process(delegate), ExportResult::Success);
    }

    void importTags(const std::string& path, Tags& tags)
    {
        TrackHolders tracks;
        std::optional<LibFileFormats::AcidizerTags> acidTags;
        TranslatableString errorMessage;
        ASSERT_TRUE(Importer::Get().Import(project(), wxString::FromUTF8(path), nullptr, &WaveTrackFactory::Get(project()),
                                           tracks, &tags, acidTags, errorMessage))
            << errorMessage.Translation().ToStdString();
        EXPECT_FALSE(tracks.empty());
    }

    QTemporaryDir m_dir;
    std::shared_ptr<au3::Au3ProjectAccessor> m_projectAccessor;
};

TEST_P(MetadataRoundTripTests, TagsSurviveExportAndImport)
{
    const MetadataFormat& format = GetParam();
    const std::string path = exportPath(format);

    //! [GIVEN] Tags with a value for each standard field, one of them non-ASCII
    Tags written;
    written.Clear();
    for (const auto& [name, value] : TAGS) {
        written.SetTag(name, value);
    }

    //! [WHEN] Exporting with these tags, then importing the file
    exportWithTags(format, written, path);
    if (HasFatalFailure()) {
        return;
    }
    Tags read;
    read.Clear();
    importTags(path, read);
    if (HasFatalFailure()) {
        return;
    }

    //! [THEN] Every tag is read back with its value
    //! (importers may add tags of their own, which we ignore)
    for (const auto& [name, value] : TAGS) {
        EXPECT_EQ(read.GetTag(name).ToStdString(wxConvUTF8), value.ToStdString(wxConvUTF8)) << "tag " << name.ToStdString();
    }
}

TEST_P(MetadataRoundTripTests, WritesId3v23Tag)
{
    const MetadataFormat& format = GetParam();
    if (format.id3Location == Id3Location::None) {
        GTEST_SKIP() << format.name << " does not use ID3";
    }
    const std::string path = exportPath(format);

    //! [GIVEN] Some tags
    Tags written;
    written.Clear();
    written.SetTag(TAG_TITLE, wxT("Title"));

    //! [WHEN] Exporting with these tags
    exportWithTags(format, written, path);
    if (HasFatalFailure()) {
        return;
    }

    //! [THEN] The file has an ID3 tag where the format puts it, and its header
    //! says ID3v2.3: "ID3", major version 3
    const auto tag = id3TagStart(path, format.id3Location);
    ASSERT_TRUE(tag.has_value()) << "no ID3 chunk";
    ASSERT_GE(tag->size(), 4u);
    EXPECT_EQ(std::string(tag->begin(), tag->begin() + 3), "ID3");
    EXPECT_EQ((*tag)[3], 3) << "ID3v2." << int((*tag)[3]);
}

//! FFmpeg formats are left out: FFmpeg is loaded at runtime and may be missing.
INSTANTIATE_TEST_SUITE_P(AllFormatsWithMetadata, MetadataRoundTripTests, ::testing::Values(
                             MetadataFormat { "MP3", "mp3", Id3Location::FileStart },
                             MetadataFormat { "MP2", "mp2", Id3Location::FileStart },
                             MetadataFormat { "WAV", "wav", Id3Location::WavChunk },
                             MetadataFormat { "AIFF", "aiff", Id3Location::AiffChunk },
                             MetadataFormat { "FLAC", "flac", Id3Location::None },
                             MetadataFormat { "OGG", "ogg", Id3Location::None },
                             MetadataFormat { "Opus", "opus", Id3Location::None },
                             MetadataFormat { "WavPack", "wv", Id3Location::None }),
                         [](const ::testing::TestParamInfo<MetadataFormat>& info) {
    return std::string(info.param.name);
});
}
