/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <QFile>
#include <QTemporaryDir>

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
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
#include "au3-import-export/ImportProgressListener.h"
#include "au3-mixer/MixerOptions.h"
#include "au3-tags/Tags.h"
#include "au3-wave-track/WaveTrack.h"
#include "mod-pcm/ExportPCM.h"
#include "mod-ffmpeg/FFmpegDefines.h"
#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/tests/tracktemplatefactory.h"
#include "importexport/export/OriginalFileInfo.h"
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
    //! For FFmpeg's custom export, the container to use
    const char* ffmpegContainer = nullptr;
    bool needsFFmpeg = false;
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

class SourceFormatListener final : public ImportProgressListener
{
public:
    bool OnImportFileOpened(ImportFileHandle& file) override
    {
        m_handle = &file;
        format = {};
        return true;
    }

    void OnImportProgress(double) override {}
    void OnImportResult(ImportResult result) override
    {
        if (result == ImportResult::Success) {
            format = m_handle->GetSourceFormat();
        }
    }

    ImportSourceFormat format;

private:
    ImportFileHandle* m_handle = nullptr;
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

class ExportTestProject : public ::testing::Test
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
        m_projectLoaded = true;

        au3::TrackTemplateFactory factory(project(), SAMPLE_RATE);
        factory.addTrackFromTemplate("track", {
                { 0.0, { { DURATION, au3::TrackTemplateFactory::createNoise } } }
            });
    }

    void TearDown() override
    {
        if (!m_projectLoaded) {
            return;
        }
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

    bool decodeFile(const std::string& path, SourceFormatListener& listener, TrackHolders& tracks)
    {
        Tags tags;
        std::optional<LibFileFormats::AcidizerTags> acidTags;
        TranslatableString error;
        return Importer::Get().Import(project(), wxString::FromUTF8(path), &listener, &WaveTrackFactory::Get(project()),
                                      tracks, &tags, acidTags, error);
    }

    QTemporaryDir m_dir;
    std::shared_ptr<au3::Au3ProjectAccessor> m_projectAccessor;
    bool m_projectLoaded = false;
};

TEST_F(ExportTestProject, DetectsAndPreservesTheEncodingAndChannelsOfAMislabeledWav)
{
    constexpr int channels = 6;
    constexpr int frames = 256;
    constexpr int sampleRate = 48000;
    constexpr int encoding = SF_FORMAT_WAVEX | SF_FORMAT_PCM_24;
    const auto sourcePath = m_dir.filePath("surround.flac").toStdString();
    SF_INFO source {};
    source.channels = channels;
    source.samplerate = sampleRate;
    source.format = encoding;
    auto* sndFile = sf_open(sourcePath.c_str(), SFM_WRITE, &source);
    ASSERT_NE(sndFile, nullptr);
    std::array<int, channels> positions {
        SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER,
        SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_SIDE_LEFT, SF_CHANNEL_MAP_SIDE_RIGHT
    };
    const auto layoutStored = sf_command(sndFile, SFC_SET_CHANNEL_MAP_INFO, positions.data(), sizeof(positions));
    std::array<float, frames* channels> samples;
    for (size_t i = 0; i < samples.size(); ++i) {
        samples[i] = (1 + i % channels) / 16.0f + (1 + (i / channels) % 127) / 65536.0f;
    }
    const auto written = sf_writef_float(sndFile, samples.data(), frames);
    ASSERT_EQ(sf_close(sndFile), 0);
    ASSERT_EQ(layoutStored, SF_TRUE);
    ASSERT_EQ(written, frames);

    SourceFormatListener listener;
    auto& projectTracks = TrackList::Get(project());
    projectTracks.Clear();
    TrackHolders tracks;
    ASSERT_TRUE(decodeFile(sourcePath, listener, tracks));
    EXPECT_EQ(listener.format.exportFormat, wxT("WAV"));
    EXPECT_EQ(listener.format.pcmFormat, encoding);
    ASSERT_EQ(listener.format.channelMap.size(), channels);
    EXPECT_TRUE(std::equal(positions.begin(), positions.end(), listener.format.channelMap.begin()));
    ASSERT_EQ(tracks.size(), channels);
    std::vector<int64_t> sourceTrackIds;
    for (auto& track : tracks) {
        projectTracks.Add(track);
        sourceTrackIds.push_back(track->GetId().raw());
    }
    OriginalFileInfo original;
    original.SetSourceChannelTracks(std::move(sourceTrackIds));
    const auto path = QString::fromStdString(sourcePath);
    original.SetOriginalFile(path, "surround.flac", OriginalFileInfo::ReadFileVersion(path));
    original.SetExportFormatID("WAV");
    original.IncrementImportedFileCount();
    EXPECT_TRUE(original.CanOverwriteOriginalFile(projectTracks));
    for (const auto& track : tracks) {
        static_cast<WaveTrack&>(*track).SetMute(true);
    }
    EXPECT_FALSE(original.CanOverwriteOriginalFile(projectTracks));
    for (const auto& track : tracks) {
        static_cast<WaveTrack&>(*track).SetMute(false);
    }
    ASSERT_TRUE(projectTracks.MoveUp(*tracks.back()));
    const auto channelMapping = original.MakeChannelMapping(projectTracks);
    ASSERT_NE(channelMapping, nullptr);

    const auto [plugin, index] = ExportPluginRegistry::Get().FindFormat(listener.format.exportFormat);
    ASSERT_NE(plugin, nullptr);
    const auto outputPath = m_dir.filePath("staged.flac").toStdString();
    auto processor = plugin->CreateProcessor(index);
    const ExportProcessor::Parameters parameters {
        { PcmExport::SourceFormat, listener.format.pcmFormat },
        { PcmExport::SourceChannelMap, std::string(listener.format.channelMap.begin(), listener.format.channelMap.end()) }
    };
    ASSERT_TRUE(processor->Initialize(project(), parameters, wxFileNameWrapper { wxFileName(wxString::FromUTF8(outputPath)) }, 0.0,
                                      static_cast<double>(frames) / sampleRate, false, sampleRate, channels, channelMapping.get(),
                                      nullptr));
    NullExportDelegate delegate;
    ASSERT_EQ(processor->Process(delegate), ExportResult::Success);
    processor.reset();

    SF_INFO output {};
    sndFile = sf_open(outputPath.c_str(), SFM_READ, &output);
    ASSERT_NE(sndFile, nullptr);
    std::array<int, channels> outputPositions {};
    const auto layoutRead = sf_command(sndFile, SFC_GET_CHANNEL_MAP_INFO, outputPositions.data(), sizeof(outputPositions));
    std::array<float, frames* channels> outputSamples {};
    const auto read = sf_readf_float(sndFile, outputSamples.data(), frames);
    ASSERT_EQ(sf_close(sndFile), 0);
    EXPECT_EQ(output.format, encoding);
    EXPECT_EQ(output.channels, channels);
    EXPECT_EQ(output.frames, frames);
    EXPECT_EQ(layoutRead, SF_TRUE);
    EXPECT_EQ(outputPositions, positions);
    ASSERT_EQ(read, frames);
    for (size_t i = 0; i < samples.size(); ++i) {
        EXPECT_NEAR(outputSamples[i], samples[i], 0.000002) << "sample " << i;
    }
}

TEST_F(ExportTestProject, DeclinesOverwriteForPrecisionTheMixerCannotPreserve)
{
    const auto sourcePath = m_dir.filePath("double.wav").toStdString();
    SF_INFO source {};
    source.channels = 1;
    source.samplerate = 48000;
    source.format = SF_FORMAT_WAV | SF_FORMAT_DOUBLE;
    auto* sndFile = sf_open(sourcePath.c_str(), SFM_WRITE, &source);
    ASSERT_NE(sndFile, nullptr);
    constexpr double sample = 0.2500000000001;
    const auto written = sf_writef_double(sndFile, &sample, 1);
    ASSERT_EQ(sf_close(sndFile), 0);
    ASSERT_EQ(written, 1);

    SourceFormatListener listener;
    TrackHolders tracks;
    ASSERT_TRUE(decodeFile(sourcePath, listener, tracks));
    EXPECT_FALSE(tracks.empty());
    EXPECT_TRUE(listener.format.exportFormat.empty());
}

TEST_F(ExportTestProject, DeclinesOverwriteForBroadcastWave)
{
    const auto path = m_dir.filePath("timestamp.wav").toStdString();
    SF_INFO source {};
    source.channels = 1;
    source.samplerate = 48000;
    source.format = SF_FORMAT_WAV | SF_FORMAT_PCM_24;
    auto* file = sf_open(path.c_str(), SFM_WRITE, &source);
    ASSERT_NE(file, nullptr);
    SF_BROADCAST_INFO broadcast {};
    broadcast.time_reference_low = 48000;
    const auto metadataWritten = sf_command(file, SFC_SET_BROADCAST_INFO, &broadcast, sizeof(broadcast));
    constexpr float sample = 0.25f;
    const auto written = sf_writef_float(file, &sample, 1);
    ASSERT_EQ(sf_close(file), 0);
    ASSERT_EQ(metadataWritten, SF_TRUE);
    ASSERT_EQ(written, 1);

    SourceFormatListener listener;
    TrackHolders tracks;
    ASSERT_TRUE(decodeFile(path, listener, tracks));
    EXPECT_FALSE(tracks.empty());
    EXPECT_TRUE(listener.format.exportFormat.empty());
}

TEST_F(ExportTestProject, DeclinesOverwriteForWavexWithoutSpeakerAssignments)
{
    const auto path = m_dir.filePath("unassigned.wav");
    SF_INFO source {};
    source.channels = 2;
    source.samplerate = 48000;
    source.format = SF_FORMAT_WAVEX | SF_FORMAT_PCM_24;
    auto* file = sf_open(path.toStdString().c_str(), SFM_WRITE, &source);
    ASSERT_NE(file, nullptr);
    constexpr std::array<float, 2> stereoFrame { 0.25f, 0.5f };
    const auto written = sf_writef_float(file, stereoFrame.data(), 1);
    ASSERT_EQ(sf_close(file), 0);
    ASSERT_EQ(written, 1);

    // libsndfile writes a default speaker mask. Clear dwChannelMask in WAVEFORMATEXTENSIBLE.
    QFile wav(path);
    ASSERT_TRUE(wav.open(QIODevice::ReadWrite));
    const auto formatChunk = wav.readAll().indexOf("fmt ", 12);
    ASSERT_GE(formatChunk, 12);
    ASSERT_TRUE(wav.seek(formatChunk + 8 + 20));
    ASSERT_EQ(wav.write(QByteArray(4, '\0')), 4);
    wav.close();

    SourceFormatListener listener;
    TrackHolders tracks;
    ASSERT_TRUE(decodeFile(path.toStdString(), listener, tracks));
    EXPECT_FALSE(tracks.empty());
    EXPECT_TRUE(listener.format.exportFormat.empty());
}

TEST_F(ExportTestProject, OffersOverwriteOnlyAfterACleanFlacDecode)
{
    const auto path = m_dir.filePath("source.flac");
    SF_INFO source {};
    source.channels = 1;
    source.samplerate = 48000;
    source.format = SF_FORMAT_FLAC | SF_FORMAT_PCM_16;
    auto* file = sf_open(path.toStdString().c_str(), SFM_WRITE, &source);
    ASSERT_NE(file, nullptr);
    const std::vector<short> samples(8192, 4096);
    const auto written = sf_writef_short(file, samples.data(), samples.size());
    ASSERT_EQ(sf_close(file), 0);
    ASSERT_EQ(written, samples.size());

    SourceFormatListener listener;
    TrackHolders tracks;
    ASSERT_TRUE(decodeFile(path.toStdString(), listener, tracks));
    EXPECT_EQ(listener.format.exportFormat, wxT("FLAC"));
    EXPECT_EQ(listener.format.bitDepth, 16);

    QFile flac(path);
    ASSERT_TRUE(flac.open(QIODevice::ReadWrite));
    const auto lastByte = flac.size() - 1;
    ASSERT_TRUE(flac.seek(lastByte));
    char crc = 0;
    ASSERT_EQ(flac.read(&crc, 1), 1);
    crc ^= 1;
    ASSERT_TRUE(flac.seek(lastByte));
    ASSERT_EQ(flac.write(&crc, 1), 1);
    flac.close();

    tracks.clear();
    SourceFormatListener damaged;
    ASSERT_TRUE(decodeFile(path.toStdString(), damaged, tracks));
    EXPECT_FALSE(tracks.empty());
    EXPECT_TRUE(damaged.format.exportFormat.empty());
}

class MetadataRoundTripTests : public ExportTestProject, public ::testing::WithParamInterface<MetadataFormat>
{
protected:
    std::string exportPath(const MetadataFormat& format) const
    {
        return m_dir.filePath(QString("metadata.") + format.extension).toStdString();
    }

    //! FFmpeg is loaded at runtime. CI installs it, so there a missing FFmpeg fails the test;
    //! elsewhere the test is skipped.
    void requireFFmpeg(const MetadataFormat& format)
    {
        if (!format.needsFFmpeg) {
            return;
        }
        const std::string missing = missingFFmpegSupport(format);
        if (missing.empty()) {
            return;
        }
        if (std::getenv("CI")) {
            FAIL() << missing << ", but CI is expected to install FFmpeg";
        }
        GTEST_SKIP() << missing;
    }

    static std::string missingFFmpegSupport(const MetadataFormat& format)
    {
        const std::shared_ptr<FFmpegFunctions> ffmpeg = FFmpegFunctions::Load();
        if (!ffmpeg) {
            return "FFmpeg is not available";
        }
        if (format.ffmpegContainer && !ffmpeg->GuessOutputFormat(format.ffmpegContainer, nullptr, nullptr)) {
            return std::string("FFmpeg has no ") + format.ffmpegContainer + " muxer";
        }
        if (!std::get<0>(ExportPluginRegistry::Get().FindFormat(wxString(format.name)))) {
            return std::string("FFmpeg cannot export ") + format.name;
        }
        return {};
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
        if (format.ffmpegContainer) {
            setParameter(parameters, FEFormatID, std::string(format.ffmpegContainer));
            //! PCM, because FFmpeg 4.4's Matroska muxer rejects the final FLAC packet the exporter writes
            setParameter(parameters, FECodecID, std::string("pcm_s16le"));
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

    static void setParameter(ExportProcessor::Parameters& parameters, int id, const ExportValue& value)
    {
        const auto it = std::find_if(parameters.begin(), parameters.end(), [id](const auto& parameter) {
            return std::get<0>(parameter) == id;
        });
        if (it != parameters.end()) {
            std::get<1>(*it) = value;
        } else {
            parameters.emplace_back(id, value);
        }
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
};

TEST_P(MetadataRoundTripTests, TagsSurviveExportAndImport)
{
    const MetadataFormat& format = GetParam();
    requireFFmpeg(format);
    if (IsSkipped() || HasFatalFailure()) {
        return;
    }
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

//! The FFmpeg formats need FFmpeg installed; see requireFFmpeg.
INSTANTIATE_TEST_SUITE_P(AllFormatsWithMetadata, MetadataRoundTripTests, ::testing::Values(
                             MetadataFormat { "MP3", "mp3", Id3Location::FileStart },
                             MetadataFormat { "MP2", "mp2", Id3Location::FileStart },
                             MetadataFormat { "WAV", "wav", Id3Location::WavChunk },
                             MetadataFormat { "AIFF", "aiff", Id3Location::AiffChunk },
                             MetadataFormat { "FLAC", "flac", Id3Location::None },
                             MetadataFormat { "OGG", "ogg", Id3Location::None },
                             MetadataFormat { "Opus", "opus", Id3Location::None },
                             MetadataFormat { "WavPack", "wv", Id3Location::None },
                             MetadataFormat { "M4A", "m4a", Id3Location::None, nullptr, true },
                             MetadataFormat { "WMA", "wma", Id3Location::None, nullptr, true },
                             MetadataFormat { "FFMPEG", "mka", Id3Location::None, "matroska", true }),
                         [](const ::testing::TestParamInfo<MetadataFormat>& info) {
    return std::string(info.param.name);
});
}
