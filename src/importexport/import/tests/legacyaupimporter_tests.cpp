/*
 * Audacity: A Digital Audio Editor
 */

#include "importexport/import/internal/legacyaupimporter.h"

#include <QDir>
#include <QFile>
#include <QTemporaryDir>
#include <QString>

#include <sndfile.h>

#include <gtest/gtest.h>

#include <algorithm>
#include <string>
#include <vector>

using au::importexport::LegacyAupImporter;

namespace {
struct AudioData
{
    int sampleRate = 0;
    int channels = 0;
    std::vector<float> samples;
};

QString dataDirPath(const QTemporaryDir& dir, const QString& projectName)
{
    return QDir(dir.path()).filePath(projectName + "_data");
}

QString projectPath(const QTemporaryDir& dir, const QString& projectName)
{
    return QDir(dir.path()).filePath(projectName + ".aup");
}

void makeDataSubdir(const QString& dataDir)
{
    ASSERT_TRUE(QDir().mkpath(QDir(dataDir).filePath("e00/d00")));
}

QString blockPath(const QString& dataDir, const QString& fileName)
{
    return QDir(dataDir).filePath("e00/d00/" + fileName);
}

void writeTextFile(const QString& path, const QString& text)
{
    QFile file(path);
    ASSERT_TRUE(file.open(QIODevice::WriteOnly | QIODevice::Text));
    ASSERT_EQ(file.write(text.toUtf8()), text.toUtf8().size());
}

void writeAudioFile(const QString& path, int sampleRate, int channels, int format, const std::vector<float>& samples)
{
    ASSERT_EQ(samples.size() % static_cast<size_t>(channels), 0u);

    QFile file(path);
    ASSERT_TRUE(file.open(QIODevice::WriteOnly));

    SF_INFO info {};
    info.samplerate = sampleRate;
    info.channels = channels;
    info.format = format;

    SNDFILE* sndFile = sf_open_fd(static_cast<int>(file.handle()), SFM_WRITE, &info, SF_FALSE);
    ASSERT_NE(sndFile, nullptr);
    const sf_count_t frames = static_cast<sf_count_t>(samples.size() / static_cast<size_t>(channels));
    EXPECT_EQ(sf_writef_float(sndFile, samples.data(), frames), frames);
    EXPECT_EQ(sf_close(sndFile), 0);
}

AudioData readAudioFile(const muse::io::path_t& path)
{
    AudioData data;
    QFile file(path.toQString());
    if (!file.open(QIODevice::ReadOnly)) {
        ADD_FAILURE() << "Unable to open audio file: " << path.toStdString();
        return data;
    }

    SF_INFO info {};
    SNDFILE* sndFile = sf_open_fd(static_cast<int>(file.handle()), SFM_READ, &info, SF_FALSE);
    if (!sndFile) {
        ADD_FAILURE() << "Unable to open audio data with libsndfile: " << path.toStdString();
        return data;
    }

    data.sampleRate = info.samplerate;
    data.channels = info.channels;
    data.samples.resize(static_cast<size_t>(info.frames) * static_cast<size_t>(std::max(1, info.channels)));
    EXPECT_EQ(sf_readf_float(sndFile, data.samples.data(), info.frames), info.frames);
    EXPECT_EQ(sf_close(sndFile), 0);
    return data;
}

void expectSamplesNear(const std::vector<float>& actual, const std::vector<float>& expected)
{
    ASSERT_EQ(actual.size(), expected.size());
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_NEAR(actual[i], expected[i], 1.0e-6f) << "sample " << i;
    }
}

QString projectXml(const QString& dataName, const QString& tracksXml, const QString& rate = "44100")
{
    return QString(
        "<?xml version=\"1.0\" standalone=\"no\"?>\n"
        "<project xmlns=\"http://audacity.sourceforge.net/xml/\" projname=\"%1\" version=\"1.3.0\" audacityversion=\"2.4.2\" rate=\"%2\">\n"
        "%3"
        "</project>\n").arg(dataName, rate, tracksXml);
}

QString monoTrackXml(const QString& clipXml, const QString& name = "Audio Track", const QString& rate = "44100",
                     const QString& channel = "2", const QString& linked = "0", const QString& offset = "0")
{
    return QString(
        "  <wavetrack name=\"%1\" channel=\"%2\" linked=\"%3\" rate=\"%4\" offset=\"%5\">\n"
        "%6"
        "  </wavetrack>\n").arg(name, channel, linked, rate, offset, clipXml);
}

QString waveClipXml(const QString& blocksXml, const QString& offset)
{
    return QString(
        "    <waveclip offset=\"%1\">\n"
        "      <sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"1\">\n"
        "%2"
        "      </sequence>\n"
        "    </waveclip>\n").arg(offset, blocksXml);
}

QString simpleBlockXml(const QString& fileName, int len, int start)
{
    return QString(
        "        <waveblock start=\"%1\">\n"
        "          <simpleblockfile filename=\"%2\" len=\"%3\"/>\n"
        "        </waveblock>\n").arg(start).arg(fileName).arg(len);
}

QString silentBlockXml(int len, int start)
{
    return QString(
        "        <waveblock start=\"%1\">\n"
        "          <silentblockfile len=\"%2\"/>\n"
        "        </waveblock>\n").arg(start).arg(len);
}

QString aliasBlockXml(const QString& fileName, int start, int aliasStart, int aliasLen, int channel)
{
    return QString(
        "        <waveblock start=\"%1\">\n"
        "          <pcmaliasblockfile aliasfile=\"%2\" aliasstart=\"%3\" aliaslen=\"%4\" aliaschannel=\"%5\"/>\n"
        "        </waveblock>\n").arg(start).arg(fileName).arg(aliasStart).arg(aliasLen).arg(channel);
}

QString labelTrackXml(const QString& labelsXml, const QString& name = "Label Track", int labelCount = 1)
{
    return QString(
        "  <labeltrack name=\"%1\" numlabels=\"%2\">\n"
        "%3"
        "  </labeltrack>\n").arg(name).arg(labelCount).arg(labelsXml);
}

QString labelXml(const QString& title, const QString& start, const QString& end = QString())
{
    if (end.isEmpty()) {
        return QString("    <label t=\"%1\" title=\"%2\"/>\n").arg(start, title);
    }

    return QString("    <label t=\"%1\" t1=\"%2\" title=\"%3\"/>\n").arg(start, end, title);
}

LegacyAupImporter::Result resolve(LegacyAupImporter& importer, const QString& path)
{
    return importer.resolve(muse::io::path_t(path));
}
}

TEST(LegacyAupImporter, importsMonoClipOffsetAndDecimalSampleRate)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "mono");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "e0000001.au"), 48000, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.25f, -0.5f, 0.75f });
    writeTextFile(projectPath(dir, "mono"), projectXml(
                      "mono_data",
                      monoTrackXml(waveClipXml(simpleBlockXml("e0000001.au", 3, 0), "1.25"), "Voice", "48000.0"),
                      "48000.0"));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "mono"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 1u);
    EXPECT_EQ(result.tracks[0].title, "Voice");
    EXPECT_EQ(result.tracks[0].channels, 1);
    ASSERT_EQ(result.tracks[0].clips.size(), 1u);
    EXPECT_DOUBLE_EQ(result.tracks[0].clips[0].startTime, 1.25);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.sampleRate, 48000);
    EXPECT_EQ(rendered.channels, 1);
    expectSamplesNear(rendered.samples, { 0.25f, -0.5f, 0.75f });
}

TEST(LegacyAupImporter, insertsSilenceForMissingSimpleBlockFiles)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "missing");
    makeDataSubdir(dataDir);

    writeTextFile(projectPath(dir, "missing"), projectXml(
                      "missing_data",
                      monoTrackXml(waveClipXml(simpleBlockXml("missing.au", 4, 0), "0"))));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "missing"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.warnings.size(), 1u);
    EXPECT_NE(result.warnings[0].find("Missing legacy block file"), std::string::npos);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.sampleRate, 44100);
    EXPECT_EQ(rendered.channels, 1);
    expectSamplesNear(rendered.samples, { 0.0f, 0.0f, 0.0f, 0.0f });
}

TEST(LegacyAupImporter, honorsWaveBlockStartsAndSilentBlocks)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "gapped");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "e0000001.au"), 32000, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.1f, 0.2f });
    writeTextFile(projectPath(dir, "gapped"), projectXml(
                      "gapped_data",
                      monoTrackXml(waveClipXml(simpleBlockXml("e0000001.au", 2, 0) + silentBlockXml(2, 4), "0"), "Gapped", "32000"),
                      "32000"));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "gapped"));

    ASSERT_TRUE(result.success) << result.error;
    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.sampleRate, 32000);
    expectSamplesNear(rendered.samples, { 0.1f, 0.2f, 0.0f, 0.0f, 0.0f, 0.0f });
}

TEST(LegacyAupImporter, resolvesRelativePcmAliasFilesAndChannelSelection)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "alias");
    makeDataSubdir(dataDir);

    writeAudioFile(QDir(dir.path()).filePath("source.wav"), 44100, 2, SF_FORMAT_WAV | SF_FORMAT_FLOAT,
                   { 0.0f, 0.5f, 0.1f, 0.6f, 0.2f, 0.7f, 0.3f, 0.8f });
    writeTextFile(projectPath(dir, "alias"), projectXml(
                      "alias_data",
                      monoTrackXml(waveClipXml(aliasBlockXml("source.wav", 0, 1, 2, 1), "2.5"))));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "alias"));

    ASSERT_TRUE(result.success) << result.error;
    EXPECT_DOUBLE_EQ(result.tracks[0].clips[0].startTime, 2.5);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.channels, 1);
    expectSamplesNear(rendered.samples, { 0.6f, 0.7f });
}

TEST(LegacyAupImporter, importsMatchedLinkedLeftRightTracksAsStereo)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "stereo");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "left.au"), 22050, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.1f, 0.2f, 0.3f });
    writeAudioFile(blockPath(dataDir, "right.au"), 22050, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { -0.1f, -0.2f, -0.3f });

    const QString left = monoTrackXml(waveClipXml(simpleBlockXml("left.au", 3, 0), "0.75"), "Stereo", "22050", "0", "1");
    const QString right = monoTrackXml(waveClipXml(simpleBlockXml("right.au", 3, 0), "0.75"), "Stereo", "22050", "1", "0");
    writeTextFile(projectPath(dir, "stereo"), projectXml("stereo_data", left + right, "22050"));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "stereo"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 1u);
    EXPECT_EQ(result.tracks[0].channels, 2);
    ASSERT_EQ(result.tracks[0].clips.size(), 1u);
    EXPECT_DOUBLE_EQ(result.tracks[0].clips[0].startTime, 0.75);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.sampleRate, 22050);
    EXPECT_EQ(rendered.channels, 2);
    expectSamplesNear(rendered.samples, { 0.1f, -0.1f, 0.2f, -0.2f, 0.3f, -0.3f });
}

TEST(LegacyAupImporter, keepsMismatchedLinkedTracksSeparate)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "mismatch");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "left.au"), 44100, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.1f, 0.2f });
    writeAudioFile(blockPath(dataDir, "right.au"), 48000, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { -0.1f, -0.2f });

    const QString left = monoTrackXml(waveClipXml(simpleBlockXml("left.au", 2, 0), "0"), "Left", "44100", "0", "1");
    const QString right = monoTrackXml(waveClipXml(simpleBlockXml("right.au", 2, 0), "0"), "Right", "48000", "1", "0");
    writeTextFile(projectPath(dir, "mismatch"), projectXml("mismatch_data", left + right, "44100"));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "mismatch"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 2u);
    EXPECT_EQ(result.tracks[0].channels, 1);
    EXPECT_EQ(result.tracks[1].channels, 1);
    EXPECT_EQ(readAudioFile(result.tracks[0].clips[0].filePath).sampleRate, 44100);
    EXPECT_EQ(readAudioFile(result.tracks[1].clips[0].filePath).sampleRate, 48000);
}

TEST(LegacyAupImporter, supportsPreWaveClipTrackLevelSequences)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "legacy");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "old.au"), 11025, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.4f, 0.5f });
    const QString track = monoTrackXml(
        QString("    <sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"2\">\n")
        + simpleBlockXml("old.au", 2, 0)
        + "    </sequence>\n",
        "Old", "11025", "2", "0", "3.0");
    writeTextFile(projectPath(dir, "legacy"), projectXml("legacy_data", track, "11025"));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "legacy"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 1u);
    ASSERT_EQ(result.tracks[0].clips.size(), 1u);
    EXPECT_DOUBLE_EQ(result.tracks[0].clips[0].startTime, 3.0);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    EXPECT_EQ(rendered.sampleRate, 11025);
    expectSamplesNear(rendered.samples, { 0.4f, 0.5f });
}

TEST(LegacyAupImporter, ignoresNestedWaveClipCutLines)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "cutline");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "main.au"), 44100, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.9f });
    writeAudioFile(blockPath(dataDir, "cut.au"), 44100, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { -0.9f });

    const QString clip = QString(
        "    <waveclip offset=\"0\">\n"
        "      <sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"1\">\n"
        "%1"
        "      </sequence>\n"
        "      <waveclip offset=\"0.25\">\n"
        "        <sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"1\">\n"
        "%2"
        "        </sequence>\n"
        "      </waveclip>\n"
        "    </waveclip>\n").arg(simpleBlockXml("main.au", 1, 0), simpleBlockXml("cut.au", 1, 0));
    writeTextFile(projectPath(dir, "cutline"), projectXml("cutline_data", monoTrackXml(clip)));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "cutline"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 1u);
    ASSERT_EQ(result.tracks[0].clips.size(), 1u);

    const AudioData rendered = readAudioFile(result.tracks[0].clips[0].filePath);
    expectSamplesNear(rendered.samples, { 0.9f });
}

TEST(LegacyAupImporter, failsWhenProjectDataDirectoryIsMissing)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());

    writeTextFile(projectPath(dir, "nodata"), projectXml(
                      "nodata_data",
                      monoTrackXml(waveClipXml(simpleBlockXml("missing.au", 1, 0), "0"))));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "nodata"));

    EXPECT_FALSE(result.success);
    EXPECT_NE(result.error.find("_data directory"), std::string::npos);
    EXPECT_TRUE(result.tracks.empty());
}

TEST(LegacyAupImporter, importsLabelOnlyProjects)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    makeDataSubdir(dataDirPath(dir, "labels"));

    writeTextFile(projectPath(dir, "labels"), projectXml(
                      "labels_data",
                      labelTrackXml(
                          labelXml("Intro &amp; Verse", "1.5", "2.25")
                          + labelXml("Marker", "3.0")
                          + labelXml("Inverted", "5.0", "4.0"),
                          "Markers",
                          3)));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "labels"));

    ASSERT_TRUE(result.success) << result.error;
    EXPECT_TRUE(result.tracks.empty());
    ASSERT_EQ(result.labelTracks.size(), 1u);
    EXPECT_EQ(result.labelTracks[0].title, "Markers");
    ASSERT_EQ(result.labelTracks[0].labels.size(), 3u);

    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[0].startTime, 1.5);
    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[0].endTime, 2.25);
    EXPECT_EQ(result.labelTracks[0].labels[0].title, "Intro & Verse");

    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[1].startTime, 3.0);
    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[1].endTime, 3.0);
    EXPECT_EQ(result.labelTracks[0].labels[1].title, "Marker");

    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[2].startTime, 4.0);
    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[2].endTime, 5.0);
    EXPECT_EQ(result.labelTracks[0].labels[2].title, "Inverted");
}

TEST(LegacyAupImporter, importsWaveAndLabelTracksTogether)
{
    QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const QString dataDir = dataDirPath(dir, "mixed");
    makeDataSubdir(dataDir);

    writeAudioFile(blockPath(dataDir, "audio.au"), 44100, 1, SF_FORMAT_AU | SF_FORMAT_FLOAT, { 0.2f });
    writeTextFile(projectPath(dir, "mixed"), projectXml(
                      "mixed_data",
                      monoTrackXml(waveClipXml(simpleBlockXml("audio.au", 1, 0), "0.5"))
                      + labelTrackXml(labelXml("Cue", "0.5", "0.75"), "Cues")));

    LegacyAupImporter importer;
    const auto result = resolve(importer, projectPath(dir, "mixed"));

    ASSERT_TRUE(result.success) << result.error;
    ASSERT_EQ(result.tracks.size(), 1u);
    ASSERT_EQ(result.labelTracks.size(), 1u);
    EXPECT_EQ(result.labelTracks[0].title, "Cues");
    ASSERT_EQ(result.labelTracks[0].labels.size(), 1u);
    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[0].startTime, 0.5);
    EXPECT_DOUBLE_EQ(result.labelTracks[0].labels[0].endTime, 0.75);
    EXPECT_EQ(result.labelTracks[0].labels[0].title, "Cue");
}
