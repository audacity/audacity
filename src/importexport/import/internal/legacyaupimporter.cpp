/*
 * Audacity: A Digital Audio Editor
 */

#include "legacyaupimporter.h"

#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QFileInfo>
#include <QHash>
#include <QTemporaryDir>
#include <QVector>
#include <QXmlStreamReader>

#include <sndfile.h>

#include <algorithm>
#include <cmath>
#include <limits>

using namespace au::importexport;

struct LegacyAupImporter::Block
{
    enum class Type {
        Audio,
        Silence
    };

    Type type = Type::Audio;
    QString filePath;
    qint64 start = -1;
    qint64 len = 0;
    qint64 origin = 0;
    int channel = 0;
};

struct LegacyAupImporter::SourceClip
{
    double offset = 0.0;
    std::vector<Block> blocks;
};

struct LegacyAupImporter::SourceTrack
{
    QString title;
    int channel = 2;
    bool linked = false;
    int rate = 44100;
    double offset = 0.0;
    std::vector<SourceClip> clips;
};

namespace {
QString elementName(const QXmlStreamReader& reader)
{
    return reader.name().toString().toLower();
}

QString attr(const QXmlStreamAttributes& attrs, const char* name, const QString& def = QString())
{
    const auto value = attrs.value(QLatin1String(name));
    return value.isNull() ? def : value.toString();
}

int intAttr(const QXmlStreamAttributes& attrs, const char* name, int def = 0)
{
    bool ok = false;
    const int value = attr(attrs, name).toInt(&ok);
    return ok ? value : def;
}

int rateAttr(const QXmlStreamAttributes& attrs, const char* name, int def = 0)
{
    bool ok = false;
    const double value = attr(attrs, name).toDouble(&ok);
    return ok ? static_cast<int>(std::lrint(value)) : def;
}

qint64 int64Attr(const QXmlStreamAttributes& attrs, const char* name, qint64 def = 0)
{
    bool ok = false;
    const qint64 value = attr(attrs, name).toLongLong(&ok);
    return ok ? value : def;
}

double doubleAttr(const QXmlStreamAttributes& attrs, const char* name, double def = 0.0)
{
    bool ok = false;
    const double value = attr(attrs, name).toDouble(&ok);
    return ok ? value : def;
}

bool boolishAttr(const QXmlStreamAttributes& attrs, const char* name, bool def = false)
{
    const QString value = attr(attrs, name).toLower();
    if (value == "1" || value == "true" || value == "on") {
        return true;
    }
    if (value == "0" || value == "false" || value == "off") {
        return false;
    }
    return def;
}

QString findDataDir(const muse::io::path_t& filePath, const QString& projectName)
{
    QFileInfo projectFile(filePath.toQString());
    QDir parent = projectFile.absoluteDir();

    if (!projectName.isEmpty()) {
        const QString projectDir = parent.filePath(projectName);
        if (QFileInfo(projectDir).isDir()) {
            return projectDir;
        }
    }

    const QString fallback = parent.filePath(projectFile.completeBaseName() + "_data");
    return QFileInfo(fallback).isDir() ? fallback : QString();
}

QHash<QString, QString> buildBlockFileMap(const QString& dataDir)
{
    QHash<QString, QString> files;
    QDirIterator it(dataDir, QDir::Files, QDirIterator::Subdirectories);
    while (it.hasNext()) {
        const QString path = it.next();
        files.insert(QFileInfo(path).fileName().toLower(), path);
    }
    return files;
}

QString resolveProjectFile(const QHash<QString, QString>& blockFiles, const QString& filename)
{
    return blockFiles.value(QFileInfo(filename).fileName().toLower());
}

QString resolveAliasFile(const QString& projectDir, const QHash<QString, QString>& blockFiles, const QString& filename)
{
    if (filename.isEmpty()) {
        return QString();
    }

    QFileInfo fileInfo(filename);
    if (fileInfo.exists()) {
        return fileInfo.filePath();
    }

    if (fileInfo.isRelative()) {
        const QString relativeToProject = QDir(projectDir).filePath(filename);
        if (QFileInfo(relativeToProject).exists()) {
            return relativeToProject;
        }
    }

    return resolveProjectFile(blockFiles, filename);
}

SNDFILE* openSndFile(QFile& file, const QString& path, int mode, SF_INFO* info)
{
    Q_UNUSED(path);
    if (!file.open(mode == SFM_READ ? QIODevice::ReadOnly : QIODevice::WriteOnly)) {
        return nullptr;
    }

    SNDFILE* sndFile = sf_open_fd(static_cast<int>(file.handle()), mode, info, SF_FALSE);
    if (!sndFile) {
        file.close();
    }

    return sndFile;
}

void appendWarning(std::vector<std::string>& warnings, const QString& warning)
{
    warnings.push_back(warning.toStdString());
}

bool writeFrames(SNDFILE* sndFile, const float* samples, size_t frames)
{
    return sf_writef_float(sndFile, samples, static_cast<sf_count_t>(frames)) == static_cast<sf_count_t>(frames);
}

bool writeSilence(SNDFILE* sndFile, size_t frames, int channels)
{
    static constexpr size_t chunkFrames = 16384;
    const size_t frameChannels = static_cast<size_t>(std::max(1, channels));
    std::vector<float> silence(chunkFrames * frameChannels, 0.0f);

    while (frames > 0) {
        const size_t framesToWrite = std::min(frames, chunkFrames);
        if (!writeFrames(sndFile, silence.data(), framesToWrite)) {
            return false;
        }
        frames -= framesToWrite;
    }

    return true;
}
}

LegacyAupImporter::LegacyAupImporter()
    : m_tempDir(std::make_unique<QTemporaryDir>(QDir::tempPath() + "/audacity-legacy-aup-XXXXXX"))
{
}

LegacyAupImporter::~LegacyAupImporter() = default;

bool LegacyAupImporter::isLegacyAupFile(const muse::io::path_t& filePath)
{
    return QFileInfo(filePath.toQString()).suffix().compare("aup", Qt::CaseInsensitive) == 0;
}

LegacyAupImporter::Result LegacyAupImporter::resolve(const muse::io::path_t& filePath)
{
    std::vector<SourceTrack> sourceTracks;
    int projectRate = 44100;
    Result result = parse(filePath, sourceTracks, projectRate);
    if (!result.success) {
        return result;
    }

    if (!m_tempDir || !m_tempDir->isValid()) {
        result.success = false;
        result.error = "Unable to create temporary directory for legacy project import.";
        return result;
    }

    result.tracks.clear();
    if (!render(result, sourceTracks, projectRate)) {
        result.success = false;
    }

    return result;
}

LegacyAupImporter::Result LegacyAupImporter::parse(const muse::io::path_t& filePath, std::vector<SourceTrack>& sourceTracks,
                                                   int& projectRate)
{
    Result result;

    QFile file(filePath.toQString());
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        result.error = "Unable to open legacy .aup file.";
        return result;
    }

    QXmlStreamReader reader(&file);
    QHash<QString, QString> blockFiles;
    const QString projectDir = QFileInfo(filePath.toQString()).absolutePath();
    SourceTrack* currentTrack = nullptr;
    SourceClip* currentClip = nullptr;
    int nestedWaveClipDepth = 0;
    qint64 currentWaveBlockStart = -1;
    QVector<QString> stack;
    const auto nextBlockStart = [](const SourceClip& clip) {
        qint64 next = 0;
        for (const Block& block : clip.blocks) {
            if (block.len > 0 && next <= std::numeric_limits<qint64>::max() - block.len) {
                next += block.len;
            }
        }
        return next;
    };
    const auto setBlockStart = [&nextBlockStart, &currentWaveBlockStart](Block& block, const SourceClip& clip) {
        block.start = currentWaveBlockStart >= 0 ? currentWaveBlockStart : nextBlockStart(clip);
    };

    while (!reader.atEnd()) {
        reader.readNext();

        if (reader.isStartElement()) {
            const QString name = elementName(reader);
            const QString parent = stack.empty() ? QString() : stack.back();
            const QXmlStreamAttributes attrs = reader.attributes();

            if (name == "project" || name == "audacityproject") {
                projectRate = rateAttr(attrs, "rate", projectRate);
                const QString dataDir = findDataDir(filePath, attr(attrs, "projname"));
                if (dataDir.isEmpty()) {
                    result.error = "Unable to find the legacy project _data directory.";
                    return result;
                }
                blockFiles = buildBlockFileMap(dataDir);
            } else if (name == "wavetrack") {
                sourceTracks.push_back(SourceTrack {});
                currentTrack = &sourceTracks.back();
                currentTrack->title = attr(attrs, "name", "Audio Track");
                currentTrack->channel = intAttr(attrs, "channel", 2);
                currentTrack->linked = boolishAttr(attrs, "linked", false);
                currentTrack->rate = rateAttr(attrs, "rate", projectRate);
                if (currentTrack->rate <= 0) {
                    currentTrack->rate = projectRate > 0 ? projectRate : 44100;
                }
                currentTrack->offset = doubleAttr(attrs, "offset", 0.0);
                currentClip = nullptr;
            } else if (name == "waveclip" && currentTrack) {
                if (parent == "waveclip") {
                    ++nestedWaveClipDepth;
                } else if (nestedWaveClipDepth == 0) {
                    currentTrack->clips.push_back(SourceClip {});
                    currentClip = &currentTrack->clips.back();
                    currentClip->offset = doubleAttr(attrs, "offset", currentTrack->offset);
                }
            } else if (name == "sequence" && currentTrack && !currentClip && nestedWaveClipDepth == 0) {
                currentTrack->clips.push_back(SourceClip {});
                currentClip = &currentTrack->clips.back();
                currentClip->offset = currentTrack->offset;
            } else if (name == "waveblock" && currentClip && nestedWaveClipDepth == 0) {
                currentWaveBlockStart = int64Attr(attrs, "start", -1);
            } else if (currentClip && nestedWaveClipDepth == 0 && name == "simpleblockfile") {
                Block block;
                block.len = int64Attr(attrs, "len");
                setBlockStart(block, *currentClip);
                block.filePath = resolveProjectFile(blockFiles, attr(attrs, "filename"));
                if (block.filePath.isEmpty()) {
                    appendWarning(result.warnings, "Missing legacy block file: " + attr(attrs, "filename"));
                    block.type = Block::Type::Silence;
                }
                currentClip->blocks.push_back(block);
            } else if (currentClip && nestedWaveClipDepth == 0 && name == "silentblockfile") {
                Block block;
                block.type = Block::Type::Silence;
                block.len = int64Attr(attrs, "len");
                setBlockStart(block, *currentClip);
                currentClip->blocks.push_back(block);
            } else if (currentClip && nestedWaveClipDepth == 0 && name == "pcmaliasblockfile") {
                Block block;
                block.len = int64Attr(attrs, "aliaslen");
                setBlockStart(block, *currentClip);
                block.origin = int64Attr(attrs, "aliasstart");
                block.channel = intAttr(attrs, "aliaschannel");
                const QString aliasFile = attr(attrs, "aliasfile");
                block.filePath = resolveAliasFile(projectDir, blockFiles, aliasFile);
                if (block.filePath.isEmpty() || !QFileInfo(block.filePath).exists()) {
                    appendWarning(result.warnings, "Missing legacy alias file: " + aliasFile);
                    block.type = Block::Type::Silence;
                }
                currentClip->blocks.push_back(block);
            }

            stack.push_back(name);
        } else if (reader.isEndElement()) {
            const QString name = elementName(reader);

            if (name == "waveclip") {
                if (nestedWaveClipDepth > 0) {
                    --nestedWaveClipDepth;
                } else {
                    currentClip = nullptr;
                }
            } else if (name == "wavetrack") {
                currentTrack = nullptr;
                currentClip = nullptr;
            } else if (name == "waveblock") {
                currentWaveBlockStart = -1;
            }

            if (!stack.empty()) {
                stack.pop_back();
            }
        }
    }

    if (reader.error() != QXmlStreamReader::NoError) {
        result.error = reader.errorString().toStdString();
        return result;
    }

    sourceTracks.erase(std::remove_if(sourceTracks.begin(), sourceTracks.end(), [](const SourceTrack& track) {
        return track.clips.empty();
    }), sourceTracks.end());

    result.success = true;
    return result;
}

bool LegacyAupImporter::render(Result& result, const std::vector<SourceTrack>& sourceTracks, int projectRate)
{
    const auto sameClipShape = [](const auto& left, const auto& right) {
        if (std::abs(left.offset - right.offset) > 0.000001 || left.blocks.size() != right.blocks.size()) {
            return false;
        }

        for (size_t i = 0; i < left.blocks.size(); ++i) {
            if (left.blocks[i].len != right.blocks[i].len || left.blocks[i].start != right.blocks[i].start) {
                return false;
            }
        }

        return true;
    };

    const auto canImportAsStereo = [&sameClipShape](const auto& left, const auto& right) {
        if (!left.linked || left.channel != 0 || right.channel != 1 || left.rate != right.rate || left.clips.size() != right.clips.size()) {
            return false;
        }

        for (size_t i = 0; i < left.clips.size(); ++i) {
            if (!sameClipShape(left.clips[i], right.clips[i])) {
                return false;
            }
        }

        return true;
    };

    for (size_t i = 0; i < sourceTracks.size(); ++i) {
        const SourceTrack& track = sourceTracks[i];
        const bool stereo = i + 1 < sourceTracks.size() && canImportAsStereo(track, sourceTracks[i + 1]);

        Track renderedTrack;
        renderedTrack.title = track.title.toStdString();
        renderedTrack.channels = stereo ? 2 : 1;

        for (size_t clipIndex = 0; clipIndex < track.clips.size(); ++clipIndex) {
            if (track.clips[clipIndex].blocks.empty()) {
                continue;
            }

            const muse::io::path_t wavPath = makeTempWavPath(result.tracks.size(), clipIndex);
            bool ok = false;
            if (stereo) {
                ok = writeStereoClip(track, track.clips[clipIndex], sourceTracks[i + 1], sourceTracks[i + 1].clips[clipIndex],
                                     wavPath, result.warnings);
            } else {
                ok = writeMonoClip(track, track.clips[clipIndex], wavPath, result.warnings);
            }

            if (!ok) {
                result.error = "Unable to render legacy project audio.";
                return false;
            }

            renderedTrack.clips.push_back(Clip { wavPath, track.clips[clipIndex].offset });
        }

        if (!renderedTrack.clips.empty()) {
            result.tracks.push_back(std::move(renderedTrack));
        }

        if (stereo) {
            ++i;
        }
    }

    if (result.tracks.empty()) {
        result.error = "No importable wave clips were found in the legacy project.";
        return false;
    }

    if (projectRate <= 0) {
        appendWarning(result.warnings, "Legacy project sample rate was invalid; clip rates were used instead.");
    }

    return true;
}

std::vector<float> LegacyAupImporter::readBlock(const Block& block, std::vector<std::string>& warnings) const
{
    const size_t len = block.len > 0 ? static_cast<size_t>(block.len) : 0;
    std::vector<float> output(len, 0.0f);

    if (block.type == Block::Type::Silence || len == 0) {
        return output;
    }

    QFile file(block.filePath);
    SF_INFO info {};
    SNDFILE* sndFile = openSndFile(file, block.filePath, SFM_READ, &info);
    if (!sndFile) {
        appendWarning(warnings, "Unable to read legacy audio block: " + block.filePath);
        return output;
    }

    if (block.origin > 0 && sf_seek(sndFile, block.origin, SEEK_SET) < 0) {
        appendWarning(warnings, "Unable to seek in legacy audio block: " + block.filePath);
        sf_close(sndFile);
        return output;
    }

    const int channels = std::max(1, info.channels);
    const int channel = std::clamp(block.channel, 0, channels - 1);
    std::vector<float> interleaved(len * static_cast<size_t>(channels), 0.0f);
    const sf_count_t framesRead = sf_readf_float(sndFile, interleaved.data(), static_cast<sf_count_t>(len));
    sf_close(sndFile);

    if (framesRead < static_cast<sf_count_t>(len)) {
        appendWarning(warnings, "Legacy audio block was shorter than expected: " + block.filePath);
    }

    const size_t validFrames = static_cast<size_t>(std::max<sf_count_t>(0, framesRead));
    for (size_t i = 0; i < validFrames; ++i) {
        output[i] = interleaved[(i * static_cast<size_t>(channels)) + static_cast<size_t>(channel)];
    }

    return output;
}

bool LegacyAupImporter::writeMonoClip(const SourceTrack& track, const SourceClip& clip, const muse::io::path_t& path,
                                      std::vector<std::string>& warnings) const
{
    QFile file(path.toQString());
    SF_INFO info {};
    info.samplerate = track.rate > 0 ? track.rate : 44100;
    info.channels = 1;
    info.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;

    SNDFILE* sndFile = openSndFile(file, path.toQString(), SFM_WRITE, &info);
    if (!sndFile) {
        return false;
    }

    qint64 cursor = 0;
    for (const Block& block : clip.blocks) {
        if (block.start > cursor && !writeSilence(sndFile, static_cast<size_t>(block.start - cursor), 1)) {
            sf_close(sndFile);
            return false;
        }
        if (block.start > cursor) {
            cursor = block.start;
        }

        std::vector<float> samples = readBlock(block, warnings);
        if (!samples.empty()) {
            if (!writeFrames(sndFile, samples.data(), samples.size())) {
                sf_close(sndFile);
                return false;
            }
        }
        cursor += static_cast<qint64>(samples.size());
    }

    return sf_close(sndFile) == 0;
}

bool LegacyAupImporter::writeStereoClip(const SourceTrack& leftTrack, const SourceClip& leftClip, const SourceTrack& rightTrack,
                                        const SourceClip& rightClip, const muse::io::path_t& path,
                                        std::vector<std::string>& warnings) const
{
    QFile file(path.toQString());
    SF_INFO info {};
    info.samplerate = leftTrack.rate > 0 ? leftTrack.rate : rightTrack.rate;
    if (info.samplerate <= 0) {
        info.samplerate = 44100;
    }
    info.channels = 2;
    info.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;

    SNDFILE* sndFile = openSndFile(file, path.toQString(), SFM_WRITE, &info);
    if (!sndFile) {
        return false;
    }

    qint64 cursor = 0;
    for (size_t blockIndex = 0; blockIndex < leftClip.blocks.size(); ++blockIndex) {
        const qint64 blockStart = leftClip.blocks[blockIndex].start;
        if (blockStart > cursor && !writeSilence(sndFile, static_cast<size_t>(blockStart - cursor), 2)) {
            sf_close(sndFile);
            return false;
        }
        if (blockStart > cursor) {
            cursor = blockStart;
        }

        std::vector<float> left = readBlock(leftClip.blocks[blockIndex], warnings);
        std::vector<float> right = readBlock(rightClip.blocks[blockIndex], warnings);
        const size_t frames = std::max(left.size(), right.size());
        std::vector<float> interleaved(frames * 2, 0.0f);
        for (size_t i = 0; i < frames; ++i) {
            interleaved[i * 2] = i < left.size() ? left[i] : 0.0f;
            interleaved[i * 2 + 1] = i < right.size() ? right[i] : 0.0f;
        }
        if (!interleaved.empty()) {
            if (!writeFrames(sndFile, interleaved.data(), frames)) {
                sf_close(sndFile);
                return false;
            }
        }
        cursor += static_cast<qint64>(frames);
    }

    return sf_close(sndFile) == 0;
}

muse::io::path_t LegacyAupImporter::makeTempWavPath(size_t trackIndex, size_t clipIndex) const
{
    const QString name = QString("legacy_aup_track_%1_clip_%2.wav")
                         .arg(static_cast<qulonglong>(trackIndex + 1))
                         .arg(static_cast<qulonglong>(clipIndex + 1));
    return muse::io::path_t(QDir(m_tempDir->path()).filePath(name));
}
