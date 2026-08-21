/*
 * Audacity: A Digital Audio Editor
 */
#include "deepfilternet3effect.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <functional>
#include <memory>
#include <vector>

#include <QCoreApplication>
#include <QDataStream>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include <QProcess>
#include <QStandardPaths>
#include <QTemporaryFile>

#include "framework/global/types/translatablestring.h"

#include "au3-effects/EffectOutputTracks.h"
#include "au3-strings/TranslatableString.h"
#include "au3-wave-track/WaveChannelUtilities.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::effects {
namespace {
constexpr auto HeaderByteCount = qint64 { 32 };
constexpr char PlanarPcmMagic[8] { 'S', 'S', 'D', 'F', 'N', '3', '\0', '\0' };
constexpr quint32 PlanarPcmVersion = 1;

struct TrackJob
{
    WaveTrack* track {};
    sampleCount start;
    size_t frameCount {};
    size_t channelCount {};
    int sampleRate {};
    std::unique_ptr<QTemporaryFile> input;
    std::unique_ptr<QTemporaryFile> output;
};

struct WorkerResult
{
    bool success {};
    bool cancelled {};
    QString error;
};

using ProgressCallback = std::function<bool (double, const QString&)>;

std::unique_ptr<QTemporaryFile> makeTemporaryFile(const QString& suffix)
{
    auto file = std::make_unique<QTemporaryFile>(
        QDir::tempPath() + QStringLiteral("/audacity-deepfilternet3-XXXXXX") + suffix);
    file->setAutoRemove(true);
    if (!file->open()) {
        return {};
    }
    return file;
}

bool writeHeader(QIODevice& device, const TrackJob& job, QString& error)
{
    QDataStream stream(&device);
    stream.setByteOrder(QDataStream::LittleEndian);
    if (stream.writeRawData(PlanarPcmMagic, sizeof(PlanarPcmMagic)) != sizeof(PlanarPcmMagic)) {
        error = TranslatableString(
            "effects-deepfilternet3", "Could not write the temporary audio header").qTranslated();
        return false;
    }
    stream << PlanarPcmVersion
           << quint32(job.sampleRate)
           << quint32(job.channelCount)
           << quint32(0)
           << quint64(job.frameCount);
    if (stream.status() != QDataStream::Ok) {
        error = TranslatableString(
            "effects-deepfilternet3", "Could not write the temporary audio header").qTranslated();
        return false;
    }
    return true;
}

bool readAndValidateHeader(QIODevice& device, const TrackJob& job, QString& error)
{
    QDataStream stream(&device);
    stream.setByteOrder(QDataStream::LittleEndian);

    char magic[sizeof(PlanarPcmMagic)] {};
    if (stream.readRawData(magic, sizeof(magic)) != sizeof(magic)
        || std::memcmp(magic, PlanarPcmMagic, sizeof(magic)) != 0) {
        error = TranslatableString(
            "effects-deepfilternet3",
            "The speech-swift worker returned an invalid audio file").qTranslated();
        return false;
    }

    quint32 version {};
    quint32 sampleRate {};
    quint32 channelCount {};
    quint32 reserved {};
    quint64 frameCount {};
    stream >> version >> sampleRate >> channelCount >> reserved >> frameCount;
    if (stream.status() != QDataStream::Ok
        || version != PlanarPcmVersion
        || sampleRate != quint32(job.sampleRate)
        || channelCount != quint32(job.channelCount)
        || reserved != 0
        || frameCount != quint64(job.frameCount)) {
        error = TranslatableString(
            "effects-deepfilternet3",
            "The speech-swift worker returned incompatible audio metadata").qTranslated();
        return false;
    }

    const auto expectedSize = HeaderByteCount
                              + qint64(job.frameCount) * qint64(job.channelCount)
                              * qint64(sizeof(float));
    if (device.size() != expectedSize) {
        error = TranslatableString(
            "effects-deepfilternet3",
            "The speech-swift worker returned truncated audio").qTranslated();
        return false;
    }
    return true;
}

QString findWorkerExecutable()
{
    const auto configured = qEnvironmentVariable("AUDACITY_SPEECH_SWIFT_WORKER");
    if (!configured.isEmpty()) {
        const QFileInfo info(configured);
        if (info.isFile() && info.isExecutable()) {
            return info.absoluteFilePath();
        }
    }

    const auto appDirectory = QCoreApplication::applicationDirPath();
    const QStringList candidates {
        QDir(appDirectory).absoluteFilePath(
            QStringLiteral("../Helpers/audacity-speech-swift-worker")),
        QDir(appDirectory).absoluteFilePath(
            QStringLiteral("audacity-speech-swift-worker")),
        QStandardPaths::findExecutable(QStringLiteral("audacity-speech-swift-worker"))
    };
    for (const auto& candidate : candidates) {
        const QFileInfo info(candidate);
        if (info.isFile() && info.isExecutable()) {
            return info.absoluteFilePath();
        }
    }
    return {};
}

WorkerResult runWorker(const std::vector<TrackJob>& jobs, const ProgressCallback& progress)
{
    const auto executable = findWorkerExecutable();
    if (executable.isEmpty()) {
        return {
            false,
            false,
            TranslatableString(
                "effects-deepfilternet3",
                "The speech-swift DeepFilterNet3 worker was not found. "
                "Rebuild with AU_BUILD_SPEECH_SWIFT_EFFECT enabled, or set "
                "AUDACITY_SPEECH_SWIFT_WORKER to the worker executable.").qTranslated()
        };
    }

    const auto cacheDirectory = QStandardPaths::writableLocation(QStandardPaths::CacheLocation)
                                + QStringLiteral("/speech-swift");
    if (!QDir().mkpath(cacheDirectory)) {
        return {
            false,
            false,
            TranslatableString(
                "effects-deepfilternet3",
                "Could not create the speech-swift model cache").qTranslated()
        };
    }

    QStringList arguments { QStringLiteral("enhance"), QStringLiteral("--cache-dir"), cacheDirectory };
    for (const auto& job : jobs) {
        arguments << QStringLiteral("--input") << job.input->fileName()
                  << QStringLiteral("--output") << job.output->fileName();
    }

    QProcess process;
    process.setProgram(executable);
    process.setArguments(arguments);
    process.setProcessChannelMode(QProcess::SeparateChannels);
    process.start();
    if (!process.waitForStarted(10'000)) {
        return {
            false,
            false,
            TranslatableString(
                "effects-deepfilternet3",
                "Could not start the speech-swift worker: %1").qTranslated().arg(process.errorString())
        };
    }

    QByteArray stdoutBuffer;
    QByteArray stderrBuffer;
    QString workerError;
    bool receivedResult = false;
    double lastProgress = 0.1;
    QString lastMessage = QStringLiteral("Running speech-swift DeepFilterNet3…");
    auto consumeOutput = [&] {
        stdoutBuffer.append(process.readAllStandardOutput());
        stderrBuffer.append(process.readAllStandardError());
        while (true) {
            const auto newline = stdoutBuffer.indexOf('\n');
            if (newline < 0) {
                break;
            }
            const auto line = stdoutBuffer.left(newline).trimmed();
            stdoutBuffer.remove(0, newline + 1);
            if (line.isEmpty()) {
                continue;
            }

            const auto document = QJsonDocument::fromJson(line);
            if (!document.isObject()) {
                continue;
            }
            const auto object = document.object();
            const auto type = object.value(QStringLiteral("type")).toString();
            if (type == QStringLiteral("progress")) {
                const auto fraction = std::clamp(
                    object.value(QStringLiteral("fraction")).toDouble(), 0.0, 1.0);
                const auto message = object.value(QStringLiteral("message")).toString();
                lastProgress = 0.1 + 0.8 * fraction;
                lastMessage = message;
                if (!progress(lastProgress, lastMessage)) {
                    process.terminate();
                    if (!process.waitForFinished(2'000)) {
                        process.kill();
                        process.waitForFinished(2'000);
                    }
                    return false;
                }
            } else if (type == QStringLiteral("error")) {
                workerError = object.value(QStringLiteral("message")).toString();
            } else if (type == QStringLiteral("result")) {
                receivedResult = true;
            }
        }
        return true;
    };

    while (process.state() != QProcess::NotRunning) {
        process.waitForFinished(100);
        if (!consumeOutput()) {
            return { false, true, {} };
        }
        if (!progress(lastProgress, lastMessage)) {
            process.terminate();
            if (!process.waitForFinished(2'000)) {
                process.kill();
                process.waitForFinished(2'000);
            }
            return { false, true, {} };
        }
    }
    stdoutBuffer.append('\n');
    if (!consumeOutput()) {
        return { false, true, {} };
    }

    if (process.exitStatus() != QProcess::NormalExit || process.exitCode() != 0 || !receivedResult) {
        if (workerError.isEmpty()) {
            workerError = QString::fromUtf8(stderrBuffer).trimmed();
        }
        if (workerError.isEmpty()) {
            workerError = TranslatableString(
                "effects-deepfilternet3",
                "The speech-swift worker failed without an error message").qTranslated();
        }
        return { false, false, workerError };
    }
    return { true, false, {} };
}
}

const ComponentInterfaceSymbol DeepFilterNet3Effect::Symbol {
    TranslatableString("effects-deepfilternet3", "DeepFilterNet3 Noise Suppression")
};

DeepFilterNet3Effect::DeepFilterNet3Effect()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(false);
}

const EffectParameterMethods& DeepFilterNet3Effect::Parameters() const
{
    static CapturedParameters<DeepFilterNet3Effect> parameters;
    return parameters;
}

ComponentInterfaceSymbol DeepFilterNet3Effect::GetSymbol() const
{
    return Symbol;
}

TranslatableString DeepFilterNet3Effect::GetDescription() const
{
    return TranslatableString(
        "effects-deepfilternet3",
        "Removes background noise using speech-swift's native Core ML DeepFilterNet3 runtime");
}

::EffectType DeepFilterNet3Effect::GetType() const
{
    return EffectTypeProcess;
}

bool DeepFilterNet3Effect::Process(::EffectInstance&, ::EffectSettings&)
{
    mLastError.clear();
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

    std::vector<TrackJob> jobs;
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        const auto selectionStart = std::max(track->GetStartTime(), mT0);
        const auto selectionEnd = std::min(track->GetEndTime(), mT1);
        if (selectionEnd <= selectionStart) {
            continue;
        }

        const auto start = track->TimeToLongSamples(selectionStart);
        const auto end = track->TimeToLongSamples(selectionEnd);
        const auto frameCount = (end - start).as_size_t();
        const auto channelCount = track->Channels().size();
        if (frameCount == 0 || channelCount == 0) {
            continue;
        }

        auto input = makeTemporaryFile(QStringLiteral("-input.sspcm"));
        auto output = makeTemporaryFile(QStringLiteral("-output.sspcm"));
        if (!input || !output) {
            mLastError = TranslatableString(
                "effects-deepfilternet3",
                "Could not create temporary files for speech-swift").translated().toStdString();
            return false;
        }
        output->close();

        const auto sampleRate = int(std::lround(track->GetRate()));
        if (sampleRate < 8'000 || sampleRate > 384'000) {
            mLastError = TranslatableString(
                "effects-deepfilternet3",
                "DeepFilterNet3 supports track rates from 8 kHz to 384 kHz.")
                         .translated().toStdString();
            return false;
        }

        jobs.push_back({
                track,
                start,
                frameCount,
                channelCount,
                sampleRate,
                std::move(input),
                std::move(output)
            });
    }

    if (jobs.empty()) {
        mLastError = TranslatableString("effects-deepfilternet3", "No audio selected.")
                     .translated().toStdString();
        return false;
    }

    quint64 totalSamples = 0;
    for (const auto& job : jobs) {
        totalSamples += quint64(job.frameCount) * quint64(job.channelCount);
    }
    quint64 preparedSamples = 0;
    for (auto& job : jobs) {
        QString error;
        if (!writeHeader(*job.input, job, error)) {
            mLastError = error.toUtf8().toStdString();
            return false;
        }

        Floats buffer { job.track->GetMaxBlockSize() };
        for (const auto& channel : job.track->Channels()) {
            auto position = job.start;
            const auto end = job.start + sampleCount(job.frameCount);
            while (position < end) {
                const auto block = limitSampleBufferSize(
                    channel->GetBestBlockSize(position), end - position);
                if (!channel->GetFloats(buffer.get(), position, block)) {
                    mLastError = TranslatableString(
                        "effects-deepfilternet3",
                        "Could not read selected audio for speech-swift").translated().toStdString();
                    return false;
                }
                const auto bytes = qint64(block * sizeof(float));
                if (job.input->write(reinterpret_cast<const char*>(buffer.get()), bytes) != bytes) {
                    mLastError = TranslatableString(
                        "effects-deepfilternet3",
                        "Could not write selected audio for speech-swift").translated().toStdString();
                    return false;
                }
                position += block;
                preparedSamples += block;
                if (TotalProgress(
                        0.1 * double(preparedSamples) / double(totalSamples),
                        TranslatableString(
                            "effects-deepfilternet3", "Preparing audio for DeepFilterNet3…"))) {
                    return false;
                }
            }
        }
        if (!job.input->flush()) {
            mLastError = TranslatableString(
                "effects-deepfilternet3",
                "Could not flush selected audio for speech-swift").translated().toStdString();
            return false;
        }
        job.input->close();
    }

    const auto workerResult = runWorker(jobs, [this](double fraction, const QString&) {
        return !TotalProgress(
            fraction,
            TranslatableString("effects-deepfilternet3", "Running DeepFilterNet3…"));
    });
    if (!workerResult.success) {
        if (!workerResult.cancelled) {
            mLastError = workerResult.error.toUtf8().toStdString();
        }
        return false;
    }

    quint64 importedSamples = 0;
    for (auto& job : jobs) {
        QFile outputFile(job.output->fileName());
        if (!outputFile.open(QIODevice::ReadOnly)) {
            mLastError = TranslatableString(
                "effects-deepfilternet3",
                "Could not open the enhanced audio returned by speech-swift").translated().toStdString();
            return false;
        }
        QString error;
        if (!readAndValidateHeader(outputFile, job, error)) {
            mLastError = error.toUtf8().toStdString();
            return false;
        }

        Floats buffer { job.track->GetMaxBlockSize() };
        for (const auto& channel : job.track->Channels()) {
            auto position = job.start;
            const auto end = job.start + sampleCount(job.frameCount);
            while (position < end) {
                const auto block = limitSampleBufferSize(
                    channel->GetBestBlockSize(position), end - position);
                const auto bytes = qint64(block * sizeof(float));
                if (outputFile.read(reinterpret_cast<char*>(buffer.get()), bytes) != bytes) {
                    mLastError = TranslatableString(
                        "effects-deepfilternet3",
                        "Could not read the enhanced audio returned by speech-swift").translated().toStdString();
                    return false;
                }
                if (!channel->SetFloats(buffer.get(), position, block)) {
                    mLastError = TranslatableString(
                        "effects-deepfilternet3",
                        "Could not apply the enhanced audio to the Audacity track").translated().toStdString();
                    return false;
                }
                position += block;
                importedSamples += block;
                if (TotalProgress(
                        0.9 + 0.1 * double(importedSamples) / double(totalSamples),
                        TranslatableString("effects-deepfilternet3", "Applying enhanced audio…"))) {
                    return false;
                }
            }
        }
    }

    outputs.Commit();
    return true;
}
}
