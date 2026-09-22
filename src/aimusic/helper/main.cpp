/*
 * Audacity: A Digital Audio Editor
 */
#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QHash>
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>
#include <QVector>
#include <QTextStream>

#include <vamp-hostsdk/PluginLoader.h>
#include <vamp-sdk/Plugin.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <memory>

namespace {
double estimatePitchHz(const QByteArray& bytes, double sampleRate, double rms)
{
    if (sampleRate <= 0.0 || rms < 0.005 || bytes.size() < static_cast<int>(sizeof(float) * 100)) {
        return 0.0;
    }

    const auto* samples = reinterpret_cast<const float*>(bytes.constData());
    const int frameCount = bytes.size() / static_cast<int>(sizeof(float));
    int crossings = 0;
    float previous = samples[0];
    for (int frame = 1; frame < frameCount; ++frame) {
        const float current = samples[frame];
        if ((previous < 0.0f && current >= 0.0f) || (previous >= 0.0f && current < 0.0f)) {
            ++crossings;
        }
        previous = current;
    }

    const double pitch = (static_cast<double>(crossings) * sampleRate) / (2.0 * frameCount);
    return pitch >= 50.0 && pitch <= 2000.0 ? pitch : 0.0;
}

struct FeatureValue {
    double timestamp = 0.0;
    QVector<float> values;
    QString label;
};

using OutputFeatures = QHash<QString, QVector<FeatureValue>>;

QVector<float> resampleTo44100(const float* samples, int frameCount, double sourceRate)
{
    constexpr double targetRate = 44100.0;
    if (sourceRate == targetRate) {
        return QVector<float>(samples, samples + frameCount);
    }

    const int targetFrames = qMax(1, qRound((frameCount * targetRate) / sourceRate));
    QVector<float> output(targetFrames);
    for (int targetFrame = 0; targetFrame < targetFrames; ++targetFrame) {
        const double sourcePosition = (targetFrame * sourceRate) / targetRate;
        const int first = qBound(0, static_cast<int>(sourcePosition), frameCount - 1);
        const int second = qMin(first + 1, frameCount - 1);
        const double fraction = sourcePosition - first;
        output[targetFrame] = static_cast<float>(samples[first] + (samples[second] - samples[first]) * fraction);
    }
    return output;
}

bool runPlugin(const std::string& library, const std::string& identifier, const QVector<float>& samples, OutputFeatures& output, QString& error)
{
    auto* loader = Vamp::HostExt::PluginLoader::getInstance();
    const auto key = loader->composePluginKey(library, identifier);
    std::unique_ptr<Vamp::Plugin> plugin(loader->loadPlugin(key, 44100.0f, Vamp::HostExt::PluginLoader::ADAPT_ALL_SAFE));
    if (!plugin) {
        error = QString("Could not load QM Vamp plugin '%1'.").arg(QString::fromStdString(identifier));
        return false;
    }

    const auto descriptors = plugin->getOutputDescriptors();
    size_t blockSize = plugin->getPreferredBlockSize();
    size_t stepSize = plugin->getPreferredStepSize();
    if (blockSize == 0) {
        blockSize = 1024;
    }
    if (stepSize == 0) {
        stepSize = blockSize;
    }
    if (stepSize > blockSize || !plugin->initialise(1, stepSize, blockSize)) {
        error = QString("QM Vamp plugin '%1' could not initialise.").arg(QString::fromStdString(identifier));
        return false;
    }

    auto collect = [&output, &descriptors](const Vamp::Plugin::FeatureSet& featureSet) {
        for (const auto& [outputIndex, features] : featureSet) {
            if (outputIndex < 0 || static_cast<size_t>(outputIndex) >= descriptors.size()) {
                continue;
            }
            auto& destination = output[QString::fromStdString(descriptors[outputIndex].identifier)];
            for (const auto& feature : features) {
                FeatureValue value;
                value.timestamp = feature.hasTimestamp
                                  ? feature.timestamp.sec + (feature.timestamp.nsec / 1000000000.0)
                                  : 0.0;
                value.values.reserve(static_cast<qsizetype>(feature.values.size()));
                for (const auto sample : feature.values) {
                    value.values.push_back(sample);
                }
                value.label = QString::fromStdString(feature.label);
                destination.push_back(std::move(value));
            }
        }
    };

    QVector<float> block(static_cast<qsizetype>(blockSize), 0.0f);
    const float* inputBuffers[] = { block.constData() };
    const int finalBlocks = qMax(1, static_cast<int>((blockSize + stepSize - 1) / stepSize) - 1);
    int trailing = finalBlocks;
    size_t position = 0;
    do {
        const size_t available = position < static_cast<size_t>(samples.size())
                                 ? qMin(blockSize, static_cast<size_t>(samples.size()) - position)
                                 : 0;
        std::fill(block.begin(), block.end(), 0.0f);
        if (available > 0) {
            std::copy_n(samples.constData() + position, available, block.data());
        } else {
            --trailing;
        }
        collect(plugin->process(inputBuffers, Vamp::RealTime::frame2RealTime(static_cast<long>(position), 44100)));
        position += stepSize;
    } while (position < static_cast<size_t>(samples.size()) || trailing > 0);
    collect(plugin->getRemainingFeatures());
    return true;
}

double median(QVector<double> values)
{
    if (values.isEmpty()) {
        return 0.0;
    }
    std::sort(values.begin(), values.end());
    const qsizetype middle = values.size() / 2;
    return values.size() % 2 == 0 ? (values[middle - 1] + values[middle]) / 2.0 : values[middle];
}

QString keyName(int keyNumber)
{
    static const QStringList names { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };
    if (keyNumber < 1 || keyNumber > 24) {
        return {};
    }
    return names[(keyNumber - 1) % 12] + (keyNumber > 12 ? " minor" : " major");
}
}

int main(int argc, char* argv[])
{
    QCoreApplication application(argc, argv);
    QCommandLineParser parser;
    parser.addOption({ "selection-start", "Selection start in seconds.", "seconds" });
    parser.addOption({ "selection-end", "Selection end in seconds.", "seconds" });
    parser.addOption({ "track-count", "Number of selected tracks.", "count" });
    parser.addOption({ "audio-path", "Path to decoded mono float32 audio.", "path" });
    parser.addOption({ "sample-rate", "Decoded audio sample rate.", "hertz" });
    parser.process(application);

    const double start = parser.value("selection-start").toDouble();
    const double end = parser.value("selection-end").toDouble();
    const int trackCount = parser.value("track-count").toInt();
    const double sampleRate = parser.value("sample-rate").toDouble();
    QFile input(parser.value("audio-path"));
    if (sampleRate <= 0.0 || !input.open(QIODevice::ReadOnly)) {
        return 2;
    }
    const QByteArray audioBytes = input.readAll();
    if (audioBytes.isEmpty() || audioBytes.size() % static_cast<int>(sizeof(float)) != 0) {
        return 3;
    }

    const auto* samples = reinterpret_cast<const float*>(audioBytes.constData());
    const int frameCount = audioBytes.size() / static_cast<int>(sizeof(float));
    double energy = 0.0;
    for (int frame = 0; frame < frameCount; ++frame) {
        energy += static_cast<double>(samples[frame]) * samples[frame];
    }
    const double rms = std::sqrt(energy / frameCount);
    const QVector<float> analysisSamples = resampleTo44100(samples, frameCount, sampleRate);

    const QString vampPath = QDir(QCoreApplication::applicationDirPath()).filePath("vamp");
    qputenv("VAMP_PATH", QDir::toNativeSeparators(vampPath).toUtf8());

    OutputFeatures tempoFeatures;
    OutputFeatures keyFeatures;
    OutputFeatures chordFeatures;
    OutputFeatures segmentFeatures;
    QString analysisError;
    const bool tempoReady = runPlugin("qm-vamp-plugins", "qm-tempotracker", analysisSamples, tempoFeatures, analysisError);
    const bool keyReady = tempoReady && runPlugin("qm-vamp-plugins", "qm-keydetector", analysisSamples, keyFeatures, analysisError);
    QString chordError;
    const bool chordsReady = keyReady && runPlugin("nnls-chroma", "chordino", analysisSamples, chordFeatures, chordError);
    QString arrangementError;
    const bool arrangementReady = keyReady && runPlugin("qm-vamp-plugins", "qm-segmenter", analysisSamples, segmentFeatures, arrangementError);

    QVector<double> beatTimes;
    for (const auto& feature : tempoFeatures.value("beats")) {
        beatTimes.push_back(feature.timestamp);
    }
    QVector<double> bpmValues;
    for (int index = 1; index < beatTimes.size(); ++index) {
        const double interval = beatTimes[index] - beatTimes[index - 1];
        if (interval > 0.0) {
            bpmValues.push_back(60.0 / interval);
        }
    }
    const double bpm = median(bpmValues);
    const double bpmConfidence = bpmValues.size() > 1
                                 ? qMax(0.0, 1.0 - std::sqrt([&bpmValues, bpm] {
                                       double sum = 0.0;
                                       for (const auto value : bpmValues) sum += (value - bpm) * (value - bpm);
                                       return sum / bpmValues.size();
                                   }()) / qMax(1.0, bpm))
                                 : 0.0;

    QHash<int, int> keyCounts;
    for (const auto& feature : keyFeatures.value("key")) {
        if (!feature.values.isEmpty()) {
            ++keyCounts[static_cast<int>(std::lround(feature.values.front()))];
        }
    }
    int detectedKey = 0;
    int detectedKeyCount = 0;
    int keyEventCount = 0;
    for (auto it = keyCounts.cbegin(); it != keyCounts.cend(); ++it) {
        keyEventCount += it.value();
        if (it.value() > detectedKeyCount) {
            detectedKey = it.key();
            detectedKeyCount = it.value();
        }
    }

    QJsonArray beats;
    for (const auto time : beatTimes) {
        beats.append(time);
    }

    // The key detector emits an event only when its estimate changes. Preserve
    // those boundaries for a sparse harmonic map rather than every beat.
    QJsonArray keyRegions;
    for (const auto& feature : keyFeatures.value("key")) {
        if (feature.timestamp < 0.0 || feature.timestamp >= qMax(0.0, end - start) || feature.label.isEmpty()) {
            continue;
        }
        keyRegions.append(QJsonObject {
            { "startSeconds", feature.timestamp },
            { "key", feature.label }
        });
    }
    if (keyRegions.isEmpty() && !keyName(detectedKey).isEmpty()) {
        keyRegions.append(QJsonObject {
            { "startSeconds", 0.0 },
            { "key", keyName(detectedKey) }
        });
    }

    QJsonArray chordRegions;
    for (const auto& feature : chordFeatures.value("simplechord")) {
        const QString chord = feature.label.trimmed();
        if (feature.timestamp < 0.0 || feature.timestamp >= qMax(0.0, end - start)
            || chord.isEmpty() || chord.size() > 64) {
            continue;
        }
        chordRegions.append(QJsonObject {
            { "startSeconds", feature.timestamp },
            { "chord", chord }
        });
    }

    // The segmenter groups acoustically similar passages. Its numeric classes
    // are intentionally presented as neutral section labels rather than
    // guessing musical roles such as verse or chorus.
    QJsonArray arrangementRegions;
    for (const auto& feature : segmentFeatures.value("segmentation")) {
        if (feature.timestamp < 0.0 || feature.timestamp >= qMax(0.0, end - start)
            || feature.values.isEmpty()) {
            continue;
        }
        const int sectionType = qRound(feature.values.front());
        if (sectionType < 1 || sectionType > 12) {
            continue;
        }
        arrangementRegions.append(QJsonObject {
            { "startSeconds", feature.timestamp },
            { "section", QString("Section %1").arg(sectionType) }
        });
    }

    QJsonObject response {
        { "protocolVersion", 1 },
        { "status", keyReady ? "ready" : "analysis-unavailable" },
        { "selectionStartSeconds", start },
        { "selectionEndSeconds", end },
        { "durationSeconds", qMax(0.0, end - start) },
        { "trackCount", trackCount },
        { "sampleRate", sampleRate },
        { "audioFrames", frameCount },
        { "rms", rms },
        { "estimatedPitchHz", estimatePitchHz(audioBytes, sampleRate, rms) },
        { "analysisSampleRate", 44100 },
        { "analysisEngine", "QM Vamp Plugins + Chordino + Segmenter" },
        { "bpm", bpm },
        { "bpmConfidence", bpmConfidence },
        { "beatTimesSeconds", beats },
        { "keyRegions", keyRegions },
        { "chordRegions", chordRegions },
        { "chordStatus", chordsReady ? "ready" : "unavailable" },
        { "chordError", chordError },
        { "arrangementRegions", arrangementRegions },
        { "arrangementStatus", arrangementReady ? "ready" : "unavailable" },
        { "arrangementError", arrangementError },
        { "musicalKey", keyName(detectedKey) },
        { "keyConfidence", keyEventCount == 0 ? 0.0 : static_cast<double>(detectedKeyCount) / keyEventCount },
        { "analysisError", analysisError }
    };

    QTextStream(stdout) << QJsonDocument(response).toJson(QJsonDocument::Compact);
    return 0;
}
