/*
 * Audacity: A Digital Audio Editor
 */

#include "settings.h"
#include "log.h"
#include "types/translatablestring.h"

#include "importexport/export/types/exporttypes.h"

#include "exportpreferencesmodel.h"

using namespace au::importexport;

std::map<ProcessType, muse::TranslatableString> EXPORT_PROCESS_MAPPING {
    { ProcessType::FULL_PROJECT_AUDIO, muse::TranslatableString("export", "Export full project audio") },
    { ProcessType::SELECTED_AUDIO, muse::TranslatableString("export", "Export selected audio") },
    { ProcessType::AUDIO_IN_LOOP_REGION, muse::TranslatableString("export", "Export audio in loop region") },
    { ProcessType::TRACKS_AS_SEPARATE_AUDIO_FILES, muse::TranslatableString("export", "Export tracks as a separate audio files (Stems") },
    { ProcessType::EACH_LABEL_AS_SEPARATE_AUDIO_FILE, muse::TranslatableString("export",
                                                                               "Export each label as a separate audio file (Chapters)") },
    { ProcessType::ALL_LABELS_AS_SUBTITLE_FILE, muse::TranslatableString("export", "Export all labels as a subtitle file") }
};

namespace au::appshell {
ExportPreferencesModel::ExportPreferencesModel(QObject* parent)
    : QObject(parent)
{
    init();
}

void ExportPreferencesModel::init()
{
    //! NOTE: init m_sampleRateMapping
    exportSampleRateList();

    exportConfiguration()->processChanged().onNotify(this, [this] {
        emit currentProcessChanged();
    });

    exportConfiguration()->filenameChanged().onNotify(this, [this] {
        emit filenameChanged();
    });

    exportConfiguration()->directoryPathChanged().onNotify(this, [this] {
        emit directoryPathChanged();
    });

    exportConfiguration()->currentFormatChanged().onNotify(this, [this] {
        emit currentFormatChanged();
        emit fileExtensionChanged();
    });

    exportConfiguration()->exportChannelsChanged().onNotify(this, [this] {
        emit exportChannelsChanged();
    });

    exportConfiguration()->exportSampleRateChanged().onNotify(this, [this](){
        emit exportSampleRateChanged();
    });
}

QString ExportPreferencesModel::currentProcess() const
{
    return EXPORT_PROCESS_MAPPING[exportConfiguration()->process()].translated();
}

void ExportPreferencesModel::setCurrentProcess(const QString& newProcess)
{
    ProcessType type;
    for (auto process : EXPORT_PROCESS_MAPPING) {
        if (newProcess == process.second.translated()) {
            type = process.first;
        }
    }

    if (newProcess == currentProcess()) {
        return;
    }

    exportConfiguration()->setProcess(type);
}

QVariantList ExportPreferencesModel::processList() const
{
    QVariantList result;
    for (const auto& process : EXPORT_PROCESS_MAPPING) {
        result << process.second.translated().toQString();
    }

    return result;
}

QString ExportPreferencesModel::filename() const
{
    return QString::fromStdString(exportConfiguration()->filename());
}

void ExportPreferencesModel::setFilename(const QString& filename)
{
    exportConfiguration()->setFilename(filename.toStdString());
}

QString ExportPreferencesModel::fileExtension() const
{
    return QString::fromStdString(exporter()->formatExtension(currentFormat().toStdString()));
}

QString ExportPreferencesModel::directoryPath() const
{
    return exportConfiguration()->directoryPath().toQString();
}

void ExportPreferencesModel::setDirectoryPath(const QString& path)
{
    if (path == directoryPath()) {
        return;
    }

    exportConfiguration()->setDirectoryPath(path);
}

QString ExportPreferencesModel::currentFormat() const
{
    return QString::fromStdString(exportConfiguration()->currentFormat());
}

void ExportPreferencesModel::setCurrentFormat(const QString& format)
{
    if (format == currentFormat()) {
        return;
    }

    exportConfiguration()->setCurrentFormat(format.toStdString());
}

QVariantList ExportPreferencesModel::formatList() const
{
    QVariantList result;
    for (const auto& format : exporter()->formatList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

ExportChannelsPref::ExportChannels ExportPreferencesModel::exportChannels() const
{
    return exportConfiguration()->exportChannels();
}

void ExportPreferencesModel::setExportChannels(ExportChannelsPref::ExportChannels exportChannels)
{
    exportConfiguration()->setExportChannels(exportChannels);
}

QString ExportPreferencesModel::exportSampleRate() const
{
    auto currentSampleRate = exportConfiguration()->exportSampleRate();
    for (const auto& rate : m_sampleRateMapping) {
        if (currentSampleRate == rate.first) {
            return rate.second;
        }
    }

    return {};
}

QVariantList ExportPreferencesModel::exportSampleRateList()
{
    //! NOTE: this list should be fetched based on selected plugin
    std::vector<uint64_t> sampleRateList = { 8000, 11025, 16000, 22050, 44100 };
    QVariantList result;
    m_sampleRateMapping.clear();
    for (const auto& rate : sampleRateList) {
        QString sampleRateName = QString::number(rate) + " Hz";
        m_sampleRateMapping.push_back(std::make_pair(rate, sampleRateName));
        result << QVariant::fromValue(sampleRateName);
    }

    return result;
}

void ExportPreferencesModel::exportSampleRateSelected(const QString& rateName)
{
    if (rateName == exportSampleRate()) {
        return;
    }

    auto it = std::find_if(m_sampleRateMapping.begin(), m_sampleRateMapping.end(),
                           [&rateName](const auto& rate) { return rateName == rate.second; });
    if (it != m_sampleRateMapping.end()) {
        exportConfiguration()->setExportSampleRate(it->first);
        return;
    }

    emit exportSampleRateChanged();
}

QString ExportPreferencesModel::exportSampleFormat() const
{
    NOT_IMPLEMENTED;
    return {};
}

QVariantList ExportPreferencesModel::exportSampleFormatList() const
{
    NOT_IMPLEMENTED;
    return {};
}

void ExportPreferencesModel::exportSampleFormatSelected(const QString& format)
{
    Q_UNUSED(format);
    NOT_IMPLEMENTED;
}
}
