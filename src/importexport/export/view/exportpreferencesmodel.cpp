/*
 * Audacity: A Digital Audio Editor
 */

#include "exportpreferencesmodel.h"

#include "translation.h"

using namespace au::importexport;

std::map<ExportProcessType, std::string> EXPORT_PROCESS_MAPPING {
    { ExportProcessType::FULL_PROJECT_AUDIO, muse::trc("export", "Export full project audio") },
    { ExportProcessType::SELECTED_AUDIO, muse::trc("export", "Export selected audio") },
    //! NOTE: not implemented yet
    // { ExportProcessType::AUDIO_IN_LOOP_REGION, muse::trc("export", "Export audio in loop region") },
    // { ExportProcessType::TRACKS_AS_SEPARATE_AUDIO_FILES,
    //   muse::trc("export", "Export tracks as a separate audio files (Stems)") },
    // { ExportProcessType::EACH_LABEL_AS_SEPARATE_AUDIO_FILE, muse::trc("export",
    //                                                                                  "Export each label as a separate audio file (Chapters)") },
    // { ExportProcessType::ALL_LABELS_AS_SUBTITLE_FILE, muse::trc("export", "Export all labels as a subtitle file") }
};

const std::vector<int> DEFAULT_SAMPLE_RATE_LIST {
    8000,
    11025,
    16000,
    22050,
    32000,
    44100,
    48000,
    88200,
    96000,
    176400,
    192000,
    352800,
    384000
};

ExportPreferencesModel::ExportPreferencesModel(QObject* parent)
    : QObject(parent)
{
    //! NOTE: init m_sampleRateMapping
    exportSampleRateList();
}

void ExportPreferencesModel::init()
{
    exportConfiguration()->processTypeChanged().onNotify(this, [this] {
        emit currentProcessChanged();
    });

    m_filename = globalContext()->currentProject()->displayName();
    emit filenameChanged();

    exportConfiguration()->directoryPathChanged().onNotify(this, [this] {
        emit directoryPathChanged();
    });

    exportConfiguration()->currentFormatChanged().onNotify(this, [this] {
        emit currentFormatChanged();
        emit fileExtensionChanged();

        emit exportSampleRateListChanged();
        emit maxExportChannelsChanged();
        updateCurrentSampleRate();
        updateExportChannels();
    });
    if (exportConfiguration()->currentFormat().empty()) {
        const std::vector<std::string> formats = exporter()->formatsList();
        if (!formats.empty()) {
            setCurrentFormat(QString::fromStdString(formats.front()));
        }
    }

    exportConfiguration()->exportChannelsChanged().onNotify(this, [this] {
        emit exportChannelsChanged();
    });

    exportConfiguration()->exportSampleRateChanged().onNotify(this, [this](){
        emit exportSampleRateChanged();
    });
}

QString ExportPreferencesModel::currentProcess() const
{
    return QString::fromStdString(EXPORT_PROCESS_MAPPING[exportConfiguration()->processType()]);
}

void ExportPreferencesModel::setCurrentProcess(const QString& newProcess)
{
    ExportProcessType type;
    for (auto process : EXPORT_PROCESS_MAPPING) {
        if (newProcess == QString::fromStdString(process.second)) {
            type = process.first;
        }
    }

    if (newProcess == currentProcess()) {
        return;
    }

    exportConfiguration()->setProcessType(type);
}

QVariantList ExportPreferencesModel::processList() const
{
    QVariantList result;
    for (const auto& process : EXPORT_PROCESS_MAPPING) {
        result << QString::fromStdString(process.second);
    }

    return result;
}

QString ExportPreferencesModel::filename() const
{
    return m_filename;
}

void ExportPreferencesModel::setFilename(const QString& filename)
{
    if (m_filename == filename) {
        return;
    }

    m_filename = filename;
    emit filenameChanged();
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
    std::string currentFormat = exportConfiguration()->currentFormat();

    if (!currentFormat.empty()) {
        return QString::fromStdString(currentFormat);
    } else {
        if (!exporter()->formatsList().empty()) {
            return QString::fromStdString(exporter()->formatsList().at(0));
        }
    }

    return {};
}

void ExportPreferencesModel::setCurrentFormat(const QString& format)
{
    if (format == QString::fromStdString(exportConfiguration()->currentFormat())) {
        return;
    }

    exportConfiguration()->setCurrentFormat(format.toStdString());
    emit customFFmpegOptionsVisibleChanged();
}

QVariantList ExportPreferencesModel::formatsList() const
{
    QVariantList result;
    for (const auto& format : exporter()->formatsList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

ExportChannelsPref::ExportChannels ExportPreferencesModel::exportChannels() const
{
    return ExportChannelsPref::ExportChannels(exportConfiguration()->exportChannels());
}

void ExportPreferencesModel::setExportChannels(ExportChannelsPref::ExportChannels exportChannels)
{
    exportConfiguration()->setExportChannels(static_cast<int>(exportChannels));
}

int ExportPreferencesModel::maxExportChannels() const
{
    return exporter()->maxChannels();
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
    std::vector<int> sampleRateList = exporter()->sampleRateList();
    if (sampleRateList.empty()) {
        sampleRateList = DEFAULT_SAMPLE_RATE_LIST;
    }

    QVariantList result;
    m_sampleRateMapping.clear();
    for (const auto& rate : sampleRateList) {
        QString sampleRateName = QString::number(rate) + " Hz";
        m_sampleRateMapping.push_back(std::make_pair(rate, sampleRateName));
        result << QVariant::fromValue(sampleRateName);
    }

    return result;
}

void ExportPreferencesModel::setExportSampleRate(const QString& rateName)
{
    if (rateName == exportSampleRate()) {
        return;
    }

    auto it = std::find_if(m_sampleRateMapping.begin(), m_sampleRateMapping.end(),
                           [&rateName](const auto& rate) { return rateName == rate.second; });
    if (it != m_sampleRateMapping.end()) {
        exportConfiguration()->setExportSampleRate(it->first);
        emit exportSampleRateChanged();
        return;
    }
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

void ExportPreferencesModel::setExportSampleFormat(const QString& format)
{
    Q_UNUSED(format);
    NOT_IMPLEMENTED;
}

void ExportPreferencesModel::openCustomFFmpegDialog()
{
    dispatcher()->dispatch("open-custom-ffmpeg-options");
}

void ExportPreferencesModel::updateCurrentSampleRate()
{
    int currentSampleRate = exportConfiguration()->exportSampleRate();
    std::vector<int> sampleRateList = exporter()->sampleRateList();
    if (muse::contains(sampleRateList, currentSampleRate)) {
        return;
    }

    //! NOTE: if current sample rate is not found within format's available sample rates
    //! get the first one available
    QVariantList stringSampleRateList = exportSampleRateList();
    if (stringSampleRateList.empty()) {
        return;
    }
    setExportSampleRate(stringSampleRateList[0].toString());
}

void ExportPreferencesModel::updateExportChannels()
{
    int maxChannels = exporter()->maxChannels();

    if (static_cast<int>(exportChannels()) > maxChannels) {
        setExportChannels(ExportChannelsPref::ExportChannels(maxChannels));
    }
}

bool ExportPreferencesModel::verifyExportPossible()
{
    muse::Ret directoryExists = fileSystem()->makePath(directoryPath());
    if (!directoryExists || directoryPath().isEmpty()) {
        interactive()->error(muse::trc("export", "Export Audio"), muse::trc("export", "Unable to create destination folder"));
        return false;
    }

    return true;
}

void ExportPreferencesModel::exportData()
{
    muse::Ret result = exporter()->exportData(filename().toStdString());
    if (!result.success() && !result.text().empty()) {
        interactive()->error(muse::trc("export", "Export error"), result.text());
    }
}

bool ExportPreferencesModel::customFFmpegOptionsVisible()
{
    return exporter()->isCustomFFmpegExportFormat();
}

int ExportPreferencesModel::optionsCount()
{
    return exporter()->optionsCount();
}
