/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "appshell/iappshellconfiguration.h"
#include "importexport/export/iexportconfiguration.h"
#include "context/iglobalcontext.h"
#include "importexport//export/iexporter.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "playback/iplaybackcontroller.h"
#include "trackedit/iselectioncontroller.h"

namespace au::importexport {
class ExportPreferencesModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<appshell::IAppShellConfiguration> configuration;
    muse::GlobalInject<IExportConfiguration> exportConfiguration;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IExporter> exporter{ this };
    muse::ContextInject<effects::IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };

    Q_PROPERTY(QString currentProcess READ currentProcess NOTIFY currentProcessChanged)
    Q_PROPERTY(QVariantList processList READ processList NOTIFY processListChanged)

    Q_PROPERTY(bool trimBlankSpace READ trimBlankSpace WRITE setTrimBlankSpace NOTIFY trimBlankSpaceChanged)
    Q_PROPERTY(bool trimBlankSpaceEnabled READ trimBlankSpaceEnabled NOTIFY trimBlankSpaceEnabledChanged)

    Q_PROPERTY(bool separateFilesExport READ separateFilesExport NOTIFY currentProcessChanged)
    Q_PROPERTY(bool separateFilesByLabels READ separateFilesByLabels NOTIFY currentProcessChanged)
    Q_PROPERTY(QString fileNamePrefix READ fileNamePrefix WRITE setFileNamePrefix NOTIFY fileNamePrefixChanged)
    Q_PROPERTY(bool includeNumbers READ includeNumbers WRITE setIncludeNumbers NOTIFY includeNumbersChanged)
    Q_PROPERTY(
        bool includeAudioBeforeFirstLabel READ includeAudioBeforeFirstLabel WRITE setIncludeAudioBeforeFirstLabel NOTIFY includeAudioBeforeFirstLabelChanged)
    Q_PROPERTY(QString fileNamePreview READ fileNamePreview NOTIFY fileNamePreviewChanged)

    Q_PROPERTY(QString filename READ filename NOTIFY filenameChanged)
    Q_PROPERTY(QString suggestedFilePath READ suggestedFilePath NOTIFY suggestedFilePathChanged)

    Q_PROPERTY(QString directoryPath READ directoryPath NOTIFY directoryPathChanged)

    Q_PROPERTY(QString currentFormat READ currentFormat NOTIFY currentFormatChanged)
    Q_PROPERTY(QStringList formatsList READ formatsList NOTIFY formatsListChanged)

    Q_PROPERTY(importexport::ExportChannelsPref::ExportChannels exportChannelsType READ exportChannelsType NOTIFY exportChannelsTypeChanged)
    Q_PROPERTY(int maxExportChannels READ maxExportChannels NOTIFY maxExportChannelsChanged)

    Q_PROPERTY(QString exportSampleRate READ exportSampleRate NOTIFY exportSampleRateChanged)
    Q_PROPERTY(QVariantList exportSampleRateList READ exportSampleRateList NOTIFY exportSampleRateListChanged)

    // dynamic inputs section
    Q_PROPERTY(bool customFFmpegOptionsVisible READ customFFmpegOptionsVisible NOTIFY customFFmpegOptionsVisibleChanged)
    Q_PROPERTY(bool oggFormatOptionsVisible READ oggFormatOptionsVisible NOTIFY oggFormatOptionsVisibleChanged)
    Q_PROPERTY(bool hasMetadata READ hasMetadata NOTIFY hasMetadataChanged)
    Q_PROPERTY(int optionsCount READ optionsCount NOTIFY optionsCountChanged)

public:
    explicit ExportPreferencesModel(QObject* parent = nullptr);
    ~ExportPreferencesModel();

    Q_INVOKABLE void init();
    Q_INVOKABLE void apply();
    Q_INVOKABLE void cancel();

    QString currentProcess() const;
    Q_INVOKABLE void setCurrentProcess(const QString& process);
    QVariantList processList() const;

    bool trimBlankSpace() const;
    void setTrimBlankSpace(bool trim);
    bool trimBlankSpaceEnabled() const;

    bool separateFilesExport() const;
    bool separateFilesByLabels() const;
    QString fileNamePrefix() const;
    void setFileNamePrefix(const QString& prefix);
    bool includeNumbers() const;
    void setIncludeNumbers(bool include);
    bool includeAudioBeforeFirstLabel() const;
    void setIncludeAudioBeforeFirstLabel(bool include);
    QString fileNamePreview() const;

    QString filename() const;
    Q_INVOKABLE void setFilename(const QString& filename);
    QString suggestedFilePath() const;

    QString directoryPath() const;
    Q_INVOKABLE void setDirectoryPath(const QString& path);

    QString currentFormat() const;
    Q_INVOKABLE void setCurrentFormat(const QString& format);
    QStringList formatsList() const;

    importexport::ExportChannelsPref::ExportChannels exportChannelsType() const;
    Q_INVOKABLE void setExportChannelsType(importexport::ExportChannelsPref::ExportChannels type);
    Q_INVOKABLE int maxExportChannels() const;

    QString exportSampleRate() const;
    QVariantList exportSampleRateList();
    Q_INVOKABLE void setExportSampleRate(const QString& rate);

    Q_INVOKABLE void openCustomFFmpegDialog();
    Q_INVOKABLE void openMetadataDialog();
    Q_INVOKABLE void openCustomMappingDialog();
    Q_INVOKABLE void setFilePickerPath(const QString& path);
    Q_INVOKABLE void setFileDialogPath(const QString& path);
    Q_INVOKABLE bool verifyExportPossible();
    Q_INVOKABLE QStringList fileFilter();
    QStringList formatExtensions(const QString& format) const;
    QStringList supportedExtensionsList() const;
    Q_INVOKABLE void exportData();

    // dynamic inputs
    bool customFFmpegOptionsVisible();
    bool oggFormatOptionsVisible();
    bool hasMetadata();
    int optionsCount();

    bool needToDisableMasterFxBeforeExport() const;
    void enableMasterFx() const;
    bool masterFxEnabled() const;
    bool customMappingEnabled() const;
    muse::Ret warnAndDisableMasterFxBeforeExport() const;

signals:
    void currentProcessChanged();
    void processListChanged();
    void trimBlankSpaceChanged();
    void trimBlankSpaceEnabledChanged();
    void fileNamePrefixChanged();
    void includeNumbersChanged();
    void includeAudioBeforeFirstLabelChanged();
    void fileNamePreviewChanged();
    void filenameChanged();
    void suggestedFilePathChanged();
    void fileExtensionChanged();
    void directoryPathChanged();
    void currentFormatChanged();
    void formatsListChanged();
    void exportChannelsTypeChanged();
    void maxExportChannelsChanged();
    void exportSampleRateChanged();
    void exportSampleRateListChanged();

    void customFFmpegOptionsVisibleChanged();
    void oggFormatOptionsVisibleChanged();
    void hasMetadataChanged();
    void optionsCountChanged();
    void optionTitleListChanged();

    void exportCompleted();

private:
    void updateCurrentSampleRate();
    void openCustomSampleRateDialog();
    void updateExportChannels();

    bool hasLabels() const;
    muse::Ret exportSingleFile();
    muse::Ret exportSeparateFiles();
    bool confirmOverwrite(const std::string& question);
    IExporter::Options separateFilesOptions() const;

    QString m_filename;
    QString m_fileNamePrefix;
    std::vector<std::pair<int, QString> > m_sampleRateMapping;
    bool m_resetSampleRate = true;
};
}
