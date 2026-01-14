/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "iinteractive.h"
#include "io/ifilesystem.h"
#include "appshell/iappshellconfiguration.h"
#include "context/iglobalcontext.h"
#include "iexportconfiguration.h"
#include "iexporter.h"
#include "playback/iplaybackcontroller.h"
#include "trackedit/iselectioncontroller.h"

namespace au::importexport {
class ExportPreferencesModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<appshell::IAppShellConfiguration> configuration;
    muse::GlobalInject<IExportConfiguration> exportConfiguration;

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<context::IGlobalContext> globalContext{ this };
    muse::Inject<IExporter> exporter{ this };
    muse::Inject<au::playback::IPlaybackController> playbackController{ this };
    muse::Inject<trackedit::ISelectionController> selectionController{ this };

    Q_PROPERTY(QString currentProcess READ currentProcess NOTIFY currentProcessChanged)
    Q_PROPERTY(QVariantList processList READ processList NOTIFY processListChanged)

    Q_PROPERTY(QString filename READ filename NOTIFY filenameChanged)

    Q_PROPERTY(QString directoryPath READ directoryPath NOTIFY directoryPathChanged)

    Q_PROPERTY(QString currentFormat READ currentFormat NOTIFY currentFormatChanged)
    Q_PROPERTY(QStringList formatsList READ formatsList NOTIFY formatsListChanged)

    Q_PROPERTY(importexport::ExportChannelsPref::ExportChannels exportChannels READ exportChannels NOTIFY exportChannelsChanged)
    Q_PROPERTY(int maxExportChannels READ maxExportChannels NOTIFY maxExportChannelsChanged)
    // TODO: add custom mapping as a separate property

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

    QString filename() const;
    Q_INVOKABLE void setFilename(const QString& filename);

    QString directoryPath() const;
    Q_INVOKABLE void setDirectoryPath(const QString& path);

    QString currentFormat() const;
    Q_INVOKABLE void setCurrentFormat(const QString& format);
    QStringList formatsList() const;

    importexport::ExportChannelsPref::ExportChannels exportChannels() const;
    Q_INVOKABLE void setExportChannels(importexport::ExportChannelsPref::ExportChannels exportChannels);
    Q_INVOKABLE int maxExportChannels() const;

    QString exportSampleRate() const;
    QVariantList exportSampleRateList();
    Q_INVOKABLE void setExportSampleRate(const QString& rate);

    Q_INVOKABLE void openCustomFFmpegDialog();
    Q_INVOKABLE void openMetadataDialog();
    Q_INVOKABLE void setFilePickerPath(const QString& path);
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

signals:
    void currentProcessChanged();
    void processListChanged();
    void filenameChanged();
    void fileExtensionChanged();
    void directoryPathChanged();
    void currentFormatChanged();
    void formatsListChanged();
    void exportChannelsChanged();
    void maxExportChannelsChanged();
    void exportSampleRateChanged();
    void exportSampleRateListChanged();

    void customFFmpegOptionsVisibleChanged();
    void oggFormatOptionsVisibleChanged();
    void hasMetadataChanged();
    void optionsCountChanged();
    void optionTitleListChanged();

private:
    void updateCurrentSampleRate();
    void updateExportChannels();

    QString m_filename;
    std::vector<std::pair<int, QString> > m_sampleRateMapping;
    bool m_resetSampleRate = true;
};
}
