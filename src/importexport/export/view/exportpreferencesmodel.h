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
#include "context/iglobalcontext.h"
#include "iexportconfiguration.h"
#include "iexporter.h"

namespace au::importexport {
class ExportPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<muse::io::IFileSystem> fileSystem;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IExportConfiguration> exportConfiguration;
    muse::Inject<IExporter> exporter;

    Q_PROPERTY(QString currentProcess READ currentProcess NOTIFY currentProcessChanged)
    Q_PROPERTY(QVariantList processList READ processList NOTIFY processListChanged)

    Q_PROPERTY(QString filename READ filename NOTIFY filenameChanged)
    Q_PROPERTY(QString fileExtension READ fileExtension NOTIFY fileExtensionChanged)

    Q_PROPERTY(QString directoryPath READ directoryPath NOTIFY directoryPathChanged)

    Q_PROPERTY(QString currentFormat READ currentFormat NOTIFY currentFormatChanged)
    Q_PROPERTY(QVariantList formatsList READ formatsList NOTIFY formatsListChanged)

    Q_PROPERTY(importexport::ExportChannelsPref::ExportChannels exportChannels READ exportChannels NOTIFY exportChannelsChanged)
    Q_PROPERTY(int maxExportChannels READ maxExportChannels NOTIFY maxExportChannelsChanged)
    // TODO: add custom mapping as a separate property

    Q_PROPERTY(QString exportSampleRate READ exportSampleRate NOTIFY exportSampleRateChanged)
    Q_PROPERTY(QVariantList exportSampleRateList READ exportSampleRateList NOTIFY exportSampleRateListChanged)

    Q_PROPERTY(QString exportSampleFormat READ exportSampleFormat NOTIFY exportSampleFormatChanged)
    Q_PROPERTY(QVariantList exportSampleFormatList READ exportSampleFormatList NOTIFY exportSampleFormatListChanged)

    // dynamic inputs section
    Q_PROPERTY(bool customFFmpegOptionsVisible READ customFFmpegOptionsVisible NOTIFY customFFmpegOptionsVisibleChanged)
    Q_PROPERTY(int optionsCount READ optionsCount NOTIFY optionsCountChanged)

public:
    explicit ExportPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    QString currentProcess() const;
    Q_INVOKABLE void setCurrentProcess(const QString& process);
    QVariantList processList() const;

    QString filename() const;
    Q_INVOKABLE void setFilename(const QString& filename);

    QString fileExtension() const;

    QString directoryPath() const;
    Q_INVOKABLE void setDirectoryPath(const QString& path);

    QString currentFormat() const;
    Q_INVOKABLE void setCurrentFormat(const QString& format);
    QVariantList formatsList() const;

    importexport::ExportChannelsPref::ExportChannels exportChannels() const;
    Q_INVOKABLE void setExportChannels(importexport::ExportChannelsPref::ExportChannels exportChannels);
    Q_INVOKABLE int maxExportChannels() const;

    QString exportSampleRate() const;
    QVariantList exportSampleRateList();
    Q_INVOKABLE void setExportSampleRate(const QString& rate);

    QString exportSampleFormat() const;
    QVariantList exportSampleFormatList() const;
    Q_INVOKABLE void setExportSampleFormat(const QString& format);

    Q_INVOKABLE void openCustomFFmpegDialog();
    Q_INVOKABLE bool verifyExportPossible();
    Q_INVOKABLE void exportData();

    // dynamic inputs
    bool customFFmpegOptionsVisible();
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
    void exportSampleFormatChanged();
    void exportSampleFormatListChanged();

    void customFFmpegOptionsVisibleChanged();
    void optionsCountChanged();
    void optionTitleListChanged();

private:
    void updateCurrentSampleRate();
    void updateExportChannels();

    QString m_filename;
    std::vector<std::pair<int, QString> > m_sampleRateMapping;
};
}
