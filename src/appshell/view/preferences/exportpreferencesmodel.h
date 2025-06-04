/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "importexport/export/iexporter.h"
#include "importexport/export/internal/exportconfiguration.h"
#include "playback/iaudiodevicesprovider.h"

using namespace au::importexport;

namespace au::appshell {
class ExportPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<au::importexport::ExportConfiguration> exportConfiguration;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<au::importexport::IExporter> exporter;

    Q_PROPERTY(QString currentProcess READ currentProcess NOTIFY currentProcessChanged)
    Q_PROPERTY(QVariantList processList READ processList NOTIFY processListChanged)

    Q_PROPERTY(QString filename READ filename NOTIFY filenameChanged)
    Q_PROPERTY(QString fileExtension READ fileExtension NOTIFY fileExtensionChanged)

    Q_PROPERTY(QString directoryPath READ directoryPath NOTIFY directoryPathChanged)

    Q_PROPERTY(QString currentFormat READ currentFormat NOTIFY currentFormatChanged)
    Q_PROPERTY(QVariantList formatList READ formatList NOTIFY formatListChanged)

    Q_PROPERTY(ExportChannelsPref::ExportChannels exportChannels READ exportChannels NOTIFY exportChannelsChanged)
    // TODO: add custom mapping as a separate property

    Q_PROPERTY(QString exportSampleRate READ exportSampleRate NOTIFY exportSampleRateChanged)
    Q_PROPERTY(QVariantList exportSampleRateList READ exportSampleRateList NOTIFY exportSampleRateListChanged)

    Q_PROPERTY(QString exportSampleFormat READ exportSampleFormat NOTIFY exportSampleFormatChanged)
    Q_PROPERTY(QVariantList exportSampleFormatList READ exportSampleFormatList NOTIFY exportSampleFormatListChanged)

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
    QVariantList formatList() const;

    ExportChannelsPref::ExportChannels exportChannels() const;
    Q_INVOKABLE void setExportChannels(ExportChannelsPref::ExportChannels exportChannels);

    QString exportSampleRate() const;
    QVariantList exportSampleRateList();
    Q_INVOKABLE void exportSampleRateSelected(const QString& rate);

    QString exportSampleFormat() const;
    QVariantList exportSampleFormatList() const;
    Q_INVOKABLE void exportSampleFormatSelected(const QString& format);

signals:
    void currentProcessChanged();
    void processListChanged();
    void filenameChanged();
    void fileExtensionChanged();
    void directoryPathChanged();
    void currentFormatChanged();
    void formatListChanged();
    void exportChannelsChanged();
    void exportSampleRateChanged();
    void exportSampleRateListChanged();
    void exportSampleFormatChanged();
    void exportSampleFormatListChanged();

private:
    std::vector<std::pair<uint64_t, QString> > m_sampleRateMapping;
};
}
