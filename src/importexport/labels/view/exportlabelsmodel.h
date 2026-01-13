/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "context/iglobalcontext.h"
#include "ilabelsexporter.h"
#include "ilabelsconfiguration.h"

namespace au::importexport {
class ExportLabelsModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(QString fileName READ fileName WRITE setFileName NOTIFY fileNameChanged)
    Q_PROPERTY(QString directoryPath READ directoryPath WRITE setDirectoryPath NOTIFY directoryPathChanged)

    Q_PROPERTY(QVariantList fileTypes READ fileTypes CONSTANT FINAL)
    Q_PROPERTY(int currentFileType READ currentFileType WRITE setCurrentFileType NOTIFY currentFileTypeChanged FINAL)

    Q_PROPERTY(QVariantList labelTracks READ labelTracks NOTIFY labelTracksChanged FINAL)
    Q_PROPERTY(QVariantList selectedTracks READ selectedTracks NOTIFY selectedTracksChanged FINAL)

    muse::GlobalInject<ILabelsConfiguration> configuration;

    muse::Inject<muse::IInteractive> interactive = { this };
    muse::Inject<context::IGlobalContext> globalContext = { this };
    muse::Inject<ILabelsExporter> labelExporter = { this };

public:
    explicit ExportLabelsModel(QObject* parent = nullptr);

    Q_INVOKABLE void init(const QVariant& trackId);
    Q_INVOKABLE void exportData();

    QVariantList labelTracks() const;
    void setLabelTracks(const QVariantList& tracks);

    QVariantList selectedTracks() const;
    void setSelectedTracks(const QVariantList& selectedTracks);

    Q_INVOKABLE void changeSelectionForTrack(const QVariant& trackId, bool select);
    Q_INVOKABLE void selectAllTracks();
    Q_INVOKABLE void deselectAllTracks();

    QString fileName() const;
    void setFileName(const QString& fileName);

    QString directoryPath() const;
    void setDirectoryPath(const QString& path);

    int currentFileType() const;
    void setCurrentFileType(int type);

    QVariantList fileTypes() const;

signals:
    void fileNameChanged();
    void directoryPathChanged();
    void currentFileTypeChanged();

    void labelTracksChanged();
    void selectedTracksChanged();

private:
    QString m_fileName;
    FileType m_currentFileType = FileType::TEXT;
    QString m_directoryPath;

    QVariantList m_labelTracks;
    QVariantList m_selectedTracks;
};
}
