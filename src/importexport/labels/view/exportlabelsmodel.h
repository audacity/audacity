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
    Q_PROPERTY(QString currentFileTypeCode READ currentFileTypeCode WRITE setCurrentFileTypeCode NOTIFY currentFileTypeCodeChanged FINAL)

    Q_PROPERTY(QVariantList labelTracks READ labelTracks NOTIFY labelTracksChanged FINAL)
    Q_PROPERTY(QVariantList selectedTracks READ selectedTracks NOTIFY selectedTracksChanged FINAL)

    muse::Inject<muse::IInteractive> interactive = { this };
    muse::Inject<context::IGlobalContext> globalContext = { this };
    muse::Inject<ILabelsExporter> labelExporter = { this };
    muse::Inject<ILabelsConfiguration> configuration = { this };

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

    QString currentFileTypeCode() const;
    void setCurrentFileTypeCode(const QString& typeCode);

    QVariantList fileTypes() const;

signals:
    void fileNameChanged();
    void directoryPathChanged();
    void currentFileTypeCodeChanged();

    void labelTracksChanged();
    void selectedTracksChanged();

private:
    void initDefaultFileType();

    QString m_fileName;
    QString m_currentFileTypeCode;
    QString m_directoryPath;

    QVariantList m_labelTracks;
    QVariantList m_selectedTracks;
};
}
