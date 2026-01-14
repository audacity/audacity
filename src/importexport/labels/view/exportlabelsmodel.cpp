/*
 * Audacity: A Digital Audio Editor
 */

#include "exportlabelsmodel.h"

#include "framework/global/translation.h"

#include "trackedit/trackedittypes.h"

#include "labelsutils.h"

using namespace au::importexport;

ExportLabelsModel::ExportLabelsModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void ExportLabelsModel::init(const QVariant& trackId)
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    QVariantList labelTracks;

    for (const trackedit::Track& track : project->trackList()) {
        if (track.type != trackedit::TrackType::Label) {
            continue;
        }

        QVariantMap labelTrackMap;
        labelTrackMap["itemId"] = QVariant::fromValue(track.id);
        labelTrackMap["title"] = track.title.toQString();

        labelTracks << labelTrackMap;
    }

    trackedit::TrackId _trackId = trackId.toInt();
    if (_trackId != -1) {
        setSelectedTracks({ trackId });
    } else {
        selectAllTracks();
    }

    setLabelTracks(labelTracks);

    setDirectoryPath(configuration()->labelsDirectoryPath().toQString());

    emit currentFileTypeChanged();
}

void ExportLabelsModel::exportData()
{
    muse::io::path_t filePath = m_directoryPath.toStdString() + "/" + m_fileName.toStdString() + "."
                                + fileSuffixFromType(m_currentFileType);

    trackedit::TrackIdList selectedTracksIds;
    for (const QVariant& trackId : m_selectedTracks) {
        selectedTracksIds.push_back(trackId.toInt());
    }

    muse::Ret ret = labelExporter()->exportData(filePath, selectedTracksIds);
    if (!ret) {
        LOGE() << ret.toString();
        return;
    }

    configuration()->setLabelsDirectoryPath(m_directoryPath.toStdString());

    interactive()->revealInFileBrowser(filePath);
}

QVariantList ExportLabelsModel::labelTracks() const
{
    return m_labelTracks;
}

void ExportLabelsModel::setLabelTracks(const QVariantList& tracks)
{
    if (m_labelTracks == tracks) {
        return;
    }

    m_labelTracks = tracks;
    emit labelTracksChanged();
}

QVariantList ExportLabelsModel::selectedTracks() const
{
    return m_selectedTracks;
}

void ExportLabelsModel::setSelectedTracks(const QVariantList& selectedTracks)
{
    if (m_selectedTracks == selectedTracks) {
        return;
    }

    m_selectedTracks = selectedTracks;
    emit selectedTracksChanged();
}

void ExportLabelsModel::changeSelectionForTrack(const QVariant& trackId, bool select)
{
    QVariantList tracks = m_selectedTracks;
    if (!select) {
        tracks.removeAll(trackId);
    } else {
        tracks << trackId;
    }

    setSelectedTracks(tracks);
}

void ExportLabelsModel::selectAllTracks()
{
    QVariantList tracks;
    for (const QVariant& track : m_labelTracks) {
        tracks << track.toMap()["itemId"];
    }
    setSelectedTracks(tracks);
}

void ExportLabelsModel::deselectAllTracks()
{
    setSelectedTracks({});
}

QVariantList ExportLabelsModel::fileTypes() const
{
    QVariantList result;

    for (const FileFilter& filter : configuration()->fileFilter()) {
        result << QVariantMap {
            { "title", QString::fromStdString(filter.title) }, { "type", static_cast<int>(filter.type) }
            };
    }

    return result;
}

QString ExportLabelsModel::fileName() const
{
    return m_fileName;
}

void ExportLabelsModel::setFileName(const QString& fileName)
{
    if (m_fileName == fileName) {
        return;
    }

    m_fileName = fileName;
    emit fileNameChanged();
}

QString ExportLabelsModel::directoryPath() const
{
    return m_directoryPath;
}

void ExportLabelsModel::setDirectoryPath(const QString& path)
{
    if (m_directoryPath == path) {
        return;
    }

    m_directoryPath = path;
    emit directoryPathChanged();
}

int ExportLabelsModel::currentFileType() const
{
    return static_cast<int>(m_currentFileType);
}

void ExportLabelsModel::setCurrentFileType(int type)
{
    FileType fileType = static_cast<FileType>(type);
    if (m_currentFileType == fileType) {
        return;
    }

    m_currentFileType = fileType;
    emit currentFileTypeChanged();
}
