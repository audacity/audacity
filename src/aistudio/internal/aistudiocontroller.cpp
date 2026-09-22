/*
 * Audacity: A Digital Audio Editor
 */
#include "aistudiocontroller.h"

#include <QObject>

#include "aijobs/runtimehostsupervisor.h"
#include "ailibrary/globalassetcatalogue.h"
#include "ailibrary/libraryassetstore.h"
#include "aiproject/aiworkspace.h"
#include "aistudio/view/aistudiostatusmodel.h"

#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QtEndian>
#include <QCryptographicHash>
#include <QFile>
#include <QFileInfo>
#include <QUuid>
#include <QVariantMap>
#include <QUrl>

#include <algorithm>

using namespace au::aistudio;
using namespace muse;
using namespace muse::actions;

static const ActionCode OPEN_JOBS_CODE("ai.openJobs");
static const QString AI_STUDIO_DOCK("aiStudioPanel");

namespace {
struct WavMetadata {
    double durationSeconds = 0.0;
    int sampleRate = 0;
    int channels = 0;
};

WavMetadata wavMetadata(const QString& filePath)
{
    QFile file(filePath);
    if (!file.open(QIODevice::ReadOnly) || file.read(4) != "RIFF") {
        return {};
    }
    if (file.read(4).size() != 4 || file.read(4) != "WAVE") {
        return {};
    }

    quint16 channels = 0;
    quint16 blockAlign = 0;
    quint32 sampleRate = 0;
    quint32 dataBytes = 0;
    bool hasFormat = false;
    bool hasData = false;
    while (file.pos() + 8 <= file.size()) {
        const QByteArray chunkId = file.read(4);
        const QByteArray chunkSizeBytes = file.read(4);
        if (chunkId.size() != 4 || chunkSizeBytes.size() != 4) {
            break;
        }
        const quint32 chunkSize = qFromLittleEndian<quint32>(
            reinterpret_cast<const uchar*>(chunkSizeBytes.constData()));
        if (chunkId == "fmt ") {
            const QByteArray format = file.read(std::min<quint32>(chunkSize, 16));
            if (format.size() >= 16) {
                channels = qFromLittleEndian<quint16>(reinterpret_cast<const uchar*>(format.constData() + 2));
                sampleRate = qFromLittleEndian<quint32>(reinterpret_cast<const uchar*>(format.constData() + 4));
                blockAlign = qFromLittleEndian<quint16>(reinterpret_cast<const uchar*>(format.constData() + 12));
                hasFormat = channels > 0 && sampleRate > 0 && blockAlign > 0;
            }
        } else if (chunkId == "data") {
            dataBytes = chunkSize;
            hasData = true;
        }
        const qint64 nextChunk = file.pos() + qint64(chunkSize - (chunkId == "fmt " ? std::min<quint32>(chunkSize, 16) : 0))
                                + (chunkSize % 2);
        if (!file.seek(nextChunk)) {
            break;
        }
        if (hasFormat && hasData) {
            break;
        }
    }
    if (!hasFormat || !hasData) {
        return {};
    }
    return { double(dataBytes) / (double(sampleRate) * double(blockAlign)), int(sampleRate), int(channels) };
}
}

void AIStudioController::init()
{
    m_runtimeHost = std::make_shared<au::aijobs::RuntimeHostSupervisor>();
    AIStudioStatusModel::instance()->setRuntimeStatus(m_runtimeHost->statusText());
    QObject::connect(m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::statusChanged,
                     AIStudioStatusModel::instance(), &AIStudioStatusModel::setRuntimeStatus);
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::testJobRequested,
                     m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::submitTestJob);
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::testJobCancelRequested,
                     m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::cancelTestJob);
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::testJobInsertRequested,
                     m_runtimeHost.get(), [this] { insertTestJobOutput(); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::workerFailureTestRequested,
                     m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::submitFailureTest);
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::workspaceEnableRequested,
                     m_runtimeHost.get(), [this] { enableProjectWorkspace(); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryRefreshRequested,
                     m_runtimeHost.get(), [this] { refreshWorkspaceStatus(); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryImportRequested,
                     m_runtimeHost.get(), [this](const QString& sourcePath) { importLocalWav(sourcePath); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetFavouriteRequested,
                     m_runtimeHost.get(), [this](const QString& assetId, bool favourite) {
        setLibraryAssetFavourite(assetId, favourite);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetsFavouriteRequested,
                     m_runtimeHost.get(), [this](const QStringList& assetIds, bool favourite) {
        setLibraryAssetsFavourite(assetIds, favourite);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetsMoveRequested,
                     m_runtimeHost.get(), [this](const QStringList& assetIds, const QString& folder) {
        moveLibraryAssetsToFolder(assetIds, folder);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetsUnfileRequested,
                     m_runtimeHost.get(), [this](const QStringList& assetIds) { moveLibraryAssetsToUnfiled(assetIds); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryFolderCreateRequested,
                     m_runtimeHost.get(), [this](const QString& folder) { createLibraryFolder(folder); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryFolderRenameRequested,
                     m_runtimeHost.get(), [this](const QString& folder, const QString& newFolder) {
        renameLibraryFolder(folder, newFolder);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryFolderDeleteRequested,
                     m_runtimeHost.get(), [this](const QString& folder) { deleteLibraryFolder(folder); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetRenameRequested,
                     m_runtimeHost.get(), [this](const QString& assetId, const QString& name) {
        renameLibraryAsset(assetId, name);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetDeleteRequested,
                     m_runtimeHost.get(), [this](const QString& assetId) { deleteLibraryAsset(assetId); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetTagsRequested,
                     m_runtimeHost.get(), [this](const QString& assetId, const QString& tags) {
        setLibraryAssetTags(assetId, tags);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetsTagsRequested,
                     m_runtimeHost.get(), [this](const QStringList& assetIds, const QString& tags) {
        setLibraryAssetsTags(assetIds, tags);
    });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetAudioDetailsRequested,
                     m_runtimeHost.get(), [this](const QString& assetId) { readLibraryAssetAudioDetails(assetId); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetsAudioDetailsRequested,
                     m_runtimeHost.get(), [this](const QStringList& assetIds) { readLibraryAssetsAudioDetails(assetIds); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetRevealRequested,
                     m_runtimeHost.get(), [this](const QString& assetId) { revealLibraryAssetInExplorer(assetId); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::libraryAssetInsertionRequested,
                     m_runtimeHost.get(), [this](const QString& assetId) { addLibraryAssetToTimeline(assetId); });
    QObject::connect(AIStudioStatusModel::instance(), &AIStudioStatusModel::globalLibraryAssetCopyRequested,
                     m_runtimeHost.get(), [this](const QString& projectPath, const QString& assetId) {
        copyGlobalLibraryAssetToProject(projectPath, assetId);
    });
    QObject::connect(m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::testJobCompleted,
                     m_runtimeHost.get(), [this](const QString& jobId, const QString& resultManifest) {
        recordCompletedJob(jobId, resultManifest);
    });
    QObject::connect(m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::testJobAccepted,
                     m_runtimeHost.get(), [this](const QString& jobId) { recordJobState(jobId, "running"); });
    QObject::connect(m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::testJobCancelled,
                     m_runtimeHost.get(), [this](const QString& jobId) { recordJobState(jobId, "cancelled"); });
    QObject::connect(m_runtimeHost.get(), &au::aijobs::RuntimeHostSupervisor::testJobFailed,
                     m_runtimeHost.get(), [this](const QString& jobId) { recordJobState(jobId, "failed"); });
    dispatcher()->reg(this, OPEN_JOBS_CODE, this, &AIStudioController::openJobs);
}

bool AIStudioController::canReceiveAction(const ActionCode& code) const
{
    return code == OPEN_JOBS_CODE;
}

void AIStudioController::openJobs()
{
    refreshWorkspaceStatus();
    if (m_activeWorkspace.isEmpty()) {
        m_runtimeHost->start();
    } else {
        m_runtimeHost->restartInWorkspace(m_activeWorkspace);
    }
    dispatcher()->dispatch("dock-set-open", ActionData::make_arg2<QString, bool>(AI_STUDIO_DOCK, true));
}

void AIStudioController::enableProjectWorkspace()
{
    const auto project = globalContext()->currentProject();
    const QString projectPath = project ? project->path().toQString() : QString();
    QString error;
    if (!au::aiproject::WorkspaceStore::create(projectPath, &error)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }

    const QString workspace = au::aiproject::WorkspaceStore::workspacePathForProject(projectPath);
    m_activeWorkspace = workspace;
    AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("AI workspace enabled: %1").arg(workspace));
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Library ready for project imports"));
    refreshLibraryAssets();
    m_runtimeHost->restartInWorkspace(workspace);
}

void AIStudioController::refreshWorkspaceStatus()
{
    const auto project = globalContext()->currentProject();
    const QString projectPath = project ? project->path().toQString() : QString();
    if (projectPath.isEmpty()) {
        m_activeWorkspace.clear();
        AIStudioStatusModel::instance()->setLibraryAssets({});
        AIStudioStatusModel::instance()->setLibraryFolders({});
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Save the project before enabling its AI workspace"));
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("No project Library is available"));
    } else if (au::aiproject::WorkspaceStore::isEnabled(projectPath)) {
        m_activeWorkspace = au::aiproject::WorkspaceStore::workspacePathForProject(projectPath);
        QString error;
        const int recovered = au::aiproject::WorkspaceStore::recoverInterruptedJobs(m_activeWorkspace, &error);
        if (recovered > 0) {
            AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Recovered %1 interrupted AI job(s)").arg(recovered));
        } else if (recovered < 0) {
            AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        } else {
            AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("AI workspace enabled: %1")
                                                                 .arg(m_activeWorkspace));
        }
        refreshLibraryAssets();
    } else {
        m_activeWorkspace.clear();
        AIStudioStatusModel::instance()->setLibraryAssets({});
        AIStudioStatusModel::instance()->setLibraryFolders({});
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("AI workspace not enabled for this project"));
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace to use its Library"));
    }
}

void AIStudioController::refreshLibraryAssets()
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryAssets({});
        AIStudioStatusModel::instance()->setLibraryFolders({});
        return;
    }

    QString error;
    const QList<au::ailibrary::ProjectAsset> assets = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        return;
    }
    const QStringList folders = au::ailibrary::LibraryAssetStore::projectFolders(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        return;
    }

    const auto project = globalContext()->currentProject();
    const QString projectPath = project ? project->path().toQString() : QString();
    QString catalogueError;
    if (!projectPath.isEmpty()
        && !au::ailibrary::GlobalAssetCatalogue::syncProject(projectPath, m_activeWorkspace, assets, &catalogueError)) {
        // A project manifest is portable and authoritative. A local catalogue
        // failure must not hide or block the current project's Library.
        AIStudioStatusModel::instance()->setLibraryStatus(catalogueError);
    }

    catalogueError.clear();
    const QList<au::ailibrary::GlobalAssetRecord> globalAssets
        = au::ailibrary::GlobalAssetCatalogue::allAssets(&catalogueError);
    if (!catalogueError.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(catalogueError);
    }
    QVariantList globalRows;
    for (const au::ailibrary::GlobalAssetRecord& record : globalAssets) {
        QFileInfo assetFile(record.asset.filePath);
        if (assetFile.isRelative()) {
            assetFile.setFile(QDir(record.workspacePath).filePath(record.asset.filePath));
        }
        const QString availability = !record.asset.filePath.isEmpty() && !assetFile.exists()
                                     ? QStringLiteral("missing") : record.asset.status;
        globalRows.append(QVariantMap {
            { "id", record.asset.id }, { "name", record.asset.name }, { "kind", record.asset.kind },
            { "origin", record.asset.origin }, { "status", availability }, { "filePath", record.asset.filePath },
            { "createdAt", record.asset.createdAt }, { "durationSeconds", record.asset.durationSeconds },
            { "sampleRate", record.asset.sampleRate }, { "channels", record.asset.channels },
            { "favourite", record.asset.favourite }, { "folder", record.asset.folder }, { "tags", record.asset.tags },
            { "provenanceId", record.asset.provenanceId }, { "sourceAssetIds", record.asset.sourceAssetIds },
            { "projectPath", record.projectPath }, { "isCurrentProject", record.projectPath == projectPath }
        });
    }
    AIStudioStatusModel::instance()->setGlobalLibraryAssets(globalRows);

    QVariantList rows;
    for (const au::ailibrary::ProjectAsset& asset : assets) {
        QFileInfo assetFile(asset.filePath);
        if (assetFile.isRelative()) {
            assetFile.setFile(QDir(m_activeWorkspace).filePath(asset.filePath));
        }
        const QString availability = !asset.filePath.isEmpty() && !assetFile.exists()
                                     ? QStringLiteral("missing") : asset.status;
        rows.append(QVariantMap {
            { "id", asset.id }, { "name", asset.name }, { "kind", asset.kind },
            { "origin", asset.origin }, { "status", availability }, { "filePath", asset.filePath },
            { "createdAt", asset.createdAt }, { "durationSeconds", asset.durationSeconds },
            { "sampleRate", asset.sampleRate }, { "channels", asset.channels }, { "favourite", asset.favourite }
            , { "folder", asset.folder }, { "tags", asset.tags }, { "provenanceId", asset.provenanceId }
            , { "sourceAssetIds", asset.sourceAssetIds }
        });
    }
    AIStudioStatusModel::instance()->setLibraryAssets(rows);
    AIStudioStatusModel::instance()->setLibraryFolders(folders);
}

void AIStudioController::copyGlobalLibraryAssetToProject(const QString& projectPath, const QString& assetId)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Open and enable a project Library before copying an archived asset"));
        return;
    }
    QString error;
    const QList<au::ailibrary::GlobalAssetRecord> records
        = au::ailibrary::GlobalAssetCatalogue::allAssets(&error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    const auto record = std::find_if(records.cbegin(), records.cend(), [&projectPath, &assetId](const auto& candidate) {
        return candidate.projectPath == projectPath && candidate.asset.id == assetId;
    });
    if (record == records.cend()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("The archived Library asset is no longer indexed"));
        return;
    }
    QFileInfo source(record->asset.filePath);
    if (source.isRelative()) {
        source.setFile(QDir(record->workspacePath).filePath(record->asset.filePath));
    }
    if (!source.exists() || !source.isFile()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("The archived Library audio file is missing"));
        return;
    }
    importLocalWav(source.filePath());
}

void AIStudioController::importLocalWav(const QString& sourcePath)
{
    if (m_activeWorkspace.isEmpty()) {
        const QString message = QObject::tr("Enable the project AI workspace before importing audio");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }
    const QFileInfo source(sourcePath);
    if (!source.exists() || !source.isFile() || source.suffix().compare("wav", Qt::CaseInsensitive) != 0) {
        const QString message = QObject::tr("Choose an existing WAV file to import into the AI Library");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }

    QFile input(source.filePath());
    if (!input.open(QIODevice::ReadOnly)) {
        const QString message = QObject::tr("Could not read the WAV file for Library import");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }
    QCryptographicHash hash(QCryptographicHash::Sha256);
    if (!hash.addData(&input)) {
        const QString message = QObject::tr("Could not checksum the WAV file for Library import");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }
    const QString checksum = QString::fromLatin1(hash.result().toHex());
    QString lookupError;
    const QList<au::ailibrary::ProjectAsset> existingAssets
        = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &lookupError);
    if (!lookupError.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(lookupError);
        AIStudioStatusModel::instance()->setLibraryStatus(lookupError);
        return;
    }
    const auto duplicate = std::find_if(existingAssets.cbegin(), existingAssets.cend(), [&checksum](const auto& asset) {
        return !asset.contentChecksum.isEmpty() && asset.contentChecksum == checksum;
    });

    const QString relativeDirectory = "assets/imported";
    const QDir workspace(m_activeWorkspace);
    if (!workspace.mkpath(relativeDirectory)) {
        const QString message = QObject::tr("Could not create the AI Library import folder");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }
    const QString assetId = QUuid::createUuid().toString(QUuid::WithoutBraces);
    const QString relativePath = relativeDirectory + "/" + assetId + "-" + source.fileName();
    const QString destinationPath = workspace.filePath(relativePath);
    if (!QFile::copy(source.filePath(), destinationPath)) {
        const QString message = QObject::tr("Could not copy the WAV file into the AI Library");
        AIStudioStatusModel::instance()->setWorkspaceStatus(message);
        AIStudioStatusModel::instance()->setLibraryStatus(message);
        return;
    }

    au::ailibrary::ProjectAsset asset;
    asset.id = assetId;
    asset.name = source.completeBaseName();
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.filePath = relativePath;
    const WavMetadata metadata = wavMetadata(source.filePath());
    asset.durationSeconds = metadata.durationSeconds;
    asset.sampleRate = metadata.sampleRate;
    asset.channels = metadata.channels;
    asset.createdAt = QDateTime::currentDateTimeUtc().toString(Qt::ISODate);
    asset.provenanceId = QUuid::createUuid().toString(QUuid::WithoutBraces);
    asset.contentChecksum = checksum;
    asset.status = "available";
    au::ailibrary::AssetProvenance provenance;
    provenance.id = asset.provenanceId;
    provenance.assetId = asset.id;
    provenance.operation = "import";
    provenance.providerId = "local-file";
    provenance.createdAt = asset.createdAt;
    provenance.outputChecksum = checksum;
    QString error;
    if (!au::ailibrary::LibraryAssetStore::addProjectAsset(m_activeWorkspace, asset, &provenance, &error)) {
        QFile::remove(destinationPath);
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    refreshLibraryAssets();
    const QString message = duplicate == existingAssets.cend()
                            ? QObject::tr("Imported %1 into the AI Library").arg(source.fileName())
                            : QObject::tr("Imported %1; it matches existing Library asset %2")
                                  .arg(source.fileName(), duplicate->name);
    AIStudioStatusModel::instance()->setWorkspaceStatus(message);
    AIStudioStatusModel::instance()->setLibraryStatus(message);
}

void AIStudioController::setLibraryAssetFavourite(const QString& assetId, bool favourite)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before changing Library favourites"));
        return;
    }

    QString error;
    if (!au::ailibrary::LibraryAssetStore::setProjectAssetFavourite(m_activeWorkspace, assetId, favourite, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    refreshLibraryAssets();
    AIStudioStatusModel::instance()->setLibraryStatus(favourite
        ? QObject::tr("Asset added to Favourites")
        : QObject::tr("Asset removed from Favourites"));
}

void AIStudioController::setLibraryAssetsFavourite(const QStringList& assetIds, bool favourite)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before changing Library favourites"));
        return;
    }

    QString error;
    if (!au::ailibrary::LibraryAssetStore::setProjectAssetsFavourite(m_activeWorkspace, assetIds, favourite, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    refreshLibraryAssets();
    AIStudioStatusModel::instance()->setLibraryStatus(favourite
        ? QObject::tr("Added %1 Library asset(s) to Favourites").arg(assetIds.size())
        : QObject::tr("Removed %1 Library asset(s) from Favourites").arg(assetIds.size()));
}

void AIStudioController::moveLibraryAssetsToFolder(const QStringList& assetIds, const QString& folder)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before moving Library assets"));
        return;
    }

    QString error;
    if (!au::ailibrary::LibraryAssetStore::moveProjectAssetsToFolder(m_activeWorkspace, assetIds, folder, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Moved %1 Library asset(s) to %2")
                                                       .arg(assetIds.size()).arg(folder.trimmed()));
    refreshLibraryAssets();
}

void AIStudioController::moveLibraryAssetsToUnfiled(const QStringList& assetIds)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before moving Library assets"));
        return;
    }

    QString error;
    if (!au::ailibrary::LibraryAssetStore::clearProjectAssetsFolder(m_activeWorkspace, assetIds, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Moved %1 Library asset(s) to Unfiled").arg(assetIds.size()));
    refreshLibraryAssets();
}

void AIStudioController::createLibraryFolder(const QString& folder)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before creating a Library folder"));
        return;
    }
    QString error;
    if (!au::ailibrary::LibraryAssetStore::createProjectFolder(m_activeWorkspace, folder, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Created Library folder %1").arg(folder.trimmed()));
    refreshLibraryAssets();
}

void AIStudioController::renameLibraryFolder(const QString& folder, const QString& newFolder)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before renaming a Library folder"));
        return;
    }
    QString error;
    if (!au::ailibrary::LibraryAssetStore::renameProjectFolder(m_activeWorkspace, folder, newFolder, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Renamed Library folder to %1").arg(newFolder.trimmed()));
    refreshLibraryAssets();
}

void AIStudioController::deleteLibraryFolder(const QString& folder)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before deleting a Library folder"));
        return;
    }
    QString error;
    if (!au::ailibrary::LibraryAssetStore::deleteProjectFolder(m_activeWorkspace, folder, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Deleted empty Library folder %1").arg(folder));
    refreshLibraryAssets();
}

void AIStudioController::renameLibraryAsset(const QString& assetId, const QString& name)
{
    QString error;
    if (!au::ailibrary::LibraryAssetStore::renameProjectAsset(m_activeWorkspace, assetId, name, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Renamed Library asset to %1").arg(name.trimmed()));
    refreshLibraryAssets();
}

void AIStudioController::deleteLibraryAsset(const QString& assetId)
{
    QString error;
    if (!au::ailibrary::LibraryAssetStore::deleteProjectAsset(m_activeWorkspace, assetId, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Removed asset from the Library"));
    refreshLibraryAssets();
}

void AIStudioController::setLibraryAssetTags(const QString& assetId, const QString& tags)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before changing Library tags"));
        return;
    }
    QStringList cleanedTags;
    for (const QString& tag : tags.split(',', Qt::SkipEmptyParts)) {
        const QString cleaned = tag.trimmed();
        if (!cleaned.isEmpty() && !cleanedTags.contains(cleaned, Qt::CaseInsensitive)) {
            cleanedTags.append(cleaned);
        }
    }
    QString error;
    if (!au::ailibrary::LibraryAssetStore::setProjectAssetTags(m_activeWorkspace, assetId, cleanedTags, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Updated Library asset tags"));
    refreshLibraryAssets();
}

void AIStudioController::setLibraryAssetsTags(const QStringList& assetIds, const QString& tags)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before changing Library tags"));
        return;
    }
    QStringList cleanedTags;
    for (const QString& tag : tags.split(',', Qt::SkipEmptyParts)) {
        const QString cleaned = tag.trimmed();
        if (!cleaned.isEmpty() && !cleanedTags.contains(cleaned, Qt::CaseInsensitive)) {
            cleanedTags.append(cleaned);
        }
    }
    QString error;
    if (!au::ailibrary::LibraryAssetStore::setProjectAssetsTags(m_activeWorkspace, assetIds, cleanedTags, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Updated tags for %1 Library asset(s)").arg(assetIds.size()));
    refreshLibraryAssets();
}

void AIStudioController::readLibraryAssetAudioDetails(const QString& assetId)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before reading WAV details"));
        return;
    }
    QString error;
    const QList<au::ailibrary::ProjectAsset> assets
        = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    const auto found = std::find_if(assets.cbegin(), assets.cend(), [&assetId](const auto& asset) {
        return asset.id == assetId;
    });
    if (found == assets.cend()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("The selected AI Library asset no longer exists"));
        return;
    }
    const WavMetadata metadata = wavMetadata(QDir(m_activeWorkspace).filePath(found->filePath));
    if (metadata.sampleRate <= 0) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Could not read standard WAV details for the selected Library asset"));
        return;
    }
    if (!au::ailibrary::LibraryAssetStore::setProjectAssetAudioMetadata(m_activeWorkspace, assetId,
                                                                          metadata.durationSeconds, metadata.sampleRate,
                                                                          metadata.channels, &error)) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    refreshLibraryAssets();
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Updated WAV details for %1").arg(found->name));
}

void AIStudioController::readLibraryAssetsAudioDetails(const QStringList& assetIds)
{
    if (assetIds.size() == 1) {
        readLibraryAssetAudioDetails(assetIds.front());
        return;
    }
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before reading WAV details"));
        return;
    }
    QString error;
    const QList<au::ailibrary::ProjectAsset> assets
        = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }

    int updated = 0;
    QStringList skippedAssets;
    for (const QString& assetId : assetIds) {
        const auto found = std::find_if(assets.cbegin(), assets.cend(), [&assetId](const auto& asset) {
            return asset.id == assetId;
        });
        if (found == assets.cend()) {
            skippedAssets.append(assetId);
            continue;
        }
        const WavMetadata metadata = wavMetadata(QDir(m_activeWorkspace).filePath(found->filePath));
        if (metadata.sampleRate <= 0
            || !au::ailibrary::LibraryAssetStore::setProjectAssetAudioMetadata(m_activeWorkspace, assetId,
                                                                                metadata.durationSeconds, metadata.sampleRate,
                                                                                metadata.channels, &error)) {
            skippedAssets.append(found->name);
            error.clear();
            continue;
        }
        ++updated;
    }
    refreshLibraryAssets();
    AIStudioStatusModel::instance()->setLibraryStatus(!skippedAssets.isEmpty()
        ? QObject::tr("Updated WAV details for %1 Library asset(s); skipped: %2")
              .arg(updated).arg(skippedAssets.join(", "))
        : QObject::tr("Updated WAV details for %1 Library asset(s)").arg(updated));
}

void AIStudioController::revealLibraryAssetInExplorer(const QString& assetId)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Enable the project AI workspace before revealing a Library asset"));
        return;
    }
    QString error;
    const QList<au::ailibrary::ProjectAsset> assets = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setLibraryStatus(error);
        return;
    }
    const auto found = std::find_if(assets.cbegin(), assets.cend(), [&assetId](const auto& asset) {
        return asset.id == assetId;
    });
    if (found == assets.cend()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("The selected AI Library asset no longer exists"));
        return;
    }
    const QString assetPath = QDir(m_activeWorkspace).filePath(found->filePath);
    const QFileInfo assetInfo(assetPath);
    if (!assetInfo.exists()) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("The selected AI Library file is missing"));
        return;
    }
    if (!QDesktopServices::openUrl(QUrl::fromLocalFile(assetInfo.absolutePath()))) {
        AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Could not open the Library asset folder"));
        return;
    }
    AIStudioStatusModel::instance()->setLibraryStatus(QObject::tr("Opened the Library asset folder"));
}

void AIStudioController::addLibraryAssetToTimeline(const QString& assetId)
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Enable the project AI workspace before adding a Library asset"));
        return;
    }
    const auto project = globalContext()->currentProject();
    if (!project) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Open a project before adding a Library asset to the timeline"));
        return;
    }
    QString error;
    const QList<au::ailibrary::ProjectAsset> assets = au::ailibrary::LibraryAssetStore::projectAssets(m_activeWorkspace, &error);
    if (!error.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        return;
    }
    const auto found = std::find_if(assets.cbegin(), assets.cend(), [&assetId](const auto& asset) {
        return asset.id == assetId;
    });
    if (found == assets.cend()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("The selected AI Library asset no longer exists"));
        return;
    }
    const QString assetPath = QDir(m_activeWorkspace).filePath(found->filePath);
    if (!QFileInfo::exists(assetPath)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("The selected AI Library file is missing"));
        return;
    }
    if (!project->import(muse::io::path_t(assetPath), false)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Could not add the AI Library asset to the timeline"));
        return;
    }
    AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Added %1 to the timeline; the Library asset remains available")
                                                        .arg(found->name));
}

void AIStudioController::recordCompletedJob(const QString& jobId, const QString& resultManifest)
{
    recordJobState(jobId, "complete", resultManifest);
}

void AIStudioController::recordJobState(const QString& jobId, const QString& state, const QString& resultManifest)
{
    if (m_activeWorkspace.isEmpty()) {
        return;
    }
    QString error;
    if (au::aiproject::WorkspaceStore::recordJobState(m_activeWorkspace, jobId, "test-provider", state, resultManifest, &error)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Test provider job %1 in the AI workspace").arg(state));
    } else {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
    }
}

void AIStudioController::insertTestJobOutput()
{
    if (m_activeWorkspace.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Enable the project AI workspace before inserting provider output"));
        return;
    }
    const auto project = globalContext()->currentProject();
    if (!project) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Open a project before inserting provider output"));
        return;
    }
    QString error;
    const QString assetPath = au::aiproject::WorkspaceStore::completedJobAssetPath(m_activeWorkspace, "test-provider-job", &error);
    if (assetPath.isEmpty()) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
        return;
    }
    if (!project->import(muse::io::path_t(assetPath), false)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Could not insert the test-provider output"));
        return;
    }
    if (au::aiproject::WorkspaceStore::markJobInserted(m_activeWorkspace, "test-provider-job", &error)) {
        AIStudioStatusModel::instance()->setWorkspaceStatus(QObject::tr("Test provider output inserted as a new track"));
    } else {
        AIStudioStatusModel::instance()->setWorkspaceStatus(error);
    }
}
