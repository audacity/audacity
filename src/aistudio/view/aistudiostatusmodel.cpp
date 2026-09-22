/*
 * Audacity: A Digital Audio Editor
 */
#include "aistudiostatusmodel.h"

#include <QCoreApplication>

using namespace au::aistudio;

AIStudioStatusModel::AIStudioStatusModel()
    : QObject(QCoreApplication::instance()), m_runtimeStatus(tr("Runtime host not started")),
      m_workspaceStatus(tr("AI workspace not enabled for this project")),
      m_libraryStatus(tr("No Library activity yet"))
{
}

AIStudioStatusModel* AIStudioStatusModel::instance()
{
    static AIStudioStatusModel model;
    return &model;
}

QString AIStudioStatusModel::runtimeStatus() const
{
    return m_runtimeStatus;
}

QString AIStudioStatusModel::workspaceStatus() const
{
    return m_workspaceStatus;
}

QVariantList AIStudioStatusModel::libraryAssets() const
{
    return m_libraryAssets;
}

QVariantList AIStudioStatusModel::globalLibraryAssets() const
{
    return m_globalLibraryAssets;
}

QStringList AIStudioStatusModel::libraryFolders() const
{
    return m_libraryFolders;
}

QString AIStudioStatusModel::libraryStatus() const
{
    return m_libraryStatus;
}

void AIStudioStatusModel::setRuntimeStatus(const QString& status)
{
    if (m_runtimeStatus == status) {
        return;
    }
    m_runtimeStatus = status;
    emit runtimeStatusChanged();
}

void AIStudioStatusModel::setWorkspaceStatus(const QString& status)
{
    if (m_workspaceStatus == status) {
        return;
    }
    m_workspaceStatus = status;
    emit workspaceStatusChanged();
}

void AIStudioStatusModel::setLibraryAssets(const QVariantList& assets)
{
    if (m_libraryAssets == assets) {
        return;
    }
    m_libraryAssets = assets;
    emit libraryAssetsChanged();
}

void AIStudioStatusModel::setGlobalLibraryAssets(const QVariantList& assets)
{
    if (m_globalLibraryAssets == assets) {
        return;
    }
    m_globalLibraryAssets = assets;
    emit globalLibraryAssetsChanged();
}

void AIStudioStatusModel::setLibraryFolders(const QStringList& folders)
{
    if (m_libraryFolders == folders) {
        return;
    }
    m_libraryFolders = folders;
    emit libraryFoldersChanged();
}

void AIStudioStatusModel::setLibraryStatus(const QString& status)
{
    if (m_libraryStatus == status) {
        return;
    }
    m_libraryStatus = status;
    emit libraryStatusChanged();
}

void AIStudioStatusModel::runTestJob()
{
    emit testJobRequested();
}

void AIStudioStatusModel::cancelTestJob()
{
    emit testJobCancelRequested();
}

void AIStudioStatusModel::insertTestJobOutput()
{
    emit testJobInsertRequested();
}

void AIStudioStatusModel::runWorkerFailureTest()
{
    emit workerFailureTestRequested();
}

void AIStudioStatusModel::enableProjectWorkspace()
{
    emit workspaceEnableRequested();
}

void AIStudioStatusModel::refreshLibrary()
{
    emit libraryRefreshRequested();
}

void AIStudioStatusModel::importLocalWav(const QString& sourcePath)
{
    if (!sourcePath.isEmpty()) {
        emit libraryImportRequested(sourcePath);
    }
}

void AIStudioStatusModel::setLibraryAssetFavourite(const QString& assetId, bool favourite)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetFavouriteRequested(assetId, favourite);
    }
}

void AIStudioStatusModel::setLibraryAssetsFavourite(const QStringList& assetIds, bool favourite)
{
    if (!assetIds.isEmpty()) {
        emit libraryAssetsFavouriteRequested(assetIds, favourite);
    }
}

void AIStudioStatusModel::moveLibraryAssetsToFolder(const QStringList& assetIds, const QString& folder)
{
    if (!assetIds.isEmpty() && !folder.trimmed().isEmpty()) {
        emit libraryAssetsMoveRequested(assetIds, folder);
    }
}

void AIStudioStatusModel::moveLibraryAssetsToUnfiled(const QStringList& assetIds)
{
    if (!assetIds.isEmpty()) {
        emit libraryAssetsUnfileRequested(assetIds);
    }
}

void AIStudioStatusModel::createLibraryFolder(const QString& folder)
{
    if (!folder.trimmed().isEmpty()) {
        emit libraryFolderCreateRequested(folder);
    }
}

void AIStudioStatusModel::renameLibraryFolder(const QString& folder, const QString& newFolder)
{
    if (!folder.isEmpty() && !newFolder.trimmed().isEmpty()) {
        emit libraryFolderRenameRequested(folder, newFolder);
    }
}

void AIStudioStatusModel::deleteLibraryFolder(const QString& folder)
{
    if (!folder.isEmpty()) {
        emit libraryFolderDeleteRequested(folder);
    }
}

void AIStudioStatusModel::renameLibraryAsset(const QString& assetId, const QString& name)
{
    if (!assetId.isEmpty() && !name.trimmed().isEmpty()) {
        emit libraryAssetRenameRequested(assetId, name);
    }
}

void AIStudioStatusModel::deleteLibraryAsset(const QString& assetId)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetDeleteRequested(assetId);
    }
}

void AIStudioStatusModel::setLibraryAssetTags(const QString& assetId, const QString& tags)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetTagsRequested(assetId, tags);
    }
}

void AIStudioStatusModel::setLibraryAssetsTags(const QStringList& assetIds, const QString& tags)
{
    if (!assetIds.isEmpty()) {
        emit libraryAssetsTagsRequested(assetIds, tags);
    }
}

void AIStudioStatusModel::readLibraryAssetAudioDetails(const QString& assetId)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetAudioDetailsRequested(assetId);
    }
}

void AIStudioStatusModel::readLibraryAssetsAudioDetails(const QStringList& assetIds)
{
    if (!assetIds.isEmpty()) {
        emit libraryAssetsAudioDetailsRequested(assetIds);
    }
}

void AIStudioStatusModel::revealLibraryAssetInExplorer(const QString& assetId)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetRevealRequested(assetId);
    }
}

void AIStudioStatusModel::addLibraryAssetToTimeline(const QString& assetId)
{
    if (!assetId.isEmpty()) {
        emit libraryAssetInsertionRequested(assetId);
    }
}

void AIStudioStatusModel::copyGlobalLibraryAssetToProject(const QString& projectPath, const QString& assetId)
{
    if (!projectPath.isEmpty() && !assetId.isEmpty()) {
        emit globalLibraryAssetCopyRequested(projectPath, assetId);
    }
}
