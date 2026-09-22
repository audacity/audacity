/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QStringList>
#include <QVariantList>

namespace au::aistudio {
class AIStudioStatusModel final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString runtimeStatus READ runtimeStatus NOTIFY runtimeStatusChanged)
    Q_PROPERTY(QString workspaceStatus READ workspaceStatus NOTIFY workspaceStatusChanged)
    Q_PROPERTY(QVariantList libraryAssets READ libraryAssets NOTIFY libraryAssetsChanged)
    Q_PROPERTY(QVariantList globalLibraryAssets READ globalLibraryAssets NOTIFY globalLibraryAssetsChanged)
    Q_PROPERTY(QStringList libraryFolders READ libraryFolders NOTIFY libraryFoldersChanged)
    Q_PROPERTY(QString libraryStatus READ libraryStatus NOTIFY libraryStatusChanged)

public:
    static AIStudioStatusModel* instance();

    QString runtimeStatus() const;
    QString workspaceStatus() const;
    QVariantList libraryAssets() const;
    QVariantList globalLibraryAssets() const;
    QStringList libraryFolders() const;
    QString libraryStatus() const;
    void setRuntimeStatus(const QString& status);
    void setWorkspaceStatus(const QString& status);
    void setLibraryAssets(const QVariantList& assets);
    void setGlobalLibraryAssets(const QVariantList& assets);
    void setLibraryFolders(const QStringList& folders);
    void setLibraryStatus(const QString& status);

    Q_INVOKABLE void runTestJob();
    Q_INVOKABLE void cancelTestJob();
    Q_INVOKABLE void insertTestJobOutput();
    Q_INVOKABLE void runWorkerFailureTest();
    Q_INVOKABLE void enableProjectWorkspace();
    Q_INVOKABLE void refreshLibrary();
    Q_INVOKABLE void importLocalWav(const QString& sourcePath);
    Q_INVOKABLE void setLibraryAssetFavourite(const QString& assetId, bool favourite);
    Q_INVOKABLE void setLibraryAssetsFavourite(const QStringList& assetIds, bool favourite);
    Q_INVOKABLE void moveLibraryAssetsToFolder(const QStringList& assetIds, const QString& folder);
    Q_INVOKABLE void moveLibraryAssetsToUnfiled(const QStringList& assetIds);
    Q_INVOKABLE void createLibraryFolder(const QString& folder);
    Q_INVOKABLE void renameLibraryFolder(const QString& folder, const QString& newFolder);
    Q_INVOKABLE void deleteLibraryFolder(const QString& folder);
    Q_INVOKABLE void renameLibraryAsset(const QString& assetId, const QString& name);
    Q_INVOKABLE void deleteLibraryAsset(const QString& assetId);
    Q_INVOKABLE void setLibraryAssetTags(const QString& assetId, const QString& tags);
    Q_INVOKABLE void setLibraryAssetsTags(const QStringList& assetIds, const QString& tags);
    Q_INVOKABLE void readLibraryAssetAudioDetails(const QString& assetId);
    Q_INVOKABLE void readLibraryAssetsAudioDetails(const QStringList& assetIds);
    Q_INVOKABLE void revealLibraryAssetInExplorer(const QString& assetId);
    Q_INVOKABLE void addLibraryAssetToTimeline(const QString& assetId);
    Q_INVOKABLE void copyGlobalLibraryAssetToProject(const QString& projectPath, const QString& assetId);

signals:
    void runtimeStatusChanged();
    void workspaceStatusChanged();
    void libraryAssetsChanged();
    void globalLibraryAssetsChanged();
    void libraryFoldersChanged();
    void libraryStatusChanged();
    void testJobRequested();
    void testJobCancelRequested();
    void testJobInsertRequested();
    void workerFailureTestRequested();
    void workspaceEnableRequested();
    void libraryRefreshRequested();
    void libraryImportRequested(const QString& sourcePath);
    void libraryAssetFavouriteRequested(const QString& assetId, bool favourite);
    void libraryAssetsFavouriteRequested(const QStringList& assetIds, bool favourite);
    void libraryAssetsMoveRequested(const QStringList& assetIds, const QString& folder);
    void libraryAssetsUnfileRequested(const QStringList& assetIds);
    void libraryFolderCreateRequested(const QString& folder);
    void libraryFolderRenameRequested(const QString& folder, const QString& newFolder);
    void libraryFolderDeleteRequested(const QString& folder);
    void libraryAssetRenameRequested(const QString& assetId, const QString& name);
    void libraryAssetDeleteRequested(const QString& assetId);
    void libraryAssetTagsRequested(const QString& assetId, const QString& tags);
    void libraryAssetsTagsRequested(const QStringList& assetIds, const QString& tags);
    void libraryAssetAudioDetailsRequested(const QString& assetId);
    void libraryAssetsAudioDetailsRequested(const QStringList& assetIds);
    void libraryAssetRevealRequested(const QString& assetId);
    void libraryAssetInsertionRequested(const QString& assetId);
    void globalLibraryAssetCopyRequested(const QString& projectPath, const QString& assetId);

private:
    AIStudioStatusModel();
    QString m_runtimeStatus;
    QString m_workspaceStatus;
    QVariantList m_libraryAssets;
    QVariantList m_globalLibraryAssets;
    QStringList m_libraryFolders;
    QString m_libraryStatus;
};
}
