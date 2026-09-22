/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "projectasset.h"

#include <QList>
#include <QString>

namespace au::ailibrary {

class LibraryAssetStore final
{
public:
    // The workspace must already have been enabled through WorkspaceStore.
    // Adding an asset never inserts it into the Audacity timeline.
    static bool addProjectAsset(const QString& workspacePath, const ProjectAsset& asset,
                                const AssetProvenance* provenance = nullptr,
                                QString* errorMessage = nullptr);
    static QList<ProjectAsset> projectAssets(const QString& workspacePath,
                                             QString* errorMessage = nullptr);
    static bool setProjectAssetFavourite(const QString& workspacePath, const QString& assetId, bool favourite,
                                         QString* errorMessage = nullptr);
    static bool setProjectAssetsFavourite(const QString& workspacePath, const QStringList& assetIds, bool favourite,
                                          QString* errorMessage = nullptr);
    static bool moveProjectAssetsToFolder(const QString& workspacePath, const QStringList& assetIds,
                                          const QString& folder, QString* errorMessage = nullptr);
    static bool clearProjectAssetsFolder(const QString& workspacePath, const QStringList& assetIds,
                                         QString* errorMessage = nullptr);
    static QStringList projectFolders(const QString& workspacePath, QString* errorMessage = nullptr);
    static bool createProjectFolder(const QString& workspacePath, const QString& folder,
                                    QString* errorMessage = nullptr);
    static bool renameProjectFolder(const QString& workspacePath, const QString& folder, const QString& newFolder,
                                    QString* errorMessage = nullptr);
    static bool deleteProjectFolder(const QString& workspacePath, const QString& folder,
                                    QString* errorMessage = nullptr);
    static bool renameProjectAsset(const QString& workspacePath, const QString& assetId, const QString& name,
                                   QString* errorMessage = nullptr);
    static bool deleteProjectAsset(const QString& workspacePath, const QString& assetId,
                                   QString* errorMessage = nullptr);
    static bool setProjectAssetTags(const QString& workspacePath, const QString& assetId, const QStringList& tags,
                                    QString* errorMessage = nullptr);
    static bool setProjectAssetsTags(const QString& workspacePath, const QStringList& assetIds, const QStringList& tags,
                                     QString* errorMessage = nullptr);
    static bool setProjectAssetAudioMetadata(const QString& workspacePath, const QString& assetId,
                                             double durationSeconds, int sampleRate, int channels,
                                             QString* errorMessage = nullptr);
};

}
