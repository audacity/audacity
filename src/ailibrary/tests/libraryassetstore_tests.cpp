/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QTemporaryDir>

#include "ailibrary/libraryassetstore.h"
#include "ailibrary/globalassetcatalogue.h"
#include "aiproject/aiworkspace.h"

namespace au::ailibrary {
namespace {

TEST(LibraryAssetStoreTests, PersistsProjectRelativeAssetAndProvenance)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());

    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.id = "asset-001";
    asset.name = "Fixture WAV";
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.filePath = "assets/imported/asset-001-fixture.wav";
    asset.contentChecksum = "sha256-fixture";
    asset.createdAt = "2026-09-20T00:00:00Z";

    AssetProvenance provenance;
    provenance.id = "provenance-001";
    provenance.assetId = asset.id;
    provenance.operation = "import";
    provenance.providerId = "local-file";
    provenance.prompt = "fixture provenance";
    provenance.createdAt = asset.createdAt;

    QString error;
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, &provenance, &error)) << error.toStdString();

    const QList<ProjectAsset> restored = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    ASSERT_EQ(restored.size(), 1);
    EXPECT_EQ(restored.front().id, asset.id);
    EXPECT_EQ(restored.front().filePath, asset.filePath);
    EXPECT_EQ(restored.front().contentChecksum, asset.contentChecksum);

    QFile manifest(workspacePath + "/manifest.json");
    ASSERT_TRUE(manifest.open(QIODevice::ReadOnly));
    const QJsonObject root = QJsonDocument::fromJson(manifest.readAll()).object();
    ASSERT_EQ(root.value("provenance").toArray().size(), 1);
    EXPECT_EQ(root.value("provenance").toArray().at(0).toObject().value("prompt").toString(), provenance.prompt);
    EXPECT_FALSE(root.value("assets").toArray().at(0).toObject().value("filePath").toString().contains(projectDirectory.path()));
}

TEST(LibraryAssetStoreTests, RejectsDuplicateAssetIdsWithoutDiscardingExistingAsset)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.id = "asset-001";
    asset.name = "First asset";
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.createdAt = "2026-09-20T00:00:00Z";
    QString error;
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();

    asset.name = "Second asset";
    EXPECT_FALSE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error));
    EXPECT_FALSE(error.isEmpty());

    const QList<ProjectAsset> restored = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_EQ(restored.size(), 1);
    EXPECT_EQ(restored.front().name, "First asset");
}

TEST(LibraryAssetStoreTests, GlobalCatalogueReplacesOnlyTheIndexedProject)
{
    QTemporaryDir directory;
    ASSERT_TRUE(directory.isValid());
    const QByteArray oldCataloguePath = qgetenv("AUDACITY_AI_LIBRARY_CATALOGUE_PATH");
    qputenv("AUDACITY_AI_LIBRARY_CATALOGUE_PATH", directory.filePath("catalogue.json").toUtf8());

    ProjectAsset first;
    first.id = "project-one-asset";
    first.name = "Project one";
    first.kind = "upload";
    first.origin = "uploaded";
    ProjectAsset second = first;
    second.id = "project-two-asset";
    second.name = "Project two";
    QString error;
    ASSERT_TRUE(GlobalAssetCatalogue::syncProject("one.aup4", "one/ai", { first }, &error)) << error.toStdString();
    ASSERT_TRUE(GlobalAssetCatalogue::syncProject("two.aup4", "two/ai", { second }, &error)) << error.toStdString();
    ASSERT_EQ(GlobalAssetCatalogue::allAssets(&error).size(), 2);

    first.name = "Project one updated";
    ASSERT_TRUE(GlobalAssetCatalogue::syncProject("one.aup4", "one/ai", { first }, &error)) << error.toStdString();
    const QList<GlobalAssetRecord> allAssets = GlobalAssetCatalogue::allAssets(&error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    ASSERT_EQ(allAssets.size(), 2);
    const auto updated = std::find_if(allAssets.cbegin(), allAssets.cend(), [](const auto& item) {
        return item.projectPath == "one.aup4";
    });
    const auto untouched = std::find_if(allAssets.cbegin(), allAssets.cend(), [](const auto& item) {
        return item.projectPath == "two.aup4";
    });
    ASSERT_NE(updated, allAssets.cend());
    ASSERT_NE(untouched, allAssets.cend());
    EXPECT_EQ(updated->asset.name, first.name);
    EXPECT_EQ(untouched->asset.name, second.name);

    if (oldCataloguePath.isEmpty()) {
        qunsetenv("AUDACITY_AI_LIBRARY_CATALOGUE_PATH");
    } else {
        qputenv("AUDACITY_AI_LIBRARY_CATALOGUE_PATH", oldCataloguePath);
    }
}

TEST(LibraryAssetStoreTests, PersistsFavouriteState)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.id = "favourite-asset";
    asset.name = "Favourite fixture";
    asset.kind = "upload";
    asset.origin = "uploaded";
    QString error;
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    ASSERT_TRUE(LibraryAssetStore::setProjectAssetFavourite(workspacePath, asset.id, true, &error)) << error.toStdString();

    const QList<ProjectAsset> restored = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    ASSERT_EQ(restored.size(), 1);
    EXPECT_TRUE(restored.front().favourite);
}

TEST(LibraryAssetStoreTests, PersistsFolderAssignmentForSeveralAssets)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.id = "folder-asset-one";
    asset.name = "First fixture";
    QString error;
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    asset.id = "folder-asset-two";
    asset.name = "Second fixture";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();

    ASSERT_TRUE(LibraryAssetStore::moveProjectAssetsToFolder(workspacePath,
                                                              { "folder-asset-one", "folder-asset-two" },
                                                              "Drum ideas", &error)) << error.toStdString();
    const QList<ProjectAsset> restored = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    ASSERT_EQ(restored.size(), 2);
    EXPECT_EQ(restored.at(0).folder, "Drum ideas");
    EXPECT_EQ(restored.at(1).folder, "Drum ideas");
}

TEST(LibraryAssetStoreTests, PersistsFavouriteForSeveralAssets)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.name = "Favourite fixture";
    QString error;
    asset.id = "favourite-one";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    asset.id = "favourite-two";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();

    ASSERT_TRUE(LibraryAssetStore::setProjectAssetsFavourite(workspacePath,
                                                               { "favourite-one", "favourite-two" }, true, &error)) << error.toStdString();
    const QList<ProjectAsset> assets = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    EXPECT_TRUE(assets.at(0).favourite);
    EXPECT_TRUE(assets.at(1).favourite);
}

TEST(LibraryAssetStoreTests, PersistsTagsForSeveralAssets)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.kind = "upload";
    asset.origin = "uploaded";
    asset.name = "Tag fixture";
    QString error;
    asset.id = "tag-one";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    asset.id = "tag-two";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();

    ASSERT_TRUE(LibraryAssetStore::setProjectAssetsTags(workspacePath, { "tag-one", "tag-two" },
                                                         { "drums", "rough idea" }, &error)) << error.toStdString();
    const QList<ProjectAsset> assets = LibraryAssetStore::projectAssets(workspacePath, &error);
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();
    EXPECT_EQ(assets.at(0).tags, QStringList({ "drums", "rough idea" }));
    EXPECT_EQ(assets.at(1).tags, QStringList({ "drums", "rough idea" }));
}

TEST(LibraryAssetStoreTests, ClearsFolderAssignmentWithoutRemovingTheFolder)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);

    ProjectAsset asset;
    asset.id = "unfile-asset";
    asset.name = "Unfile fixture";
    asset.kind = "upload";
    asset.origin = "uploaded";
    QString error;
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    ASSERT_TRUE(LibraryAssetStore::moveProjectAssetsToFolder(workspacePath, { asset.id }, "Ideas", &error)) << error.toStdString();
    ASSERT_TRUE(LibraryAssetStore::clearProjectAssetsFolder(workspacePath, { asset.id }, &error)) << error.toStdString();

    EXPECT_TRUE(LibraryAssetStore::projectAssets(workspacePath, &error).front().folder.isEmpty());
    EXPECT_EQ(LibraryAssetStore::projectFolders(workspacePath, &error), QStringList({ "Ideas" }));
}

TEST(LibraryAssetStoreTests, KeepsEmptyFolderAndRenamesAssignedAssets)
{
    QTemporaryDir projectDirectory;
    ASSERT_TRUE(projectDirectory.isValid());
    const QString projectPath = projectDirectory.filePath("library-test.aup4");
    ASSERT_TRUE(aiproject::WorkspaceStore::create(projectPath));
    const QString workspacePath = aiproject::WorkspaceStore::workspacePathForProject(projectPath);
    QString error;

    ASSERT_TRUE(LibraryAssetStore::createProjectFolder(workspacePath, "Empty ideas", &error)) << error.toStdString();
    EXPECT_EQ(LibraryAssetStore::projectFolders(workspacePath, &error), QStringList({ "Empty ideas" }));
    ASSERT_TRUE(error.isEmpty()) << error.toStdString();

    ProjectAsset asset;
    asset.id = "rename-folder-asset";
    asset.name = "Rename fixture";
    asset.kind = "upload";
    asset.origin = "uploaded";
    ASSERT_TRUE(LibraryAssetStore::addProjectAsset(workspacePath, asset, nullptr, &error)) << error.toStdString();
    ASSERT_TRUE(LibraryAssetStore::moveProjectAssetsToFolder(workspacePath, { asset.id }, "Empty ideas", &error)) << error.toStdString();
    ASSERT_TRUE(LibraryAssetStore::renameProjectFolder(workspacePath, "Empty ideas", "Archive", &error)) << error.toStdString();
    EXPECT_EQ(LibraryAssetStore::projectAssets(workspacePath, &error).front().folder, "Archive");
    EXPECT_EQ(LibraryAssetStore::projectFolders(workspacePath, &error), QStringList({ "Archive" }));
}

}
}
