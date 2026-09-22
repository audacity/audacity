/*
 * Audacity: A Digital Audio Editor
 */
#include "libraryassetstore.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QObject>
#include <QRegularExpression>
#include <QSaveFile>
#include <QSet>

using namespace au::ailibrary;

namespace {
constexpr auto MANIFEST_FILE = "manifest.json";

QJsonArray toJson(const QStringList& values)
{
    QJsonArray result;
    for (const QString& value : values) {
        result.append(value);
    }
    return result;
}

QStringList fromJson(const QJsonArray& values)
{
    QStringList result;
    for (const QJsonValue& value : values) {
        result.append(value.toString());
    }
    return result;
}

QString cleanFolderName(const QString& folder)
{
    return folder.trimmed();
}

bool isValidFolderName(const QString& folder)
{
    static const QRegularExpression invalidFolderName(R"([\\/:*?"<>|])");
    return !folder.isEmpty() && !invalidFolderName.match(folder).hasMatch();
}

QJsonObject assetToJson(const ProjectAsset& asset)
{
    return {
        { "id", asset.id }, { "projectId", asset.projectId }, { "name", asset.name },
        { "kind", asset.kind }, { "origin", asset.origin }, { "filePath", asset.filePath },
        { "sourceAssetIds", toJson(asset.sourceAssetIds) }, { "durationSeconds", asset.durationSeconds },
        { "sampleRate", asset.sampleRate }, { "channels", asset.channels }, { "createdAt", asset.createdAt },
        { "updatedAt", asset.updatedAt }, { "provenanceId", asset.provenanceId },
        { "contentChecksum", asset.contentChecksum }, { "tags", toJson(asset.tags) },
        { "favourite", asset.favourite }, { "folder", asset.folder }, { "status", asset.status }
    };
}

ProjectAsset assetFromJson(const QJsonObject& value)
{
    ProjectAsset asset;
    asset.id = value.value("id").toString();
    asset.projectId = value.value("projectId").toString();
    asset.name = value.value("name").toString();
    asset.kind = value.value("kind").toString();
    asset.origin = value.value("origin").toString();
    asset.filePath = value.value("filePath").toString();
    asset.sourceAssetIds = fromJson(value.value("sourceAssetIds").toArray());
    asset.durationSeconds = value.value("durationSeconds").toDouble();
    asset.sampleRate = value.value("sampleRate").toInt();
    asset.channels = value.value("channels").toInt();
    asset.createdAt = value.value("createdAt").toString();
    asset.updatedAt = value.value("updatedAt").toString();
    asset.provenanceId = value.value("provenanceId").toString();
    asset.contentChecksum = value.value("contentChecksum").toString();
    asset.tags = fromJson(value.value("tags").toArray());
    asset.favourite = value.value("favourite").toBool();
    asset.folder = value.value("folder").toString();
    asset.status = value.value("status").toString("available");
    return asset;
}

QJsonObject provenanceToJson(const AssetProvenance& provenance)
{
    return {
        { "id", provenance.id }, { "assetId", provenance.assetId }, { "operation", provenance.operation },
        { "providerId", provenance.providerId }, { "providerVersion", provenance.providerVersion },
        { "modelId", provenance.modelId }, { "modelRevision", provenance.modelRevision },
        { "adaptationIds", toJson(provenance.adaptationIds) }, { "sourceAssetIds", toJson(provenance.sourceAssetIds) },
        { "songPlanId", provenance.songPlanId }, { "lyricsRevisionId", provenance.lyricsRevisionId },
        { "prompt", provenance.prompt }, { "jobId", provenance.jobId }, { "createdAt", provenance.createdAt },
        { "outputChecksum", provenance.outputChecksum }
    };
}

bool saveManifest(const QString& manifestPath, const QJsonObject& root, QString* errorMessage)
{
    QSaveFile output(manifestPath);
    if (!output.open(QIODevice::WriteOnly)
        || output.write(QJsonDocument(root).toJson(QJsonDocument::Indented)) < 1
        || !output.commit()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not save the AI workspace manifest");
        }
        return false;
    }
    return true;
}
}

bool LibraryAssetStore::addProjectAsset(const QString& workspacePath, const ProjectAsset& asset,
                                        const AssetProvenance* provenance, QString* errorMessage)
{
    if (asset.id.isEmpty() || asset.name.isEmpty() || asset.kind.isEmpty() || asset.origin.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("An AI Library asset needs an id, name, kind, and origin");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return false;
    }
    const QByteArray manifestData = manifest.readAll();
    manifest.close();
    QJsonDocument document = QJsonDocument::fromJson(manifestData);
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return false;
    }

    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    for (const QJsonValue& value : assets) {
        if (value.toObject().value("id").toString() == asset.id) {
            if (errorMessage) {
                *errorMessage = QObject::tr("An AI Library asset with this id already exists");
            }
            return false;
        }
    }
    assets.append(assetToJson(asset));
    root.insert("assets", assets);

    if (provenance) {
        QJsonArray provenanceEntries = root.value("provenance").toArray();
        provenanceEntries.append(provenanceToJson(*provenance));
        root.insert("provenance", provenanceEntries);
    }

    return saveManifest(manifestPath, root, errorMessage);
}

QList<ProjectAsset> LibraryAssetStore::projectAssets(const QString& workspacePath, QString* errorMessage)
{
    QFile manifest(QDir(workspacePath).filePath(MANIFEST_FILE));
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return {};
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return {};
    }
    QList<ProjectAsset> result;
    for (const QJsonValue& value : document.object().value("assets").toArray()) {
        result.append(assetFromJson(value.toObject()));
    }
    return result;
}

bool LibraryAssetStore::setProjectAssetFavourite(const QString& workspacePath, const QString& assetId, bool favourite,
                                                 QString* errorMessage)
{
    if (assetId.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Select a Library asset first");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return false;
    }

    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    bool found = false;
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (asset.value("id").toString() == assetId) {
            asset.insert("favourite", favourite);
            assets.replace(index, asset);
            found = true;
            break;
        }
    }
    if (!found) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The selected AI Library asset no longer exists");
        }
        return false;
    }

    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::setProjectAssetsFavourite(const QString& workspacePath, const QStringList& assetIds,
                                                  bool favourite, QString* errorMessage)
{
    if (assetIds.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Select one or more Library assets first");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return false;
    }

    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    QSet<QString> pending(assetIds.cbegin(), assetIds.cend());
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (pending.remove(asset.value("id").toString())) {
            asset.insert("favourite", favourite);
            assets.replace(index, asset);
        }
    }
    if (!pending.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("One or more selected Library assets no longer exist");
        }
        return false;
    }

    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::moveProjectAssetsToFolder(const QString& workspacePath, const QStringList& assetIds,
                                                  const QString& folder, QString* errorMessage)
{
    const QString cleanFolder = cleanFolderName(folder);
    if (assetIds.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Select one or more Library assets first");
        }
        return false;
    }
    if (!isValidFolderName(cleanFolder)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Choose a folder name without path characters");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return false;
    }

    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    QStringList folders = fromJson(root.value("folders").toArray());
    if (!folders.contains(cleanFolder, Qt::CaseInsensitive)) {
        folders.append(cleanFolder);
        folders.sort(Qt::CaseInsensitive);
        root.insert("folders", toJson(folders));
    }
    QSet<QString> pending(assetIds.cbegin(), assetIds.cend());
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        const QString id = asset.value("id").toString();
        if (pending.remove(id)) {
            asset.insert("folder", cleanFolder);
            assets.replace(index, asset);
        }
    }
    if (!pending.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("One or more selected Library assets no longer exist");
        }
        return false;
    }

    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::clearProjectAssetsFolder(const QString& workspacePath, const QStringList& assetIds,
                                                 QString* errorMessage)
{
    if (assetIds.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Select one or more Library assets first");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return false;
    }

    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    QSet<QString> pending(assetIds.cbegin(), assetIds.cend());
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (pending.remove(asset.value("id").toString())) {
            asset.insert("folder", QString());
            assets.replace(index, asset);
        }
    }
    if (!pending.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("One or more selected Library assets no longer exist");
        }
        return false;
    }

    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

QStringList LibraryAssetStore::projectFolders(const QString& workspacePath, QString* errorMessage)
{
    QFile manifest(QDir(workspacePath).filePath(MANIFEST_FILE));
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return {};
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return {};
    }
    QStringList folders = fromJson(document.object().value("folders").toArray());
    for (const QJsonValue& value : document.object().value("assets").toArray()) {
        const QString folder = value.toObject().value("folder").toString();
        if (!folder.isEmpty() && !folders.contains(folder)) {
            folders.append(folder);
        }
    }
    folders.sort(Qt::CaseInsensitive);
    return folders;
}

bool LibraryAssetStore::createProjectFolder(const QString& workspacePath, const QString& folder, QString* errorMessage)
{
    const QString cleanFolder = cleanFolderName(folder);
    if (!isValidFolderName(cleanFolder)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Choose a folder name without path characters");
        }
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QStringList folders = fromJson(root.value("folders").toArray());
    if (folders.contains(cleanFolder, Qt::CaseInsensitive)) {
        if (errorMessage) *errorMessage = QObject::tr("A Library folder with this name already exists");
        return false;
    }
    folders.append(cleanFolder);
    folders.sort(Qt::CaseInsensitive);
    root.insert("folders", toJson(folders));
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::renameProjectFolder(const QString& workspacePath, const QString& folder, const QString& newFolder,
                                            QString* errorMessage)
{
    const QString cleanFolder = cleanFolderName(folder);
    const QString cleanNewFolder = cleanFolderName(newFolder);
    if (cleanFolder.isEmpty() || !isValidFolderName(cleanNewFolder)) {
        if (errorMessage) *errorMessage = QObject::tr("Choose a folder name without path characters");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QStringList folders = fromJson(root.value("folders").toArray());
    if (!folders.contains(cleanFolder)) {
        if (errorMessage) *errorMessage = QObject::tr("The selected Library folder no longer exists");
        return false;
    }
    if (cleanFolder.compare(cleanNewFolder, Qt::CaseInsensitive) != 0
        && folders.contains(cleanNewFolder, Qt::CaseInsensitive)) {
        if (errorMessage) *errorMessage = QObject::tr("A Library folder with this name already exists");
        return false;
    }
    folders.replace(folders.indexOf(cleanFolder), cleanNewFolder);
    folders.sort(Qt::CaseInsensitive);
    QJsonArray assets = root.value("assets").toArray();
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (asset.value("folder").toString() == cleanFolder) {
            asset.insert("folder", cleanNewFolder);
            assets.replace(index, asset);
        }
    }
    root.insert("folders", toJson(folders));
    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::deleteProjectFolder(const QString& workspacePath, const QString& folder, QString* errorMessage)
{
    const QString cleanFolder = cleanFolderName(folder);
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QStringList folders = fromJson(root.value("folders").toArray());
    if (!folders.contains(cleanFolder)) {
        if (errorMessage) *errorMessage = QObject::tr("The selected Library folder no longer exists");
        return false;
    }
    for (const QJsonValue& value : root.value("assets").toArray()) {
        if (value.toObject().value("folder").toString() == cleanFolder) {
            if (errorMessage) *errorMessage = QObject::tr("Move all assets out of this folder before deleting it");
            return false;
        }
    }
    folders.removeAll(cleanFolder);
    root.insert("folders", toJson(folders));
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::renameProjectAsset(const QString& workspacePath, const QString& assetId, const QString& name,
                                           QString* errorMessage)
{
    const QString cleanName = name.trimmed();
    if (assetId.isEmpty() || cleanName.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("Enter a name for the Library asset");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    bool found = false;
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (asset.value("id").toString() == assetId) {
            asset.insert("name", cleanName);
            assets.replace(index, asset);
            found = true;
            break;
        }
    }
    if (!found) {
        if (errorMessage) *errorMessage = QObject::tr("The selected AI Library asset no longer exists");
        return false;
    }
    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::deleteProjectAsset(const QString& workspacePath, const QString& assetId, QString* errorMessage)
{
    if (assetId.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("Select a Library asset first");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    bool found = false;
    for (qsizetype index = assets.size() - 1; index >= 0; --index) {
        if (assets.at(index).toObject().value("id").toString() == assetId) {
            assets.removeAt(index);
            found = true;
            break;
        }
    }
    if (!found) {
        if (errorMessage) *errorMessage = QObject::tr("The selected AI Library asset no longer exists");
        return false;
    }
    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::setProjectAssetTags(const QString& workspacePath, const QString& assetId, const QStringList& tags,
                                            QString* errorMessage)
{
    if (assetId.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("Select a Library asset first");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    bool found = false;
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (asset.value("id").toString() == assetId) {
            asset.insert("tags", toJson(tags));
            assets.replace(index, asset);
            found = true;
            break;
        }
    }
    if (!found) {
        if (errorMessage) *errorMessage = QObject::tr("The selected AI Library asset no longer exists");
        return false;
    }
    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::setProjectAssetsTags(const QString& workspacePath, const QStringList& assetIds,
                                             const QStringList& tags, QString* errorMessage)
{
    if (assetIds.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("Select one or more Library assets first");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    QSet<QString> pending(assetIds.cbegin(), assetIds.cend());
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (pending.remove(asset.value("id").toString())) {
            asset.insert("tags", toJson(tags));
            assets.replace(index, asset);
        }
    }
    if (!pending.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("One or more selected Library assets no longer exist");
        return false;
    }
    root.insert("assets", assets);
    return saveManifest(manifestPath, root, errorMessage);
}

bool LibraryAssetStore::setProjectAssetAudioMetadata(const QString& workspacePath, const QString& assetId,
                                                      double durationSeconds, int sampleRate, int channels,
                                                      QString* errorMessage)
{
    if (assetId.isEmpty()) {
        if (errorMessage) *errorMessage = QObject::tr("Select a Library asset first");
        return false;
    }
    const QString manifestPath = QDir(workspacePath).filePath(MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(manifest.readAll());
    manifest.close();
    if (!document.isObject()) {
        if (errorMessage) *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        return false;
    }
    QJsonObject root = document.object();
    QJsonArray assets = root.value("assets").toArray();
    for (qsizetype index = 0; index < assets.size(); ++index) {
        QJsonObject asset = assets.at(index).toObject();
        if (asset.value("id").toString() == assetId) {
            asset.insert("durationSeconds", durationSeconds);
            asset.insert("sampleRate", sampleRate);
            asset.insert("channels", channels);
            assets.replace(index, asset);
            root.insert("assets", assets);
            return saveManifest(manifestPath, root, errorMessage);
        }
    }
    if (errorMessage) *errorMessage = QObject::tr("The selected AI Library asset no longer exists");
    return false;
}
