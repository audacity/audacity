/*
 * Audacity: A Digital Audio Editor
 */
#include "globalassetcatalogue.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSaveFile>
#include <QStandardPaths>

using namespace au::ailibrary;

namespace {
constexpr auto CATALOGUE_FILE = "ai-library-catalogue.json";

QJsonArray stringListToJson(const QStringList& values)
{
    QJsonArray result;
    for (const QString& value : values) {
        result.append(value);
    }
    return result;
}

QStringList stringListFromJson(const QJsonArray& values)
{
    QStringList result;
    for (const QJsonValue& value : values) {
        result.append(value.toString());
    }
    return result;
}

QJsonObject catalogueAssetToJson(const ProjectAsset& asset)
{
    return {
        { "id", asset.id }, { "projectId", asset.projectId }, { "name", asset.name },
        { "kind", asset.kind }, { "origin", asset.origin }, { "filePath", asset.filePath },
        { "sourceAssetIds", stringListToJson(asset.sourceAssetIds) },
        { "durationSeconds", asset.durationSeconds }, { "sampleRate", asset.sampleRate },
        { "channels", asset.channels }, { "createdAt", asset.createdAt }, { "updatedAt", asset.updatedAt },
        { "provenanceId", asset.provenanceId }, { "contentChecksum", asset.contentChecksum },
        { "tags", stringListToJson(asset.tags) }, { "favourite", asset.favourite },
        { "folder", asset.folder }, { "status", asset.status }
    };
}

ProjectAsset catalogueAssetFromJson(const QJsonObject& value)
{
    ProjectAsset asset;
    asset.id = value.value("id").toString();
    asset.projectId = value.value("projectId").toString();
    asset.name = value.value("name").toString();
    asset.kind = value.value("kind").toString();
    asset.origin = value.value("origin").toString();
    asset.filePath = value.value("filePath").toString();
    asset.sourceAssetIds = stringListFromJson(value.value("sourceAssetIds").toArray());
    asset.durationSeconds = value.value("durationSeconds").toDouble();
    asset.sampleRate = value.value("sampleRate").toInt();
    asset.channels = value.value("channels").toInt();
    asset.createdAt = value.value("createdAt").toString();
    asset.updatedAt = value.value("updatedAt").toString();
    asset.provenanceId = value.value("provenanceId").toString();
    asset.contentChecksum = value.value("contentChecksum").toString();
    asset.tags = stringListFromJson(value.value("tags").toArray());
    asset.favourite = value.value("favourite").toBool();
    asset.folder = value.value("folder").toString();
    asset.status = value.value("status").toString("available");
    return asset;
}

bool readCatalogue(QJsonObject* root, QString* errorMessage)
{
    QFile input(GlobalAssetCatalogue::cataloguePath());
    if (!input.exists()) {
        *root = {};
        return true;
    }
    if (!input.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI Library catalogue");
        }
        return false;
    }
    const QJsonDocument document = QJsonDocument::fromJson(input.readAll());
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI Library catalogue is invalid");
        }
        return false;
    }
    *root = document.object();
    return true;
}

bool saveCatalogue(const QJsonObject& root, QString* errorMessage)
{
    const QFileInfo outputInfo(GlobalAssetCatalogue::cataloguePath());
    if (!QDir().mkpath(outputInfo.absolutePath())) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not create the AI Library catalogue folder");
        }
        return false;
    }
    QSaveFile output(outputInfo.filePath());
    if (!output.open(QIODevice::WriteOnly)
        || output.write(QJsonDocument(root).toJson(QJsonDocument::Compact)) < 1
        || !output.commit()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not save the AI Library catalogue");
        }
        return false;
    }
    return true;
}
}

QString GlobalAssetCatalogue::cataloguePath()
{
    const QString testPath = qEnvironmentVariable("AUDACITY_AI_LIBRARY_CATALOGUE_PATH");
    if (!testPath.isEmpty()) {
        return testPath;
    }
    return QDir(QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation)).filePath(CATALOGUE_FILE);
}

bool GlobalAssetCatalogue::syncProject(const QString& projectPath, const QString& workspacePath,
                                       const QList<ProjectAsset>& assets, QString* errorMessage)
{
    if (projectPath.isEmpty() || workspacePath.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("A saved project is required to index its AI Library");
        }
        return false;
    }
    QJsonObject root;
    if (!readCatalogue(&root, errorMessage)) {
        return false;
    }
    QJsonArray records = root.value("assets").toArray();
    QJsonArray updated;
    for (const QJsonValue& value : records) {
        if (value.toObject().value("projectPath").toString() != projectPath) {
            updated.append(value);
        }
    }
    for (const ProjectAsset& asset : assets) {
        updated.append(QJsonObject {
            { "projectPath", projectPath }, { "workspacePath", workspacePath }, { "asset", catalogueAssetToJson(asset) }
        });
    }
    root.insert("version", 1);
    root.insert("assets", updated);
    return saveCatalogue(root, errorMessage);
}

QList<GlobalAssetRecord> GlobalAssetCatalogue::allAssets(QString* errorMessage)
{
    QJsonObject root;
    if (!readCatalogue(&root, errorMessage)) {
        return {};
    }
    QList<GlobalAssetRecord> result;
    for (const QJsonValue& value : root.value("assets").toArray()) {
        const QJsonObject record = value.toObject();
        GlobalAssetRecord item;
        item.projectPath = record.value("projectPath").toString();
        item.workspacePath = record.value("workspacePath").toString();
        item.asset = catalogueAssetFromJson(record.value("asset").toObject());
        if (!item.projectPath.isEmpty() && !item.asset.id.isEmpty()) {
            result.append(item);
        }
    }
    return result;
}
