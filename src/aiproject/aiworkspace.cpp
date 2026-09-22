/*
 * Audacity: A Digital Audio Editor
 */
#include "aiworkspace.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QObject>
#include <QSaveFile>

using namespace au::aiproject;

QString WorkspaceStore::workspacePathForProject(const QString& projectPath)
{
    return QDir(QFileInfo(projectPath).absolutePath()).filePath(AI_WORKSPACE_DIRECTORY);
}

bool WorkspaceStore::create(const QString& projectPath, QString* errorMessage)
{
    if (projectPath.isEmpty()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Save the project before enabling its AI workspace");
        }
        return false;
    }

    const QString workspacePath = workspacePathForProject(projectPath);
    if (!QDir().mkpath(QDir(workspacePath).filePath("jobs"))) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not create the AI workspace");
        }
        return false;
    }

    const QString manifestPath = QDir(workspacePath).filePath(AI_MANIFEST_FILE);
    if (QFileInfo::exists(manifestPath)) {
        return true;
    }

    QFile manifest(manifestPath);
    const QJsonObject document {
        { "schemaVersion", 1 },
        { "projectFile", QFileInfo(projectPath).fileName() },
        { "jobs", QJsonArray() }
    };
    if (!manifest.open(QIODevice::WriteOnly | QIODevice::NewOnly)
        || manifest.write(QJsonDocument(document).toJson(QJsonDocument::Indented)) < 1) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not write the AI workspace manifest");
        }
        return false;
    }
    return true;
}

bool WorkspaceStore::isEnabled(const QString& projectPath)
{
    return !projectPath.isEmpty()
           && QFileInfo::exists(QDir(workspacePathForProject(projectPath)).filePath(AI_MANIFEST_FILE));
}

bool WorkspaceStore::recordCompletedJob(const QString& workspacePath, const QString& jobId,
                                        const QString& providerId, const QString& resultManifest,
                                        QString* errorMessage)
{
    return recordJobState(workspacePath, jobId, providerId, "complete", resultManifest, errorMessage);
}

bool WorkspaceStore::recordJobState(const QString& workspacePath, const QString& jobId,
                                    const QString& providerId, const QString& state,
                                    const QString& resultManifest, QString* errorMessage)
{
    const QString manifestPath = QDir(workspacePath).filePath(AI_MANIFEST_FILE);
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
    QJsonArray jobs = root.value("jobs").toArray();
    QJsonObject job {
        { "jobId", jobId },
        { "providerId", providerId },
        { "state", state }
    };
    if (!resultManifest.isEmpty()) {
        job.insert("resultManifest", resultManifest);
    }
    bool replaced = false;
    for (int i = 0; i < jobs.size(); ++i) {
        if (jobs[i].toObject().value("jobId").toString() == jobId) {
            jobs[i] = job;
            replaced = true;
            break;
        }
    }
    if (!replaced) {
        jobs.append(job);
    }
    root.insert("jobs", jobs);

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

int WorkspaceStore::recoverInterruptedJobs(const QString& workspacePath, QString* errorMessage)
{
    const QString manifestPath = QDir(workspacePath).filePath(AI_MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return -1;
    }
    const QByteArray manifestData = manifest.readAll();
    manifest.close();
    QJsonDocument document = QJsonDocument::fromJson(manifestData);
    if (!document.isObject()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The AI workspace manifest is invalid");
        }
        return -1;
    }
    QJsonObject root = document.object();
    QJsonArray jobs = root.value("jobs").toArray();
    int recovered = 0;
    for (int i = 0; i < jobs.size(); ++i) {
        QJsonObject job = jobs[i].toObject();
        if (job.value("state").toString() == "running") {
            job.insert("state", "interrupted");
            jobs[i] = job;
            ++recovered;
        }
    }
    if (recovered == 0) {
        return 0;
    }
    root.insert("jobs", jobs);
    QSaveFile output(manifestPath);
    if (!output.open(QIODevice::WriteOnly)
        || output.write(QJsonDocument(root).toJson(QJsonDocument::Indented)) < 1
        || !output.commit()) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not save the AI workspace manifest");
        }
        return -1;
    }
    return recovered;
}

QString WorkspaceStore::completedJobAssetPath(const QString& workspacePath, const QString& jobId,
                                              QString* errorMessage)
{
    const QString manifestPath = QDir(workspacePath).filePath(AI_MANIFEST_FILE);
    QFile manifest(manifestPath);
    if (!manifest.open(QIODevice::ReadOnly)) {
        if (errorMessage) {
            *errorMessage = QObject::tr("Could not read the AI workspace manifest");
        }
        return {};
    }
    const QJsonObject root = QJsonDocument::fromJson(manifest.readAll()).object();
    for (const QJsonValue& entry : root.value("jobs").toArray()) {
        const QJsonObject job = entry.toObject();
        if (job.value("jobId").toString() != jobId || job.value("state").toString() != "complete") {
            continue;
        }
        if (job.value("insertedIntoProject").toBool()) {
            if (errorMessage) {
                *errorMessage = QObject::tr("This completed test-provider output is already inserted into the project");
            }
            return {};
        }
        const QString resultPath = QDir(workspacePath).filePath(job.value("resultManifest").toString());
        QFile result(resultPath);
        if (!result.open(QIODevice::ReadOnly)) {
            break;
        }
        const QString asset = QJsonDocument::fromJson(result.readAll()).object().value("asset").toString();
        const QString assetPath = QDir(QFileInfo(resultPath).absolutePath()).filePath(asset);
        if (QFileInfo::exists(assetPath)) {
            return assetPath;
        }
        break;
    }
    if (errorMessage) {
        *errorMessage = QObject::tr("No completed test-provider output is available to insert");
    }
    return {};
}

bool WorkspaceStore::markJobInserted(const QString& workspacePath, const QString& jobId, QString* errorMessage)
{
    const QString manifestPath = QDir(workspacePath).filePath(AI_MANIFEST_FILE);
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
    QJsonArray jobs = root.value("jobs").toArray();
    bool found = false;
    for (int i = 0; i < jobs.size(); ++i) {
        QJsonObject job = jobs[i].toObject();
        if (job.value("jobId").toString() == jobId) {
            job.insert("insertedIntoProject", true);
            jobs[i] = job;
            found = true;
            break;
        }
    }
    if (!found) {
        if (errorMessage) {
            *errorMessage = QObject::tr("The completed job is missing from the AI workspace manifest");
        }
        return false;
    }
    root.insert("jobs", jobs);
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
