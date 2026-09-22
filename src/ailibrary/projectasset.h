/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>
#include <QStringList>

namespace au::ailibrary {

// A Library asset is independent of a timeline clip. File paths are stored
// project-relative whenever the project owns the file.
struct ProjectAsset {
    QString id;
    QString projectId;
    QString name;
    QString kind;
    QString origin;
    QString filePath;
    QStringList sourceAssetIds;
    double durationSeconds = 0.0;
    int sampleRate = 0;
    int channels = 0;
    QString createdAt;
    QString updatedAt;
    QString provenanceId;
    QString contentChecksum;
    QStringList tags;
    bool favourite = false;
    // A project-local Library folder. This is metadata only: managed files
    // retain their stable relative paths so existing timeline imports remain valid.
    QString folder;
    QString status = "available";
};

struct AssetProvenance {
    QString id;
    QString assetId;
    QString operation;
    QString providerId;
    QString providerVersion;
    QString modelId;
    QString modelRevision;
    QStringList adaptationIds;
    QStringList sourceAssetIds;
    QString songPlanId;
    QString lyricsRevisionId;
    QString prompt;
    QString jobId;
    QString createdAt;
    QString outputChecksum;
};

}
