/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>

namespace au::aiproject {
inline constexpr const char* AI_WORKSPACE_DIRECTORY = "ai";
inline constexpr const char* AI_MANIFEST_FILE = "manifest.json";

class WorkspaceStore final
{
public:
    static QString workspacePathForProject(const QString& projectPath);
    static bool create(const QString& projectPath, QString* errorMessage = nullptr);
    static bool isEnabled(const QString& projectPath);
    static bool recordCompletedJob(const QString& workspacePath, const QString& jobId,
                                   const QString& providerId, const QString& resultManifest,
                                   QString* errorMessage = nullptr);
    static bool recordJobState(const QString& workspacePath, const QString& jobId,
                               const QString& providerId, const QString& state,
                               const QString& resultManifest = QString(), QString* errorMessage = nullptr);
    static int recoverInterruptedJobs(const QString& workspacePath, QString* errorMessage = nullptr);
    static QString completedJobAssetPath(const QString& workspacePath, const QString& jobId,
                                         QString* errorMessage = nullptr);
    static bool markJobInserted(const QString& workspacePath, const QString& jobId,
                                QString* errorMessage = nullptr);
};
}
