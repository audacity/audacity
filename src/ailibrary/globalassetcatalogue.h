/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "projectasset.h"

#include <QList>
#include <QString>

namespace au::ailibrary {

// A disposable application-level index. Project manifests remain the source
// of truth, so this catalogue can safely be rebuilt when it is unavailable.
struct GlobalAssetRecord {
    QString projectPath;
    QString workspacePath;
    ProjectAsset asset;
};

class GlobalAssetCatalogue final
{
public:
    static bool syncProject(const QString& projectPath, const QString& workspacePath,
                            const QList<ProjectAsset>& assets, QString* errorMessage = nullptr);
    static QList<GlobalAssetRecord> allAssets(QString* errorMessage = nullptr);

    // Exposed for focused tests and diagnostics. Production callers use the
    // AppLocalDataLocation-backed default.
    static QString cataloguePath();
};

}
