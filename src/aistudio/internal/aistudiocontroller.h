/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "actions/actionable.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

#include <QString>
#include <QStringList>

namespace au::aijobs { class RuntimeHostSupervisor; }

namespace au::aistudio {
class AIStudioController final : public muse::actions::Actionable, public muse::Contextable
{
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };

public:
    AIStudioController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void openJobs();
    void enableProjectWorkspace();
    void importLocalWav(const QString& sourcePath);
    void setLibraryAssetFavourite(const QString& assetId, bool favourite);
    void setLibraryAssetsFavourite(const QStringList& assetIds, bool favourite);
    void moveLibraryAssetsToFolder(const QStringList& assetIds, const QString& folder);
    void moveLibraryAssetsToUnfiled(const QStringList& assetIds);
    void createLibraryFolder(const QString& folder);
    void renameLibraryFolder(const QString& folder, const QString& newFolder);
    void deleteLibraryFolder(const QString& folder);
    void renameLibraryAsset(const QString& assetId, const QString& name);
    void deleteLibraryAsset(const QString& assetId);
    void setLibraryAssetTags(const QString& assetId, const QString& tags);
    void setLibraryAssetsTags(const QStringList& assetIds, const QString& tags);
    void readLibraryAssetAudioDetails(const QString& assetId);
    void readLibraryAssetsAudioDetails(const QStringList& assetIds);
    void revealLibraryAssetInExplorer(const QString& assetId);
    void addLibraryAssetToTimeline(const QString& assetId);
    void copyGlobalLibraryAssetToProject(const QString& projectPath, const QString& assetId);
    void refreshWorkspaceStatus();
    void refreshLibraryAssets();
    void recordJobState(const QString& jobId, const QString& state, const QString& resultManifest = QString());
    void recordCompletedJob(const QString& jobId, const QString& resultManifest);
    void insertTestJobOutput();
    std::shared_ptr<au::aijobs::RuntimeHostSupervisor> m_runtimeHost;
    QString m_activeWorkspace;
};
}
