/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iimporterconfiguration.h"

namespace au::importexport {
class ImporterConfiguration : public IImporterConfiguration
{
public:
    ImporterConfiguration() = default;

    void init();

    std::vector<std::string> tempoDetectionWorkspaces() const override;
    void setTempoDetectionWorkspaces(const std::vector<std::string>& workspaces) override;
    muse::async::Notification tempoDetectionWorkspacesChanged() const override;

    LoopAction emptyProjectLoopAction() const override;
    void setEmptyProjectLoopAction(LoopAction action) override;

    LoopAction subsequentImportLoopAction() const override;
    void setSubsequentImportLoopAction(LoopAction action) override;

private:
    muse::async::Notification m_tempoDetectionWorkspacesChanged;
};
}
