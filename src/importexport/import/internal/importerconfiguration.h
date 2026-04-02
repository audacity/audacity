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

    TempoDetectionPref::TempoDetection tempoDetectionPref() const override;
    void setTempoDetectionPref(TempoDetectionPref::TempoDetection pref) override;
    muse::async::Notification tempoDetectionPrefChanged() const override;

    std::vector<std::string> tempoDetectionWorkspaces() const override;
    void setTempoDetectionWorkspaces(const std::vector<std::string>& workspaces) override;
    muse::async::Notification tempoDetectionWorkspacesChanged() const override;

    LoopAction emptyProjectLoopAction() const override;
    void setEmptyProjectLoopAction(LoopAction action) override;

    LoopAction subsequentImportLoopAction() const override;
    void setSubsequentImportLoopAction(LoopAction action) override;

private:
    muse::async::Notification m_tempoDetectionPrefChanged;
    muse::async::Notification m_tempoDetectionWorkspacesChanged;
};
}
