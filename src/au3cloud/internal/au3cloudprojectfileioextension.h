/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/modularity/ioc.h"
#include "au3cloud/iau3audiocomservice.h"

#include "au3-project-file-io/ProjectFileIOExtension.h"

namespace au::au3cloud {
class CloudProjectFileIOExtension final : public ProjectFileIOExtension, public muse::Injectable
{
    muse::Inject<IAu3AudioComService> audioComService { this };

public:
    explicit CloudProjectFileIOExtension(muse::modularity::ContextPtr ctx = nullptr);

    OnOpenAction OnOpen(AudacityProject&, const std::string&) override;
    void OnLoad(AudacityProject&) override;
    OnSaveAction OnSave(AudacityProject&, const ProjectSaveCallback&) override;
    OnCloseAction OnClose(AudacityProject&) override;
    void OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer) override;
    bool IsBlockLocked(const AudacityProject& project, int64_t blockId) const override;
};
}
