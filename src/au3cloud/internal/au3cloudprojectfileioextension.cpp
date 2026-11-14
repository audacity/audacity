/*
* Audacity: A Digital Audio Editor
*/

#include "au3cloudprojectfileioextension.h"

#include "au3-cloud-audiocom/sync/ProjectCloudExtension.h"

using namespace au::au3cloud;

OnOpenAction CloudProjectFileIOExtension::OnOpen(AudacityProject&, const std::string&)
{
    return OnOpenAction::Continue;
}

void CloudProjectFileIOExtension::OnLoad(AudacityProject& project)
{
    auto& projectCloudExtension
        = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(project);

    projectCloudExtension.OnLoad();
}

OnSaveAction CloudProjectFileIOExtension::OnSave(AudacityProject&, const ProjectSaveCallback&)
{
    return OnSaveAction::Continue;
}

OnCloseAction CloudProjectFileIOExtension::OnClose(AudacityProject&)
{
    return OnCloseAction::Continue;
}

void CloudProjectFileIOExtension::OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer)
{
    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(project);
    if (!projectCloudExtension.IsCloudProject()) {
        return;
    }

    projectCloudExtension.OnUpdateSaved(serializer);
}

bool CloudProjectFileIOExtension::IsBlockLocked(const AudacityProject& project, int64_t blockId) const
{
    return audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(project).IsBlockLocked(blockId);
}

CloudProjectFileIOExtension& GetExtension()
{
    static CloudProjectFileIOExtension extension;
    return extension;
}

ProjectFileIOExtensionRegistry::Extension extension { GetExtension() };
