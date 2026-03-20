/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-project-file-io/ProjectFileIOExtension.h"

namespace au::au3cloud {
class CloudProjectFileIOExtension final : public ProjectFileIOExtension
{
public:
    OnOpenAction OnOpen(AudacityProject&, const std::string&) override;
    void OnLoad(AudacityProject&) override;
    OnSaveAction OnSave(AudacityProject&, const ProjectSaveCallback&) override;
    OnCloseAction OnClose(AudacityProject&) override;
    void OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer) override;
    bool IsBlockLocked(const AudacityProject& project, int64_t blockId) const override;
};
}
