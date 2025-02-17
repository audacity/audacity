/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIOExtension.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectFileIOExtension.h"

#include <vector>

namespace {
std::vector<ProjectFileIOExtension*>& GetExtensions()
{
    static std::vector<ProjectFileIOExtension*> extensions;
    return extensions;
}
} // namespace

ProjectFileIOExtension::~ProjectFileIOExtension() = default;

ProjectFileIOExtensionRegistry::Extension::Extension(
    ProjectFileIOExtension& extension)
{
    GetExtensions().push_back(&extension);
}

OnOpenAction ProjectFileIOExtensionRegistry::OnOpen(
    AudacityProject& project, const std::string& path)
{
    for (auto& extension : GetExtensions()) {
        if (extension->OnOpen(project, path) == OnOpenAction::Cancel) {
            return OnOpenAction::Cancel;
        }
    }

    return OnOpenAction::Continue;
}

void ProjectFileIOExtensionRegistry::OnLoad(AudacityProject& project)
{
    for (auto& extension : GetExtensions()) {
        extension->OnLoad(project);
    }
}

OnSaveAction ProjectFileIOExtensionRegistry::OnSave(
    AudacityProject& project, const ProjectSaveCallback& projectSaveCallback)
{
    for (auto& extension : GetExtensions()) {
        if (auto action = extension->OnSave(project, projectSaveCallback);
            action != OnSaveAction::Continue) {
            return action;
        }
    }

    return OnSaveAction::Continue;
}

OnCloseAction ProjectFileIOExtensionRegistry::OnClose(AudacityProject& project)
{
    for (auto& extension : GetExtensions()) {
        if (extension->OnClose(project) == OnCloseAction::Veto) {
            return OnCloseAction::Veto;
        }
    }

    return OnCloseAction::Continue;
}

void ProjectFileIOExtensionRegistry::OnUpdateSaved(
    AudacityProject& project, const ProjectSerializer& serializer)
{
    for (auto& extension : GetExtensions()) {
        extension->OnUpdateSaved(project, serializer);
    }
}

bool ProjectFileIOExtensionRegistry::IsBlockLocked(
    const AudacityProject& project, int64_t blockId)
{
    for (auto& extension : GetExtensions()) {
        if (extension->IsBlockLocked(project, blockId)) {
            return true;
        }
    }

    return false;
}
