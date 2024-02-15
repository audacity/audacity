/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIOExtension.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectFileIOExtension.h"

#include <vector>

namespace
{
std::vector<ProjectFileIOExtension*>& GetExtensions()
{
   static std::vector < ProjectFileIOExtension*> extensions;
   return extensions;
}
}

ProjectFileIOExtension::~ProjectFileIOExtension() = default;

ProjectFileIOExtensionRegistry::Extension::Extension(
   ProjectFileIOExtension& extension)
{
   GetExtensions().push_back(&extension);
}

bool ProjectFileIOExtensionRegistry::OnOpen(
   AudacityProject& project, const std::string& path)
{
   for (auto& extension : GetExtensions())
      if (!extension->OnOpen(project, path))
         return false;

   return true;
}

void ProjectFileIOExtensionRegistry::OnLoad(AudacityProject& project)
{
   for (auto& extension : GetExtensions())
      extension->OnLoad(project);
}

bool ProjectFileIOExtensionRegistry::OnSave(
   AudacityProject& project, const ProjectSaveCallback& projectSaveCallback)
{
   for (auto& extension : GetExtensions())
   {
      if (extension->OnSave(project, projectSaveCallback))
         return true;
   }

   return false;
}

bool ProjectFileIOExtensionRegistry::OnClose(AudacityProject& project)
{
   for (auto& extension : GetExtensions())
   {
      if (!extension->OnClose(project))
         return false;
   }

   return true;
}

void ProjectFileIOExtensionRegistry::OnUpdateSaved(
   AudacityProject& project, const ProjectSerializer& serializer)
{
   for (auto& extension : GetExtensions())
      extension->OnUpdateSaved(project, serializer);
}

bool ProjectFileIOExtensionRegistry::IsBlockLocked(
   const AudacityProject& project, int64_t blockId)
{
   for (auto& extension : GetExtensions())
   {
      if (extension->IsBlockLocked(project, blockId))
         return true;
   }

   return false;
}
