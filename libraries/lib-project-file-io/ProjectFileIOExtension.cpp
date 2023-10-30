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

void ProjectFileIOExtensionRegistry::OnLoad(AudacityProject& project)
{
   for (auto& extension : GetExtensions())
      extension->OnLoad(project);
}

void ProjectFileIOExtensionRegistry::OnSave(AudacityProject& project)
{
   for (auto& extension : GetExtensions())
      extension->OnSave(project);
}

bool ProjectFileIOExtensionRegistry::OnClose(AudacityProject& project)
{
   bool ok = true;

   for (auto& extension : GetExtensions())
   {
      if (!extension->OnClose(project))
         ok = false;
   }

   return ok;
}
