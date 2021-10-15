/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFormatExtensionRegistry.cpp

  Dmitry Vedenko

**********************************************************************/


#include "ProjectFormatExtensionsRegistry.h"

#include <algorithm>

#include "Project.h"

ProjectFormatExtensionsRegistry& GetProjectFormatExtensionsRegistry()
{
   static ProjectFormatExtensionsRegistry instance;
   return instance;
}

const ProjectFormatExtensionsRegistry& ProjectFormatExtensionsRegistry::Get()
{
   return GetProjectFormatExtensionsRegistry();
}

void ProjectFormatExtensionsRegistry::Register(
   ProjectVersionResolver formatExtension)
{
   mRegisteredExtensions.emplace_back(std::move(formatExtension));
}

ProjectFormatVersion ProjectFormatExtensionsRegistry::GetRequiredVersion(
   const AudacityProject& project) const
{
   ProjectFormatVersion minVersion = BaseProjectFormatVersion;

   for (auto formatExtension : mRegisteredExtensions)
   {
      if (!formatExtension)
         continue;
      
      const auto formatExtensionVersion = formatExtension(project);

      if (minVersion < formatExtensionVersion)
         minVersion = formatExtensionVersion;
   }

   return minVersion;
}

ProjectFormatExtensionsRegistry::Extension::Extension(
   ProjectVersionResolver resolver)
{
   if (resolver)
      GetProjectFormatExtensionsRegistry().Register(std::move(resolver));
}
