/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFormatExtensionsRegistry.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <mutex>
#include <functional>

#include "ClientData.h"
#include "ProjectFormatVersion.h"

class AudacityProject;

//! The class that allows evaluating the minimum version of Audacity required to support the project
class PROJECT_API ProjectFormatExtensionsRegistry final
   : public ClientData::Base
{
public:
   using ProjectVersionResolver = ProjectFormatVersion (*)(const AudacityProject&);

   static const ProjectFormatExtensionsRegistry& Get();

   //! Returns the minimum possible version that can be used to save the project
   ProjectFormatVersion GetRequiredVersion(const AudacityProject& project) const;

   //! A struct to register the project extension that requires a different project version
   struct PROJECT_API Extension final
   {
      Extension(ProjectVersionResolver resolver);
   };

private:
   void Register(ProjectVersionResolver resolver);

   std::vector<ProjectVersionResolver> mRegisteredExtensions;
};
