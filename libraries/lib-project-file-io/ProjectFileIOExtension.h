/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIOExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <string>

class AudacityProject;
class ProjectSerializer;

using ProjectSaveCallback = std::function<bool(const std::string& path, bool nameChanged)>;

class PROJECT_FILE_IO_API ProjectFileIOExtension /* not final */
{
public:
   virtual ~ProjectFileIOExtension();

   virtual bool OnOpen(AudacityProject& project, const std::string& path) = 0;
   virtual void OnLoad(AudacityProject& project) = 0;
   virtual bool OnSave(AudacityProject& project, const ProjectSaveCallback& projectSaveCallback) = 0;
   virtual bool OnClose(AudacityProject& project) = 0;
   virtual void OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer) = 0;
   virtual bool IsBlockLocked(const AudacityProject& project, int64_t blockId) const = 0;
};

struct PROJECT_FILE_IO_API ProjectFileIOExtensionRegistry final
{
   struct PROJECT_FILE_IO_API Extension final
   {
      explicit Extension(ProjectFileIOExtension& extension);
   };

   static bool OnOpen(AudacityProject& project, const std::string& path);
   static void OnLoad(AudacityProject& project);
   static bool OnSave(AudacityProject& project, const ProjectSaveCallback& projectSaveCallback);
   static bool OnClose(AudacityProject& project);
   static void OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer);
   static bool IsBlockLocked(const AudacityProject& project, int64_t blockId);
};
