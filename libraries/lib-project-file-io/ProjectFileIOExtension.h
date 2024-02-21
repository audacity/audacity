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

using ProjectSaveCallback =
   std::function<bool(const std::string& path, bool nameChanged)>;

enum class OnSaveAction
{
   Handled,
   Cancelled,
   Continue
};

enum class OnCloseAction
{
   Veto,
   Continue
};

enum class OnOpenAction
{
   Cancel,
   Continue
};

class PROJECT_FILE_IO_API ProjectFileIOExtension /* not final */
{
public:
   virtual ~ProjectFileIOExtension();

   virtual OnOpenAction
   OnOpen(AudacityProject& project, const std::string& path) = 0;
   virtual void OnLoad(AudacityProject& project)             = 0;
   virtual OnSaveAction OnSave(
      AudacityProject& project,
      const ProjectSaveCallback& projectSaveCallback)      = 0;
   virtual OnCloseAction OnClose(AudacityProject& project) = 0;
   virtual void OnUpdateSaved(
      AudacityProject& project, const ProjectSerializer& serializer) = 0;
   virtual bool
   IsBlockLocked(const AudacityProject& project, int64_t blockId) const = 0;
};

struct PROJECT_FILE_IO_API ProjectFileIOExtensionRegistry final
{
   struct PROJECT_FILE_IO_API Extension final
   {
      explicit Extension(ProjectFileIOExtension& extension);
   };

   static OnOpenAction
   OnOpen(AudacityProject& project, const std::string& path);
   static void OnLoad(AudacityProject& project);
   static OnSaveAction OnSave(
      AudacityProject& project, const ProjectSaveCallback& projectSaveCallback);
   static OnCloseAction OnClose(AudacityProject& project);
   static void
   OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer);
   static bool IsBlockLocked(const AudacityProject& project, int64_t blockId);
};
