/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIOExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

class AudacityProject;
class ProjectFileIO;

class PROJECT_FILE_IO_API ProjectFileIOExtension /* not final */
{
public:
   virtual ~ProjectFileIOExtension();

   virtual void OnLoad(AudacityProject& project) = 0;
   virtual void OnSave(AudacityProject& project) = 0;
   virtual bool OnClose(AudacityProject& project) = 0;
};

struct PROJECT_FILE_IO_API ProjectFileIOExtensionRegistry final
{
   struct PROJECT_FILE_IO_API Extension final
   {
      explicit Extension(ProjectFileIOExtension& extension);
   };

   static void OnLoad(AudacityProject& project);
   static void OnSave(AudacityProject& project);
   static bool OnClose(AudacityProject& project);
};
