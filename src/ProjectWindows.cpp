/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file ProjectWindows.cpp

  Paul Licameli split from Project.cpp

**********************************************************************/

#include "ProjectWindows.h"
#include "Project.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include <wx/frame.h>

namespace {
struct ProjectWindows final : ClientData::Base
{
   static ProjectWindows &Get( AudacityProject &project );
   static const ProjectWindows &Get( const AudacityProject &project );
   explicit ProjectWindows(AudacityProject &project)
      : mAttachedWindows{project}
   {}

   wxWeakRef< wxWindow > mPanel{};
   wxWeakRef< wxFrame > mFrame{};

   AttachedWindows mAttachedWindows;
};

const AudacityProject::AttachedObjects::RegisteredFactory key{
   [](AudacityProject &project) {
      return std::make_unique<ProjectWindows>(project);
   }
};

ProjectWindows &ProjectWindows::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectWindows >( key );
}

const ProjectWindows &ProjectWindows::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}
}

AUDACITY_DLL_API wxWindow &GetProjectPanel( AudacityProject &project )
{
   auto ptr = ProjectWindows::Get(project).mPanel;
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

AUDACITY_DLL_API const wxWindow &GetProjectPanel(
   const AudacityProject &project )
{
   auto ptr = ProjectWindows::Get(project).mPanel;
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

AUDACITY_DLL_API void SetProjectPanel(
   AudacityProject &project, wxWindow &panel )
{
   ProjectWindows::Get(project).mPanel = &panel;
}

AUDACITY_DLL_API wxFrame &GetProjectFrame( AudacityProject &project )
{
   auto ptr = ProjectWindows::Get(project).mFrame;
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

AUDACITY_DLL_API const wxFrame &GetProjectFrame( const AudacityProject &project )
{
   auto ptr = ProjectWindows::Get(project).mFrame;
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

wxFrame *FindProjectFrame( AudacityProject *project ) {
   if (!project)
      return nullptr;
   return ProjectWindows::Get(*project).mFrame;
}

const wxFrame *FindProjectFrame( const AudacityProject *project ) {
   if (!project)
      return nullptr;
   return ProjectWindows::Get(*project).mFrame;
}

std::unique_ptr<const BasicUI::WindowPlacement>
ProjectFramePlacement( AudacityProject *project )
{
   if (!project)
      return std::make_unique<BasicUI::WindowPlacement>();
   return std::make_unique<wxWidgetsWindowPlacement>(
      &GetProjectFrame(*project));
}

void SetProjectFrame(AudacityProject &project, wxFrame &frame )
{
   ProjectWindows::Get(project).mFrame = &frame;
}

AUDACITY_DLL_API AttachedWindows &GetAttachedWindows(AudacityProject &project)
{
   return ProjectWindows::Get(project).mAttachedWindows;
}
