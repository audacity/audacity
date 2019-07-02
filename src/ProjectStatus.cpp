/**********************************************************************

Audacity: A Digital Audio Editor

ProjectStatus.h

Paul Licameli

**********************************************************************/

#include "ProjectStatus.h"

#include "Project.h"

wxDEFINE_EVENT(EVT_PROJECT_STATUS_UPDATE, wxCommandEvent);

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &parent ){
     return std::make_shared< ProjectStatus >( parent );
   }
};

ProjectStatus &ProjectStatus::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectStatus >( key );
}

const ProjectStatus &ProjectStatus::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectStatus::ProjectStatus( AudacityProject &project )
   : mProject{ project }
{
}

ProjectStatus::~ProjectStatus() = default;

void ProjectStatus::Set(const wxString &msg)
{
   auto &project = mProject;
   if ( msg != mLastMainStatusMessage ) {
      mLastMainStatusMessage = msg;
      wxCommandEvent evt{ EVT_PROJECT_STATUS_UPDATE };
      project.ProcessEvent( evt );
   }
}
