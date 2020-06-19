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

namespace
{
   ProjectStatus::StatusWidthFunctions &statusWidthFunctions()
   {
      static ProjectStatus::StatusWidthFunctions theFunctions;
      return theFunctions;
   }
}

ProjectStatus::RegisteredStatusWidthFunction::RegisteredStatusWidthFunction(
   const StatusWidthFunction &function )
{
   statusWidthFunctions().emplace_back( function );
}

auto ProjectStatus::GetStatusWidthFunctions() -> const StatusWidthFunctions &
{
   return statusWidthFunctions();
}

const TranslatableString &ProjectStatus::Get( StatusBarField field ) const
{
   return mLastStatusMessages[ field - 1 ];
}

void ProjectStatus::Set(const TranslatableString &msg, StatusBarField field )
{
   auto &project = mProject;
   auto &lastMessage = mLastStatusMessages[ field - 1 ];
   // compare full translations not msgids!
   if ( msg.Translation() != lastMessage.Translation() ) {
      lastMessage = msg;
      wxCommandEvent evt{ EVT_PROJECT_STATUS_UPDATE };
      evt.SetInt( field );
      project.ProcessEvent( evt );
   }
}
