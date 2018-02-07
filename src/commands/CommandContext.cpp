/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2017 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file CommandContext.cpp
\brief Contains definitions for CommandContext

\class CommandContext
\brief CommandContext provides addiitonal information to an
'Apply()' command.  It provides the project, and provides output
channels for Error, Progress and Status.  Status is used for general
messaging from a command back to its invoker.

*//*******************************************************************/

#include "Command.h"
#include <map>
#include <wx/string.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "../AudacityException.h"
#include "Validators.h"
#include "CommandType.h"
#include "CommandMisc.h"
#include "CommandBuilder.h"
#include "CommandTargets.h"
#include "CommandDirectory.h"

//#include "CommandContext.h"
#include "CommandContext.h"
#include "../Project.h"

CommandContext::CommandContext(
      AudacityProject &p
      , const wxEvent *e
      , int ii
      , const CommandParameter &param
   )
      : project{ p }
      // No target specified?  Use the special interactive one that pops up a dialog.
      , pOutput( std::move( std::make_unique<InteractiveOutputTarget>()) )
      , pEvt{ e }
      , index{ ii }
      , parameter{ param }
{
}
   
CommandContext::CommandContext(
      AudacityProject &p,
      std::unique_ptr<CommandOutputTarget> target)
      : project{ p }
      // Revisit and use std_unique pointer for pOutput??
      , pOutput( std::move( target) )
      , pEvt{ nullptr }
      , index{ 0 }
      , parameter{ CommandParameter{}}
{
}

void CommandContext::Status( const wxString & message ) const
{
   if( pOutput )
      pOutput->Status( message );
   else
   {
      wxLogDebug("Status:%s", message );
   }
}

void CommandContext::Error(  const wxString & message ) const
{
   if( pOutput )
      pOutput->Error( message );
   else
   {
      wxLogDebug("Error:%s", message );
   }
}

void CommandContext::Progress( double d ) const
{
   if( pOutput )
      pOutput->Progress( d );
}

AudacityApp * CommandContext::GetApp() const
{  return (AudacityApp *) wxTheApp;}

AudacityProject *CommandContext::GetProject() const
{  return GetActiveProject();}

