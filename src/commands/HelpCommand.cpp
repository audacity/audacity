/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file HelpCommand.cpp
\brief Definitions for HelpCommand and HelpCommandType classes

*//*******************************************************************/

#include "../Audacity.h"
#include "HelpCommand.h"

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"
#include "../effects/EffectManager.h"

bool HelpCommand::DefineParams( ShuttleParams & S ){
   S.Define( mCommandName, wxT("Command"),  "Help" );
   return true;
}

void HelpCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Command:"),mCommandName);
   }
   S.EndMultiColumn();
}

bool HelpCommand::Apply(const CommandContext & context){
   EffectManager & em = EffectManager::Get();
   PluginID ID = em.GetEffectByIdentifier( mCommandName );
   if( ID.empty() )
      context.Status( "Command not found" );
   else
      em.GetCommandDefinition( ID, context, 1);
   return true;
}

