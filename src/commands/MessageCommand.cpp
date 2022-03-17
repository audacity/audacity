/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.cpp
\brief Definitions for MessageCommand class

*//*******************************************************************/


#include "MessageCommand.h"

#include "LoadCommands.h"
#include "CommandContext.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"

const ComponentInterfaceSymbol MessageCommand::Symbol
{ XO("Message") };

namespace{ BuiltinCommandsModule::Registration< MessageCommand > reg; }

template<bool Const>
bool MessageCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( mMessage, wxT("Text"), wxString{"Some message"} );
   return true;
}

bool MessageCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool MessageCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void MessageCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Text:"),mMessage,60);
   }
   S.EndMultiColumn();
}

bool MessageCommand::Apply(const CommandContext & context){
   context.Status( mMessage );
   return true;
}
