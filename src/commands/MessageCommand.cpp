/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.cpp
\brief Definitions for MessageCommand class

*//*******************************************************************/

#include "../Audacity.h"
#include "MessageCommand.h"

#include "CommandType.h"
#include "CommandContext.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"

bool MessageCommand::DefineParams( ShuttleParams & S ){
   S.Define( mMessage, wxT("Text"),  "Some message" );
   return true;
}

void MessageCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Text:"),mMessage,60);
   }
   S.EndMultiColumn();
}

bool MessageCommand::Apply(const CommandContext & context){
   context.Status( mMessage );
   return true;
}
