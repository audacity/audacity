/**********************************************************************

  Audacity: A Digital Audio Editor

  Demo.cpp

  James Crook

*******************************************************************//**

\class DemoCommand
\brief An AudacityCommand that does nothing but provide 
parameters.  It is for development purposes.

*//****************************************************************//**

\class DemoDialog
\brief DemoDialog used with DemoCommand

*//*******************************************************************/


#include "Demo.h"
#include "LoadCommands.h"

#include <float.h>

#include <wx/intl.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/valnum.h"
#include "../commands/CommandContext.h"

const ComponentInterfaceSymbol DemoCommand::Symbol
{ XO("Demo") };

//Don't register the demo command.  
//namespace{ BuiltinCommandsModule::Registration< DemoCommand > reg; }

bool DemoCommand::DefineParams( ShuttleParams & S ){
   S.Define( delay, wxT("Delay"), 1.0f, 0.001f,  FLT_MAX, 1.0f );
   S.Define( decay, wxT("Decay"), 0.5f, 0.0f,    FLT_MAX, 1.0f  );
   return true;
}

bool DemoCommand::Apply(const CommandContext & context){
   context.Status( "A Message");
   return true;
}

void DemoCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Delay time (seconds):"),delay);
      S.TieTextBox(XXO("Decay factor:"),decay);
   }
   S.EndMultiColumn();
}



